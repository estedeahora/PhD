# Educational

# TODO Usar data.table para optimizar  preparación de bases
# TODO Revisar si N_MATRIC es correcto en CARTO$ESCUELAS. Diferenciar primario
#       y secundario de forma explícita

cli::cli_h2(col_blue("Análisis Educativo (EDU)"))

EDU <- list()

# cli_h2(col_blue("Modelo LP"))
# cli_alert_warning("Basado en script Tesis MGAIE: 'script/MGAIE-ANEXO Modelos lp.R'.")

# Cargar datos --------------------------------------

  # Requiere:
  #   ✓ CARTO$CABA2       -> Es CARTO$RADIO
  #   ✓ ESCUELA           -> Es CARTO$ESCUELA
  #   AUX$RADIO
  #   PRI y SEC con análisis de segmentación (Cáp 4)

  # load("_Anexo/MGAIE Base lp.RData")

  if(!exists("CARTO", inherits = F)){
    f <- fs::path("data-workflow", "final-data", "CARTO", ext = "rda")
    cli_alert_info(
      col_silver("Cargando cartografía (`CARTO`): {.path {fs::path_rel(f)}}")
    )

    load(file = f)
    rm(f)
  }

  if(!exists("CALLE", inherits = F)){
    f <- fs::path("data-workflow", "final-data", "CALLE", ext = "rda")
    cli_alert_info(
      col_silver("Cargando callejero (`CALLE`): {.path {fs::path_rel(f)}}")
    )

    load(file = f)
    rm(f)
  }

# Preparar datos -------------------------------------

### ╠ Identificación de nodos para cada RADIO/ESCUELA (from/to) --------

  RAD <- CARTO$RADIO |>
    filter(!is.na(TH)) |>
    select(ID = ID_RADIO) |>
    arrange(ID) |>
    st_point_on_surface()

  ESC <- CARTO$ESCUELA  |>
      filter(NIV_SEC & !is.na(N_MATRIC))  |>
      select(CUEANEXO)  |>
      arrange(CUEANEXO)
{
  tictoc::tic("")
  # ~ 8 minutos

  cat("Creando nodos cercanos a radios")

  blended <- st_network_blend(CALLE, RAD)

  cat("Creando nodos ceranos a escuelas")

  blended <- st_network_blend(blended, ESC)

  cat("Tiempo de ejecución")
  tictoc::toc()
}

  blended <- blended %>%
    activate(edges) %>%
    mutate(weight = st_length(geometry))

  nodes <- blended %>%
    activate(nodes) %>%
    as_tibble() %>%
    st_as_sf()

  # Seleccionar nodos mas cercanos a...
  # ... from: RADIOS

  RAD <- RAD |>
    mutate(# ID del nodo más próximo al centroide de cada radio
           from = st_nearest_feature(geometry, nodes),
           # Distancia al nodo más próximo
           RAD_min = st_distance(geometry, nodes[from, ], by_element = TRUE),
           from = as.character(from) )

  # ... to: ESCUELAS
  ESC <- ESC %>%
    mutate(# ID del nodo más próximo a cada escuela
           to = st_nearest_feature(geometry, nodes),
           # Distancia al nodo más próximo
           ESC_min = st_distance(geometry, nodes[to, ], by_element = TRUE),
           to = as.character(to) )

  # ID de nodos más cercanos a cada radio (from) / escuelas (to)
  from <- unique(RAD$from)
  to <- unique(ESC$to)

  rm(nodes)

### ╠ Matriz de distancias/costos --------------------------------------
  # Matrices de 3273 x 13995 = 43.841.835 elementos

  # Matriz de distancia lineal (DIS lineal)
  # ~10 segs
  DIS_l <- st_distance(RAD, ESC) %>%
    round() %>%
    units::set_units("km") %>%
    units::drop_units() %>%
    as.data.frame()

  rownames(DIS_l) <- RAD$ID
  colnames(DIS_l) <- ESC$CUEANEXO

  DIS_l <- DIS_l %>%
    as_tibble(rownames = "ID") %>%
    pivot_longer(cols = -ID, names_to = "CUEANEXO",
                 values_to = "DIS_l")

  # Distancias entre nodos por trama (DIS net)
{
  tictoc::tic("Calculando distancias por red urbana")
  # ~ 26mins
  DIS_n <- igraph::distances(
    graph = blended,
    v = from,
    to = to,
    weights = blended %>% activate(edges) %>% pull(weight)
  )
  tictoc::toc()
}

  rownames(DIS_n) <- from
  colnames(DIS_n) <- to

  DIS_n <- DIS_n |>
    as_tibble(rownames = "from") |>
    pivot_longer(cols = -from, names_to = "to", values_to = "DIS_n")

  DIS_n <- RAD |>
    st_drop_geometry() |>
    left_join(DIS_n, by = "from")

  DIS_n <- ESC |>
    st_drop_geometry() |>
    left_join(DIS_n, by = "to", relationship = "many-to-many")

  DIS_n <- DIS_n |>
    mutate(across(.cols = c(RAD_min, ESC_min), drop_units),
           across(.cols = c(DIS_n, RAD_min, ESC_min),
                  .fns = ~round(.x ) ),
           # Distancia total (sumando camino lineal hasta nodo)
           DIS_t = DIS_n + RAD_min + ESC_min,
           # Transformar distancias: mts -> km
           across(.cols = c(DIS_n, DIS_t),
                  .fns = ~.x/1000) ) |>
    select(ID, CUEANEXO, DIS_n, DIS_t)

  # Base de distancias lineales + por red urbana (matriz compacta)
  # ex DIS2
  db_DIS <- full_join(DIS_l, DIS_n,
                    by = c("ID", "CUEANEXO") ) |>
    arrange(ID, CUEANEXO)

  save(db_DIS, file = fs::path("_draft", "LP_01_distancias", ext = "RData"))
  # load(fs::path("_draft", "LP_01_distancias", ext = "RData"))

  # Matriz de distancias
  # ex DIS3
  DIS <- db_DIS |>
    pivot_wider(id_cols = ID, names_from = CUEANEXO, values_from = DIS_t) |>
    data.frame(check.names = F, row.names = 1)

  rm(DIS_l, DIS_n, ESC, RAD, from, to)

### ╚ Preparación de bases marginales y distancia ----------------------------------

  # Bases con marginales

  margDEM <- CARTO$RADIO |>
    st_drop_geometry() |>
    filter(!is.na(TH) ) |>
    select(ID = ID_RADIO, TIPO_HAB = TH, SEC = n_estSEC) |>
    arrange(ID) |>
    mutate(SEC = replace_na(SEC, 0))

  margOFE <- CARTO$ESCUELAS |>
    filter(NIV_SEC & !is.na(N_MATRIC)) |>
    select(CUEANEXO, n = N_MATRIC, SECTOR)

# Análisis -----------------------------------------

### ╠ Calculo de lp  ----------------------------------------------------------

  res <- list()

  # Valores marginales de constriccion
  cost.mat <- as.matrix(DIS[colnames(DIS) %in% margOFE$CUEANEXO])/10
  # cost.mat <- as.matrix(DIS)/10
  row.rhs <- margDEM$SEC
  col.rhs <- margOFE$n

  # Signo de la desigualdad de filas y columnas
  if(sum(margDEM$SEC) <= sum(margOFE$n)){
    row.signs <- rep ("=", nrow(cost.mat))
    col.signs <- rep ("<=", ncol(cost.mat))
  }else{
    row.signs <- rep ("<=", nrow(cost.mat))
    col.signs <- rep ("=", ncol(cost.mat))
  }

{
  # Resolución
  tictoc::tic("Tiempo de ejecución")
  cat("Resolviendo modelo lp. ")
  res <- lp.transport(cost.mat, "min", presolve = 1,
                           row.signs, row.rhs, col.signs, col.rhs)
  tictoc::toc()
}
  rownames(res$solution) <- rownames(cost.mat)
  colnames(res$solution) <- colnames(cost.mat)
  res$objval <- res$objval * 10

  rm(row.rhs, col.rhs, cost.mat, row.signs, col.signs)

  save(res, file = fs::path("_draft", "LP_02_res-LP", ext = "RData"))
  # load(file = fs::path("_draft", "LP_02_res-LP", ext = "RData"))

# Figuras ---------------------------------------

### ╠ FIG 7.3: Distribución de escuelas -------------------------------


### ╚ FIG App ?.?: Comparación entre distancias lineales y por red vial --------------------

  p8 <- db_DIS %>%
    ggplot() +
    geom_point(aes(x = DIS_l, y = DIS_t),
               alpha = 0.1, color = "cornflowerblue") +
    geom_abline(intercept = 0, slope = 1,
                linetype = 2, color = "tomato3") +
    theme_minimal() +
    labs(x = "Distancia lineal (km)",
         y = "Distancia por red urbana (km)")

  # ggsave("_Figuras/5-Fig1-Distancia-Lineal-Red.png",
  #          width = 10, height = 10, units = "cm")

# Tablas ---------------------------

margDEM$PRI_distMIN <- apply(DIS[["PRI"]], 1, min) * 1000
margDEM$SEC_distMIN <- apply(DIS[["SEC"]], 1, min) * 1000

for(i in names(res)){
  lp_res <- res[[i]]$solution
  if(str_starts(i, "PRI")){
    a <- "PRI"
  }else{
    a <- "SEC"
  }
  margDEM[paste0(i, "_distPROM")] <- apply( lp_res * DIS[[a]] * 1000, 1, sum) /
    margDEM[[a]]
  margDEM[paste0(i, "_SINVAC")] <- apply(lp_res, 1, sum) - margDEM[[a]]
  cat(paste0("\n", i,"\n" ) )

  if(sum(margDEM$PRI_SINVACANTE) == 0){
    cat("Todos los alumnos tienen vacante")
    margDEM[paste0(i, "_SINVAC")] <- NULL
  }else{
    cat(paste0("Alumnos sin vacante: ", sum(margDEM$PRI_SINVACANTE)) )
  }
}
rm(a, i, lp_res)

### ╠ TAB1 -----------------------------
  # Distancia promedio recorrida
  tab1 <- margDEM %>%
    group_by(TIPO_HAB) %>%
    summarise(n_PRI = sum(PRI),
              DIST_PRImin = round(mean(PRI_distMIN, na.rm = T)),
              DIST_PRI = round(mean(PRI_distPROM, na.rm = T)),
              DIST_PRIcl = round(mean(PRIcl_distPROM, na.rm = T)),
              DIST_PRIcls = round(mean(PRIcl_s_distPROM, na.rm = T)),
              n_SEC = sum(SEC),
              DIST_SECmin = round(mean(SEC_distMIN, na.rm = T)),
              DIST_SEC = round(mean(SEC_distPROM, na.rm = T)),
              DIST_SECcl = round(mean(SECcl_distPROM, na.rm = T)),
              DIST_SECcls = round(mean(SECcl_s_distPROM, na.rm = T)))

  tab1 <- tab1 %>% rename(PRI_TOTAL = n_PRI, SEC_TOTAL = n_SEC )

  tab1 <- rbind(tab1[!names(tab1) %in% c("PRI_TOTAL", "SEC_TOTAL")],
                summarise(tab1,
                          TIPO_HAB = "Total CABA",
                          across(.cols = contains("_PRI"),
                                 .fns = ~weighted.mean(.x, PRI_TOTAL)),
                          across(.cols = contains("_SEC"),
                                 .fns = ~weighted.mean(.x, SEC_TOTAL)) ) )

# Tab2: Distribución de alumnos en circuitos por radio
# Tab3: Distribución de alumnos en sectores por radio
# Tab4: Distribución de alumnos en circuitos:sectores por radio

# base <- list(PRI = PRI, SEC = SEC)
# tab2 <- list()
# tab3 <- list()
# tab4 <- list()
tab5 <- list()

# for(i in names(res)){
#   if(str_starts(i, "PRI")){
#     a <- "PRI"
#   }else{
#     a <- "SEC"
#   }
  # lp_res <- res[[i]]$solution
  lp_res <- res$solution
  # rownames(lp_res) <- rownames(DIS[[a]])
  # colnames(lp_res) <- colnames(DIS[[a]])

  lp_res <- lp_res %>%
    as_tibble(rownames = "ID") %>%
    pivot_longer(cols = !ID, names_to = "CUEANEXO",
                 values_to = "n") %>%
    filter(n != 0) %>%
    left_join(margOFE[c("CUEANEXO", "SECTOR")],
              by = "CUEANEXO" ) %>%
    left_join(margDEM[c("ID", "TIPO_HAB")], by = "ID" )

#   tab2[[i]] <- xtabs(n ~ TIPO_HAB + cl, data = lp_res)
#   tab2[[i]] <- rbind(round(prop.table(tab2[[i]], 1) * 100 ),
#                      OCUPADO = apply(tab2[[i]], 2, sum),
#                      VACANTES = xtabs(n~cl, data = margOFE[[a]]))
#   tab3[[i]] <- round(prop.table(xtabs(n ~ TIPO_HAB + SECTOR,
#                                       data = lp_res), 1) * 100 )
  lp_res <- lp_res %>% mutate(cl_SEC = paste(cl, SECTOR, sep = ":"),
                              cl_SEC = factor(cl_SEC,
                                              levels = paste(rep(1:4, each = 2),
                                                             c("Estatal", "Privado"), sep = ":")) )
  # tab4[[i]] <- round(prop.table(xtabs(n ~ TIPO_HAB + cl_SEC,
  #                                     data = lp_res), 1) * 100 )
  # tab5[[i]] <- lp_res %>%
  #   group_by(TIPO_HAB) %>%
  #   summarise(across(.cols = Clus.1:Clus.4,
  #                    .fns = ~weighted.mean(.x, n)*100) )

  rm(lp_res)
# }
tab3 <- lapply(tab3, as.data.frame.matrix)
tab3 <- data.frame(TH = rownames(tab3$PRI),
                   PRI_ASI = tab3$PRI$Estatal,
                   PRI_CL = tab3$PRIcl$Estatal,
                   PRI_CL_S =  tab3$PRIcl_s$Estatal,
                   SEC_ASI = tab3$SEC$Estatal,
                   SEC_CL = tab3$SECcl$Estatal,
                   SEC_CL_S =  tab3$SECcl_s$Estatal)
rm(i, a, base)

### ╚ TAB6 ---------------------
  # Comparación de distancias lineales y por red urbana

  tab6a <- DIS2 %>%
    mutate(DIF = DIS_t - DIS_l) %>%
    left_join(AUX$RADIO[c("ID", "TIPO_HAB2")], by = "ID") %>%
    group_by(TIPO_HAB2) %>%
    summarise(DIF = mean(DIF))

  tab6b <- DIS2 %>%
    mutate(DIF = DIS_t - DIS_l,
           DIS_c = cut(DIS_l, breaks = c(0, 0.75, 2, 20),
                       labels = c("Menos de 750m", "Entre 750m y 2km",
                                  "Más de 2km") ) ) %>%
    left_join(AUX$RADIO[c("ID", "TIPO_HAB2")], by = "ID") %>%
    group_by(TIPO_HAB2, DIS_c, .drop = T) %>%
    summarise(DIF = mean(DIF)) %>%
    filter(!is.na(DIS_c) ) %>%
    pivot_wider(id_cols = TIPO_HAB2, names_from = DIS_c, values_from = DIF)

  tab6 <- cbind(tab6a, tab6b[-1])

# Guardar resultados ----------------------------------------------------------

save(res, margDEM, tab1, tab2, tab3, tab4, tab5, tab6,
     file = "_lpRES.RData")

rm(AUX, CARTO, PRI, SEC)
rm(DIS, DIS2, DIS3, margOFE)
rm(res, margDEM, tab1, tab2, tab3, tab4, tab5, tab6, tab6a, tab6b)

