# Educational

# TODO Usar data.table para optimizar  preparación de bases
# TODO Revisar si N_MATRIC es correcto en CARTO$ESCUELAS. Diferenciar primario
#       y secundario de forma explícita

cli::cli_h2(col_blue("Análisis Educativo (EDU)"))

EDU <- list()

# Definir si se realizan los procesamientos más costosos
#   o se cargan datos procesados anteriormente
forzar <- FALSE

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

  if(!exists("AUX", inherits = F)){
    f <- fs::path("data-workflow", "final-data", "AUX-data", ext = "rda")

    cli_alert_info(
      col_silver("Cargando datos auxiliares (`AUX`): {.path {fs::path_rel(f)}}")
    )

    load(file = f)
    rm(f)
  }

# Preparar datos -------------------------------------

  cli::cli_li(col_yellow("Procesar y preparar datos para el análisis"))

### ╠ Base de Escuelas Secundarias ----------------------

  cli::cli_alert_success(cli::col_grey("Preparando base de escuelas secundarias (`SEC`)"))

  SEC <- CARTO$ESCUELAS |>
    filter(NIV_SEC & !is.na(N_MATRIC))

  cli::cli_alert_success(cli::col_grey("Agregando información sobre segregación escolar"))

  SEC_seg <- SEC |>
    filter(!is.na(PRI) | !is.na(SEC) | !is.na(SUP)) |>
    st_drop_geometry() |>
    mutate(across(PRI:SUP, ~.x*N_MATRIC)) |>
    select(CUEANEXO, PRI:SUP) |>
    pivot_longer(cols = PRI:SUP,  names_to = "EDU",  values_to = "N") |>
    mutual_local(group = "EDU", weight = "N", unit = "CUEANEXO",
                 se = TRUE, wide = TRUE)

  SEC <- left_join(SEC, SEC_seg, by = "CUEANEXO")

  rm(SEC_seg)

### ╠ Identificación de nodos para cada RADIO/ESCUELA (from/to) --------

  RAD <- CARTO$RADIO |>
    filter(!is.na(TH)) |>
    select(ID = ID_RADIO) |>
    arrange(ID) |>
    st_point_on_surface()

  ESC <- SEC  |>
      select(CUEANEXO)  |>
      arrange(CUEANEXO)

{
  tictoc::tic("")
  # ~ 5/6 minutos

  cli::cli_process_start(cli::col_grey("Creando nodos cercanos a radios"))
  blended <- st_network_blend(CALLE, RAD)
  cli_process_done()

  cli::cli_process_start(cli::col_grey("Creando nodos cercanos a escuelas"))
  blended <- st_network_blend(blended, ESC)
  cli_process_done()

  cat(col_blue("Tiempo de ejecución"))
  tictoc::toc()
  invisible(gc())
}

  blended <- blended |>
    activate(edges) |>
    mutate(weight = st_length(geometry))

  nodes <- blended |>
    activate(nodes) |>
    as_tibble() |>
    st_as_sf()

  cli_alert_success(col_silver("Identificar nodos cercanos a radios y escuelas"))

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

  rm(nodes, CALLE)

### ╚ Construir matriz de distancias/costos --------------------------------------
  # Matrices de 3273 x 13995 = 43.841.835 elementos

  cli::cli_li(col_yellow("Calcular distancias entre radios y escuelas"))

  # Matriz de distancia lineal (DIS lineal)
  # ~10 segs

  cli::cli_process_start(cli::col_grey("Calculando distancias lineales"))

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

  cli::cli_process_done()
  setDT(DIS_l, key = c("ID", "CUEANEXO"))

  # Distancias entre nodos por trama (DIS net)
  if(forzar){
    tictoc::tic("")
    # ~ 26/31 mins
    cli::cli_process_start(cli::col_grey("Calculando distancias por red urbana"))
    DIS_n <- igraph::distances(
      graph = blended,
      v = from,
      to = to,
      weights = blended %>% activate(edges) %>% pull(weight)
    )
    cli::cli_process_done()
    cat(col_blue("Tiempo de ejecución"))
    tictoc::toc()
    invisible(gc())

    save(DIS_n, file = "_draft/LP_00_distancias-nodos.Rdata")

  }else{
    cli::cli_process_start(cli::col_grey("Cargando datos de distancias por red urbana de {.path _draft/LP_00_distancias-nodos.Rdata}"))
    load(file = "_draft/LP_00_distancias-nodos.Rdata")
    cli::cli_process_done()
  }

  cli::cli_process_start(cli::col_grey("Armado de bases de distancias (`db_DIS`)"))

  rownames(DIS_n) <- from
  DIS_n <- as.data.table(DIS_n, keep.rownames = "from")

  # colnames(DIS_n) <- to
  setnames(DIS_n, old = names(DIS_n)[-1], new = to)

  DIS_n <- melt(DIS_n, verbose = TRUE, id.vars = "from",
                variable.factor = FALSE, variable.name = "to",
                measure.vars = to, value.name = "DIS_n")

  setkey(DIS_n, "from")
  setindex(DIS_n, "to")

  RAD <- st_drop_geometry(RAD)
  setDT(RAD, key = "from")

  ESC <- st_drop_geometry(ESC)
  setDT(ESC, key = "to")

  # Agregar datos de radios (por nodo 'from')
  DIS_n <- DIS_n[RAD, on = .(from), allow.cartesian = TRUE]

  # Agregar datos de escuelas (por nodo 'to')
  DIS_n <- DIS_n[ESC, on = .(to), allow.cartesian = TRUE]

  # (a) Usar precisión en metros (redondear); (b) Transformar distancias (mts -> km)
  DIS_n <- DIS_n[,`:=`(RAD_min = round(drop_units(RAD_min))/1000,
                       ESC_min = round(drop_units(ESC_min))/1000,
                       DIS_n   = round(DIS_n)/1000)
                   ][ , .(ID, CUEANEXO, DIS_n,
                          # (c) Distancia total (sumando camino lineal hasta nodo)
                          DIS_t = DIS_n + RAD_min + ESC_min)]

  setkeyv(DIS_n, c("ID", "CUEANEXO"))

  # Base de distancias lineales + por red urbana (matriz compacta)
  db_DIS <- merge(DIS_l, DIS_n, by = c("ID", "CUEANEXO"),
                  all = TRUE, allow.cartesian = TRUE)

  cli::cli_process_done()

  # cli::cli_alert_info(col_silver("Guardando base de distancias (`db_DIS`) en {.path _draft/LP_01_distancias.RData}"))
  # save(db_DIS, file = fs::path("_draft", "LP_01_distancias", ext = "RData"))

  rm(DIS_l, DIS_n, ESC, RAD, from, to, blended)

  invisible(gc())

# Análisis -----------------------------------------

### ╠ Calculo de lp  ----------------------------------------------------------

  cli::cli_li(col_yellow("Generar allocation model (EDU4)"))

  # load(fs::path("_draft", "LP_01_distancias", ext = "RData"))
  cli::cli_process_start(cli::col_grey("Preparando bases para modelo lp"))

  # Bases con marginales
  margDEM <- CARTO$RADIO |>
    st_drop_geometry() |>
    filter(!is.na(TH) ) |>
    select(ID = ID_RADIO, TIPO_HAB = TH, SEC = n_estSEC) |>
    arrange(ID) |>
    mutate(SEC = replace_na(SEC, 0))

  margOFE <- SEC |>
    st_drop_geometry() |>
    select(CUEANEXO, n = N_MATRIC, SECTOR)

  cost_mat <- dcast(db_DIS, formula = ID ~ CUEANEXO,
                    value.var = "DIS_t")[margDEM$ID, margOFE$CUEANEXO, with = FALSE] |>
    as.matrix() |>
    round(2)

  rownames(cost_mat) <- margDEM$ID

  cli::cli_process_done()

  if(forzar){
    # ~ 2.72 [h]

    invisible(gc())

    tictoc::tic("")
    cli::cli_process_start(cli::col_grey("Calcular modelo lp"))
    lpres <- PhD_lp_allocation(cost = cost_mat,
                               supply = margDEM$SEC,
                               capacity = margOFE$n,
                               solver = "Rglpk", k = 0.2,
                               verbose = TRUE)
    cli::cli_process_done()
    cat(col_blue("Tiempo de ejecución"))
    tictoc::toc()

    cli::cli_alert_info(col_silver("Guardando resultado lp (`lpres`) en {.path data-workflow/final-data/LPRES.RData}"))
    save(lpres, file = fs::path("data-workflow", "final-data", "LPRES", ext = "RData"))

  }else{
    cli::cli_li(col_yellow("Cargar resultados de model allocation (reslp)"))
    load(file = fs::path("data-workflow", "final-data", "LPRES", ext = "RData"))
  }

  EDU$EDU4 <- lpres

  rm(lpres)

### ╠ Segregación escolar -----------------------------------

### ╚ Diferencia de segregación -----------------------------------

# Figuras ---------------------------------------

### ╠ FIG 7.1: Allocation model -------------------------------

  file.edit("C:/Users/este.de.ahora/Documents/_investigacion/_publicado/2023.06 - Artículo. residential-and-school-segregation/analysis/script/03_Fig1_allocation-model.R")

### ╠ FIG 7.2: Distribución de escuelas -------------------------------

  p2 <- ggplot() +
    geom_sf(data = CARTO$AMGBA) +
    geom_sf(data = SEC,
            aes(size = N_MATRIC^2, color = SECTOR),
            alpha = 0.2) +
    geom_sf(data = CARTO$DPTO_AMBA, color = "grey", fill = NA) +
    geom_sf(data = CARTO$FFCC1 |> filter(sentido == 1),
            color = "grey25", alpha = 0.5, linewidth = 0.2) +
    geom_sf(data = CARTO$FFCC2, color = "grey25",
            alpha = 0.5, size = 0.75) +
    facet_wrap(~SECTOR) +
    scale_color_manual(values = c(Estatal = "darkblue", Privado = "darkgreen")) +
    coord_sf(xlim = AUX$lim_AMBA[c(1,3)],
             ylim = AUX$lim_AMBA[c(2,4)]) +
    guides(size = "none", color = "none") +
    theme_void() +
    theme(strip.text = element_text(face = "bold", size = 16))

  ggsave(file = fs::path("figures", "school_distribution", ext = "png"),
         plot = p2, width = 30, height = 15, units = "cm")

  rm(p2)

### ╠ FIG 7.3: Graficación de función de densidad (Kernel) ------------------

### ╠ FIG 7.4: Descripción de la segregación escolar -------------
  # Ver: Serrati, P. S. (2024). School and Residential Segregation in the Reproduction of Urban Segregation: A Case Study in Buenos Aires. Urban Studies, 61(2), 313-330. https://doi.org/10.1177/00420980231178401
  # Local school segregation dispersion (left); and sample variogram (right)

  # Boxplot school M (left)
  # TODO Ver M común

  p4a <- SEC |>
    filter(!is.na(ls)) |>
    ggplot(aes(x= SECTOR, y = ls)) +
    geom_boxplot(color = "grey60", outlier.alpha = 0.5,
                 notch = T, width = 0.2) +
    geom_violin(alpha = 0.3, color =NA, aes(fill = SECTOR)) +
    # geom_hline(yintercept = SEGRE$tab_indic$School[3],
    #            color = "tomato3", linetype = 3 ) +
    # geom_text(x = 1.5, y = SEGRE$tab_indic$School[3] + 0.03,
    #           label = "Global M Index", color = "tomato4") +
    guides(fill = "none") +
    labs(x = "",
         y = "Local Mutual Information Index") +
    ggsci::scale_fill_d3() +
    theme_minimal()

  # Semivariogram (right)
  p4b <- EDU$EDU3 |>
    ggplot(aes(x = dist)) +
    geom_ribbon(aes(ymin = low, ymax = up),
                color = "lightsteelblue3", fill = "grey60",
                alpha = 0.1 ) +
    geom_point(aes(y = gamma), color = "darkolivegreen") +
    geom_col(aes(y = np/800000), alpha = 0.3, fill = "dodgerblue3" ) +
    scale_y_continuous(expression(paste("Semivarianza ", gamma, "(h)", sep = "")),
                       limits = c(0, NA),
                       sec.axis = sec_axis(~. * 800000, name = "Conteo de relaciones (n)") ) +
    scale_x_continuous("Distancia entre escuela en metros (h)") +
    theme_minimal()

  ggsave(file = fs::path("figures", "school_variogram", ext = "png"),
         plot = ggarrange(p4a, p4b), width = 20, height = 10, units = "cm")

  rm(p4a, p4b)

### ╠ FIG 7.5: Distancia promedio de los nodos de radios a escuelas -------------------------------------------------

### ╠ FIG 7.6: Relationship between real and estimated segregation of each school (left); and Location of schools according to difference in local segregation (right) -----------------

  # Ver: Serrati, P. S. (2024). School and Residential Segregation in the Reproduction of Urban Segregation: A Case Study in Buenos Aires. Urban Studies, 61(2), 313-330. https://doi.org/10.1177/00420980231178401
  # Relationship between real and estimated segregation of each school (left); and Location of schools according to difference in local segregation (right)
  file.edit("C:/Users/este.de.ahora/Documents/_investigacion/_publicado/2023.06 - Artículo. residential-and-school-segregation/analysis/script/03_Fig4_local-segregation.R")

### ╠ FIG 7.7: Composición de escuelas (colores + gráfico ternario) ------------------------------


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

### ╠ TAB01: -----------------------------
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

### ╠ TAB03: Diferencia de segregación --------------------------------------

  file.edit("C:/Users/este.de.ahora/Documents/_investigacion/_publicado/2023.06 - Artículo. residential-and-school-segregation/analysis/script/03_Tables.R")

### ╚ TAB06: Comparación de distancias lineales y por red urbana ------------
  # Comparación de distancias lineales y por red urbana

  aux_TAB06a <- db_DIS |>
    mutate(DIF = DIS_t - DIS_l) |>
    left_join(AUX$RADIO[c("ID", "TIPO_HAB2")], by = "ID") |>
    group_by(TIPO_HAB2)  |>
    summarise(DIF = mean(DIF))

  aux_TAB06a <- DIS2 |>
    mutate(DIF = DIS_t - DIS_l,
           DIS_c = cut(DIS_l, breaks = c(0, 0.75, 2, 20),
                       labels = c("Menos de 750m", "Entre 750m y 2km",
                                  "Más de 2km") ) )  |>
    left_join(AUX$RADIO[c("ID", "TIPO_HAB2")], by = "ID") |>
    group_by(TIPO_HAB2, DIS_c, .drop = T) |>
    summarise(DIF = mean(DIF)) |>
    filter(!is.na(DIS_c) ) |>
    pivot_wider(id_cols = TIPO_HAB2, names_from = DIS_c, values_from = DIF)

  EDU$TAB06 <- cbind(aux_TAB06a, aux_TAB06a[-1])

  rm(aux_TAB06a, aux_TAB06a)

# Guardar resultados ----------------------------------------------------------

  usethis::use_data(EDU, overwrite = TRUE)

# rm(AUX, CARTO, CALLE)
