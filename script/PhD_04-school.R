# Educational

# TODO  Modificar directorios _draft en bases intermedias
# TODO? Filtrar si los costos son más altos que un límite en
#         PhD_lp_make_edges()
# TODO? Incorporar covariable (cantidad de población) en densidad o
#         conteo por cuadrantes?  Dejar para otro trabajo (no aporta
#         a segregación).
# TODO? Calcular la distancia promedio de los nodos de un radio a la
#         escuela más cercana por red. Dejar para otro trabajo (no
#         aporta a segregación).

cli::cli_h2(col_blue("Análisis Educativo (EDU)"))

EDU <- list()

# Definir si se realizan los procesamientos más costosos
#   o se cargan datos procesados anteriormente
forzar <- FALSE
replic <- 1000

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
    filter(NIV_SEC & !is.na(N_MATRIC)) |>
    mutate(AP_sel = n_sinNA >= 5 & !is.na(PRI) & !is.na(SEC) & !is.na(SUP))

  cli::cli_alert_success(cli::col_grey("Agregando información sobre segregación escolar"))

  SEC_seg <- SEC |>
    st_drop_geometry() |>
    filter(AP_sel) |>
    mutate(across(PRI:SUP, ~.x * N_MATRIC)) |>
    select(CUEANEXO, PRI:SUP) |>
    pivot_longer(cols = PRI:SUP,  names_to = "EDU",  values_to = "N") |>
    mutual_local(group = "EDU", weight = "N", unit = "CUEANEXO",
                 se = TRUE, wide = TRUE)

  SEC <- left_join(SEC, SEC_seg, by = "CUEANEXO")

  rm(SEC_seg)

### ╠ Armado de ppp --------------------------------------

  cli::cli_alert_success(cli::col_grey("Armando objeto ppp de escuelas secundarias"))

  # Ventana (w)
  w <- CARTO$AMGBA |>
    st_buffer(dist = 1000) |>
    st_concave_hull(ratio = 0.2) |>
    as.owin()

  # Objeto ppp (Point Pattern Process)
  set.seed(123)
  ESC_aux <- SEC |>
    st_jitter(amount = 50) |>
    mutate(x = st_coordinates(geometry)[,1],
           y = st_coordinates(geometry)[,2]) %>%
    st_drop_geometry()

  ESC_ppp <- ppp(x = ESC_aux$x, y = ESC_aux$y,
                 marks = ESC_aux$SECTOR,
                 window = w)  |>
    rescale(1000, "km")

  attr(ESC_ppp, "weights") <- ESC_aux$N_MATRIC

  rm(w, ESC_aux)

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

  cli::cli_alert_info(col_silver("Guardando base de distancias (`db_DIS`) en {.path _draft/LP_01_distancias.RData}"))
  save(db_DIS, file = fs::path("_draft", "LP_01_distancias", ext = "RData"))

  rm(DIS_l, DIS_n, ESC, RAD, from, to, blended)

  invisible(gc())

# Análisis -----------------------------------------

### ╠ Intensidad global de oferta (vacantes / escuelas)  --------------------------
  # Indica cantidad de vacantes/escuela por km2 (suponiendo homogeneidad)

  cli::cli_li(col_yellow("Calcular intensidad de escuelas secundarias (EDU1)"))

  EDU$EDU1 <- rbind.data.frame(
    map_dbl(list("Estatal", "Privado", c("Estatal", "Privado")),
            ~intensity(ESC_ppp[marks(ESC_ppp) %in% .x])) |>
      round(3),
    map_dbl(list("Estatal", "Privado", c("Estatal", "Privado")),
            ~intensity(ESC_ppp[marks(ESC_ppp) %in% .x],
                       weights = attr(ESC_ppp, "weights")[marks(ESC_ppp) %in% .x])) |>
      round(1)
    ) |>
    set_names(c("Estatal", "Privado", "Total")) |>
    mutate(Intensidad = c("Escuelas", "Alumnos")) |>
    select(Intensidad, everything()) |>
    as_tibble()

### ╠ Estimación de la densidad de oferta --------------------------

  cli::cli_li(col_yellow("Estimar densidad de escuelas secundarias (EDU2)"))

  EDU$EDU2 <- ESC_ppp |>
    rescale(1/1000, "m") |>
    adaptive.density(method = "kernel", dimyx = c(300, 300),
                     weights = attr(ESC_ppp, "weights"), edge = FALSE)

### ╠ Variograma de correlación espacial (segregación) ----------------------------------

  cli::cli_li(col_yellow("Calcular variograma de segregación escolar (EDU3)"))

  vg <- SEC |>
    filter(!is.na(ls)) |>
    variogram(object = ls ~ 1, width = 500, cutoff = 7500)

  CI_vg <- variosig::envelope(vario = vg, formula = ls ~ 1,
                              width = 500, cutoff = 7500,
                              conf.level = 0.99, nsim = 2000,
                              data = SEC |> filter(!is.na(ls)) |> as_Spatial() )

  EDU$EDU3 <- vg |>
    mutate(low = CI_vg$lower,
           up = CI_vg$upper)

  attr(EDU$EDU3, "signific") <- variosig::envsig(CI_vg, method = "eb")

  # fit_vg <- fit.variogram(vg, model = vgm("Sph"))
  rm(vg, CI_vg)

### ╠ Calculo de lp  ----------------------------------------------------------

  cli::cli_li(col_yellow("Generar allocation model (EDU4)"))

  # load(fs::path("_draft", "LP_01_distancias", ext = "RData"))
  cli::cli_process_start(cli::col_grey("Preparando bases para modelo lp"))

  # Bases con marginales
  margDEM <- CARTO$RADIO |>
    st_drop_geometry() |>
    filter(!is.na(TH) ) |>
    select(ID = ID_RADIO, TH, EST_SEC = n_estSEC, HOG_PRI:HOG_SUP) |>
    rename_with(~str_remove(., "HOG_")) |>
    mutate(EST_SEC = replace_na(EST_SEC, 0)) |>
    arrange(ID)

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
    # ~ 2.7 [h]

    invisible(gc())

    tictoc::tic("")
    cli::cli_process_start(cli::col_grey("Calcular modelo lp"))
    lpres <- PhD_lp_allocation(cost = cost_mat,
                               supply = margDEM$EST_SEC,
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

### ╠ Armado de bases de segregación -----------------------------------

  cli::cli_li(col_yellow("Armado de bases de segregación (EDU5)"))

  SEGRE <- list()

  # ► Residential db
  SEGRE$db_RESI <- margDEM |> select(ID, PRI:SUP)

  # ► School db (real situation)
  SEGRE$db_EDU <- SEC |>
    st_drop_geometry() |>
    filter(AP_sel) |>
    mutate(across(PRI:SUP, ~.x * N_MATRIC)) |>
    select(CUEANEXO, PRI:SUP)

  # ► School db (model situation)
  base <- EDU$EDU4$solution |> slam::as.simple_triplet_matrix()

  if(any(rownames(base) != SEGRE$db_RESI$ID)){
    cli::cli_abort("ID de Radios en la matriz de asignación no coinciden con nombres de la base original")
  }

  ALLOCATION_aux <- SEGRE$db_RESI |>
    mutate(TOT = PRI  + SEC + SUP,
           across(PRI:SUP, ~.x / TOT) ) |>
    select(PRI:SUP) |>
    map_dfc(\(EDU) apply(base * EDU, 2, sum))

  SEGRE$db_MODEL <- bind_cols(CUEANEXO = colnames(base), ALLOCATION_aux)

  EDU$EDU5 <- SEGRE

  rm(ALLOCATION_aux, base, SEGRE)

  # # Cálculo original:
  #
  # # Radio composition
  # rad_aux <- SEGRE$db_RESI |>
  #   select(-ID, - TH) |>
  #   rename_with(~str_remove(., "HOG_")) |>
  #   as.matrix() |>
  #   prop.table(margin = 1)
  #
  # # Theoretical (modelled) School Composition
  # base <- EDU$EDU4$solution
  # SEGRE$db_MODEL <- data.frame(ID_s = colnames(base))
  #
  # for(i in colnames(rad_aux)){
  #   SEGRE$db_MODEL[[i]] <- apply(base * rad_aux[ , i], 2, sum)
  # }
  #
  # rm(rad_aux, i, base)

### ╠ Diferencia de segregación -----------------------------------

# Figuras ---------------------------------------

### ╠ FIG 7.1: Allocation model -------------------------------

  # file.edit("C:/Users/este.de.ahora/Documents/_investigacion/_publicado/2023.06 - Artículo. residential-and-school-segregation/analysis/script/03_Fig1_allocation-model.R")

### ╠ FIG 7.2: Distribución de escuelas -------------------------------

  cli::cli_li(col_yellow("Figure 7.2: Distribución de escuelas secundarias."))

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

  cli::cli_li(col_yellow("Figure 7.3: Densidad estimada de capacidad en escuelas secundarias."))

  p3 <- ggplot() +
    stars::geom_stars(data = EDU$EDU2 |>
                        stars::st_as_stars(),
                      aes(fill = v), alpha = 0.8) +
    geom_sf(data = CARTO$DPTO_AMBA, fill = NA,
            color = "grey", size = 0.5) +
    geom_sf(data = CARTO$FFCC1 |> filter(sentido == 1),
            color = "grey25", alpha = 0.5, linewidth = 0.2) +
    geom_sf(data = CARTO$FFCC2, color = "grey25",
            alpha = 0.5, size = 0.75) +
    geom_sf(data = SEC, color = "black",
            size = 0.1, alpha = 0.15) +
    coord_sf(xlim = AUX$lim_AMBA[c(1,3)],
             ylim = AUX$lim_AMBA[c(2,4)]) +
    scale_fill_viridis_c(option = "turbo", na.value = "white",
                         direction = 1) +
    labs(#caption  = paste0("Método: Kernel adaptativo"),
         #title = "Densidad estimada de vacantes en escuelas secundarias",
         fill = "Densidad")+
    theme_void()

  ggsave(file = fs::path("figures", "school_density", ext = "png"),
         plot = p3, width = 20, height = 20, units = "cm")

  rm(p3)

### ╠ FIG 7.4: Descripción de la segregación escolar -------------
  # Ver: Serrati, P. S. (2024). School and Residential Segregation in the Reproduction of Urban Segregation: A Case Study in Buenos Aires. Urban Studies, 61(2), 313-330. https://doi.org/10.1177/00420980231178401
  # Local school segregation dispersion (left); and sample variogram (right)

  # Boxplot school M (left)
  # TODO Ver M común
  cli::cli_li(col_yellow("Figure 7.4: Descripción de la segregación escolar."))

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
  # file.edit("C:/Users/este.de.ahora/Documents/_investigacion/_publicado/2023.06 - Artículo. residential-and-school-segregation/analysis/script/03_Fig4_local-segregation.R")

### ╠ FIG 7.7: Composición de escuelas (colores + gráfico ternario) ------------------------------


### ╚ FIG App ?.?: Comparación entre distancias lineales y por red vial --------------------

  set.seed(123)

  p8 <- db_DIS |>
    sample_frac(size = 0.05) |>
    ggplot() +
    geom_point(aes(x = DIS_l, y = DIS_t),
               alpha = 0.1, color = "cornflowerblue") +
    geom_abline(intercept = 0, slope = 1,
                linetype = 2, color = "tomato3") +
    labs(x = "Distancia lineal (km)",
         y = "Distancia por red urbana (km)") +
    theme_minimal()

  ggsave(filename = fs::path("figures", "school_distances-comparation", ext = "png"),
         plot = p8, width = 15, height = 15, units = "cm", dpi = 300)

  rm(p8)

# Tablas ---------------------------

### ╠ TAB 7.1: Distancia promedio recorrida x entorno -----------------------------

  EDU$TAB1 <- EDU$EDU7 |>
    group_by(TH) |>
    summarise (radios = n(),
               EST = sum(EST_SEC),
               SV = sum(SINVAC),
               p_SV = SV/EST * 100,
               dis_min = weighted.mean(distMIN, na.rm = T) * 1000,
               dis_alloc = weighted.mean(distPROM, (EST_SEC-SINVAC), na.rm = T) * 1000) |>
    as_tibble() %>%
    add_row(TH = "Total AMBA",
            radios = sum(.$radios),
            EST = sum(.$EST),
            SV = sum(.$SV),
            p_SV = SV/EST * 100,
            dis_min = NA,
            dis_alloc = weighted.mean(.$dis_alloc, .$EST - .$SV) ) |>
    mutate(across(starts_with("dis_"), round),
           p_SV = round(p_SV, 1),
           s1 = "", s2 = "") |>
    select(TH, radios, EST, s1, SV, p_SV, s2, dis_min, dis_alloc) |>
    flextable() |>
    set_caption(caption = "Descripción del modelo de asignación",
                autonum = run_autonum(seq_id = "tab", bkm = "school-tab1",
                                      bkm_all = TRUE)) |>
    colformat_double(big.mark = " ", decimal.mark = ",",
                     digits = 0, na_str = "-") |>
    colformat_int(big.mark = " ", na_str = "-") |>
    colformat_double(j = 6, suffix = "%", digits = 1, ,
                     big.mark = " ", decimal.mark = ",") |>
    set_header_labels(TH = "Entorno Urbano", radios = "Radios",
                      EST = "Estudiantes", s1 = " ", SV = "Total",
                      p_SV = "%", s2 = " ", dis_min = "Mínima",
                      dis_alloc = "Modelo") |>
    add_header_row(values = c("Entorno Urbano", "Totales",
                              " ",  "Sin vacantes",  " ",
                              "Distancia promedio [m]"),
                   colwidths = c(1, 2, 1, 2, 1, 2)) |>
    merge_v(j = c(1, 4, 7), part = "header") |>
    align(j = -1, i = 1, align ='center', part = 'header') |>
    bold(i = 10, part = "body") |>
    hline(i = 9:10, border = fp_border(width = 1)) |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 7, part = "all") |>
    flextable::width(j = 1, 2, unit = "cm") |>
    flextable::width(j = c(4, 7), 0.25, unit = "cm") |>
    flextable::width(j = c(2:3, 5:6, 8:9), 1.15, unit = "cm")

### ╠ TAB 7.2: Diferencia de segregación --------------------------------------

  name_lab <- c("Empírico", "Modelo",
                "Diferencia (Modelo - Empírico)",
                "Unidades agregadas", "Unidades removidas",
                "Marginal de Grupos Educativos",
                "Marginal de Unidades",
                "Segregación estructural")

  EDU$TAB2 <- EDU$EDU6 |>
    filter(is.na(CUEANEXO)) |>
    mutate(grupo = c(rep("Segregación escolar", 2),
                     "Diferencia (Modelo - Empírico)",
                     rep("Descomposición", 5) ),
           name = name_lab,
           por = round(est/est[3]*100, 1),
           por = paste0(format(round(por, 1), digits = 1, nsmall = 1), "%"),
           por = case_when(stat %in% c("M1", "M2") ~ "",
                           T ~ por),
           CI = CI |>
             map(round, digits = 3) |>
             map(format, scientific = FALSE, digits = 3, nsmall = 3) |>
             map(str_c, collapse = "/") |>
             unlist(),
           CI = paste0("(", CI, ")") ) |>
    select(grupo, name, est, CI, por) |>
    flextable() |>
    set_caption(caption = "Descomposición de la diferencia de segregación (Índice de Información Mutua)",
                autonum = run_autonum(seq_id = "tab", bkm = "school-tab2",
                                      bkm_all = TRUE)) |>
    set_header_labels(grupo = "", name = "", est = "Value",
                      CI = "CI", por = "% Explicado") |>
    add_header_row(values = c("", "",
                              "Índice de Información Mutua (M)",
                              "% Explicado"),
                   colwidths = c(1, 1, 2, 1)) |>
    colformat_double(big.mark = " ", decimal.mark = ",", na_str = "-") |>
    colformat_int(big.mark = " ", na_str = "-") |>
    colformat_double(j = 3, digits = 3,
                     decimal.mark = ",", big.mark = " ") |>
    italic(j = 4, part = "body") |>
    bold(i = 3, part = "body") |>
    color(j = 4, part = "body", color = "grey35") |>
    merge_v(j = c(1:2, 5), part = "header") |>
    merge_v(j = 1, part = "body") |>
    merge_h(i = 3, part = "body") |>
    align(align ='center', part = 'all') |>
    align(j = 1:2, align ='left', part = 'all') |>
    align(j = 5, align ='center', part = 'header') |>
    align(j = 5, i = 4:8, align ='right', part = 'body') |>
    align(j = 5, i = 3, align ='center', part = 'body') |>
    hline(i = 2:3, border = fp_border(width = 1)) |>
    add_footer_lines("Estimator method: Shapley.") |>
    footnote(part = "header", j = 3, i = 1,
             value = as_paragraph(
               paste("Los valores están corregidos por sesgo.",
                     replic, "iteraciones Bootstrap.",
                     "Intervalo de confianza (CI) = 99 %") ),
             ref_symbols = "a") |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 7, part = "all")

  rm(name_lab)

### ╠ TAB 7.3: Comparación de distancias lineales y por red urbana ------------
  # Comparación de distancias lineales y por red urbana

  # aux_TAB06a <- db_DIS |>
  #   mutate(DIF = DIS_t - DIS_l) |>
  #   left_join(AUX$RADIO[c("ID", "TIPO_HAB2")], by = "ID") |>
  #   group_by(TIPO_HAB2)  |>
  #   summarise(DIF = mean(DIF))
  #
  # aux_TAB06a <- DIS2 |>
  #   mutate(DIF = DIS_t - DIS_l,
  #          DIS_c = cut(DIS_l, breaks = c(0, 0.75, 2, 20),
  #                      labels = c("Menos de 750m", "Entre 750m y 2km",
  #                                 "Más de 2km") ) )  |>
  #   left_join(AUX$RADIO[c("ID", "TIPO_HAB2")], by = "ID") |>
  #   group_by(TIPO_HAB2, DIS_c, .drop = T) |>
  #   summarise(DIF = mean(DIF)) |>
  #   filter(!is.na(DIS_c) ) |>
  #   pivot_wider(id_cols = TIPO_HAB2, names_from = DIS_c, values_from = DIF)
  #
  # EDU$TAB06 <- cbind(aux_TAB06a, aux_TAB06a[-1])
  #
  # rm(aux_TAB06a, aux_TAB06a)

### ╚ TAB App G.1: Índices de segregación comparado -----------------

  EDU$appG_TAB1 <- EDU$EDU5 |>
    map2(names(EDU$EDU5) |> str_remove("db_"),
         \(db, nm){
           db |>
             set_names(c("ID_RADIO", "PRI", "SEC", "SUP")) |>
             PhD_seg_summarise(bootstrap = replic) |>
             set_names(c("Index", nm))
         }) |>
    reduce(left_join, by = "Index") |>
    mutate(EXP = MODEL / EDU * 100,
           s = "") |>
    select(Index, RESI, EDU, s, MODEL, EXP) |>
    flextable() |>
    set_caption(caption = "Indicadores de segregación escolar según valores observados y modelado",
                autonum = run_autonum(seq_id = "tab", bkm = "appG-segreg-index",
                                      bkm_all = TRUE)) |>
    align(align ='center', part = 'all') |>
    align(j = 1, align ='left', part = 'all') |>
    colformat_double(digits = 3, big.mark = " ",
                     decimal.mark = ",", na_str = "-") |>
    colformat_int(big.mark = " ", na_str = "-") |>
    colformat_double(j = 6, digits = 1, suffix = "%") |>
    PhD_compose_eq() |>
    set_header_labels(Index = "Índice de Segregación",
                      RESI = "Residencial",  EDU = "Escolar",
                      s = " ", MODEL = "Asignado", EXP = "% Explicado") |>
    add_header_row(values = c("Índice de Segregación", "Empírico",
                              s = " ", "Modelo"),
                   colwidths = c(1, 2, 1, 2)) |>
    merge_v(j = c(1, 4), part = "header") |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 7, part = "all") |>
    flextable::width(j = 1, 5.25, unit = "cm") |>
    flextable::width(j = 4, 0.25, unit = "cm") |>
    flextable::width(j = c(2:3, 5:6), 1.25, unit = "cm")

# Guardar resultados ----------------------------------------------------------

  usethis::use_data(EDU, overwrite = TRUE)

# rm(AUX, CARTO)

# Otros resultados ---------------------------

### Intens. inhomogénea: Conteo por cuadrantes --------------------------------

  # # Tesselación Hexagonal (filtrado de áreas pequeñas)
  # # Hexágonos de 750 metros (se filtran hexágonos que son menos de 10% de un hexágono normal)
  # H <- hextess(ESC_ppp, 0.750)
  # H <- H[tile.areas(H) > max(tile.areas(H)) * 0.1]
  #
  # # Test de hipótesis
  # # Para validad $H_0$ el p-value debe ser mayor que un nivel de confianza establecido.
  # ## Hip nula: la intensidad es homogénea. La división por cuadrantes responde a una distribución de Poisson (CSR)
  # ## Hip alternativa: la intensidad no es homogénea en una forma no especificada
  #
  # # 99.9 % de rechazo a H0
  #
  # EDU$ppp_p <- map_dbl(list("Estatal", "Privado", c("Estatal", "Privado")),
  #              ~quadrat.test(ESC_ppp[marks(ESC_ppp) %in% .x], nsim = 5000,
  #                            tess = H, method = "MonteCarlo")$p.value) |>
  #   set_names(c("Estatal", "Privado", "Total"))
  #
  # rm(H)
  #
  # a1 <- map_dbl(c(0.500, 1.000, 2.000, 4.000, 6.000, 8.000),
  #               \(distancia){
  #                 quadratcount.ppp(ESC_ppp,
  #                                  tess = hextess(ESC_ppp, distancia)) |>
  #                   quadrat.test(method = "MonteCarlo", nsim = 10000) |>
  #                   pluck("p.value")
  #               }) |>
  #   set_names(paste0("HEX_size_", c(500, 1000, 2000, 4000, 6000, 8000)))

### Estimación de la densidad --------------------------

  # # Optimizar el valor sigma ("esfumado").
  # s <- bw.ppl(ESC_ppp)
  # # bw.diggle(ESC_ppp)
  # # bw.scott(ESC_ppp)
  #
  # # Cálculo de intensidad (Kernel)
  # EDU$ppp_density <- ESC_ppp |>
  #   rescale(1/1000, "m") |>
  #   density(diggle = TRUE,  edge = TRUE, sigma = s * 1000, dimyx = c(300, 300),
  #           verbose = TRUE, weights = attr(ESC_ppp, "weights"))
  #
  # attr(EDU$ppp_density, "sigma") <- s
  # rm(s)
  #
  # EDU$EDU2 <- ESC_ppp |>
  #   rescale(1/1000, "m") |>
  # adaptive.density(method = "nearest", k = 60, dimyx = c(300, 300),
  #                  weights = attr(ESC_ppp, "weights"))
