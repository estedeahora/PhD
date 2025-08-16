# Residential segregation

cli::cli_h2(col_blue("Análisis de Segregación Residencial (SEG)"))

RES <- list()

# Cargar datos --------------------------------------

if(!exists("CARTO", inherits = F)){
  f <- fs::path("data-workflow", "final-data", "CARTO", ext = "rda")
  cli_alert_info(
    col_silver("Cargando cartografía (`CARTO`): {.path {fs::path_rel(f)}}")
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

# Preparar datos ------------------------------------

  cli::cli_li(col_yellow("Crear bases de datos auxiliares"))

  db_aux1 <- CARTO$RADIO |>
    filter(!is.na(TH)) |>
    st_drop_geometry() |>
    select(ID_RADIO, IDPROV = PROV, IDDPTO = DPTO, TH, ls,
           HOG, starts_with("HOG_")) |>
    rename_with(~str_remove(.x, "HOG_"))

# Análisis de segregación residencial -------------------------------

### ╠ Segregación residencial global (listado de indicadores) ------------------

  cli::cli_li(col_yellow("Calcular segregación residencial global (SEG1)."))

  RES$SEG1 <- PhD_seg_summarise(db_aux1, filtro = ".") |>
    left_join(PhD_seg_summarise(db_aux1, filtro = "02"), by = "Index") |>
    left_join(PhD_seg_summarise(db_aux1, filtro = "06"), by = "Index" ) |>
    set_names(c("Index", "AMBA", "CABA", "PBA"))

### ╠ Segregación Between-Within (Provincias / Departamento / entorno) ---------

  cli::cli_li(col_yellow("Calcular segregación Between-Within (SEG2)."))

  db_aux2 <- db_aux1 |>
    select(ID_RADIO, IDPROV, IDDPTO, TH, PRI:SUP) |>
    pivot_longer(cols = PRI:SUP, values_drop_na = F)

  RES$SEG2 <- rbind(
                Jurisdicciones = PhD_seg_between_within(db_aux2, entre = "IDPROV", se = F),
                Departamentos = PhD_seg_between_within(db_aux2, entre = "IDDPTO", se = F),
                Entornos_urbanos = PhD_seg_between_within(db_aux2, entre = "TH", se = F)
                    ) |>
    as.data.frame() |>
    mutate(across(WITHIN:BETWEEN, ~round(.x/TOTAL * 100, 1)))

### ╠ Aporte de cada grupo social ----------------------------------

  cli::cli_li(col_yellow(paste0("Calcular aporte de cada grupo educativo",
                                "a la segregación (SEG3).")))

  RES$SEG3 <- segregation::mutual_local(db_aux2,
                                        unit = "name",
                                        group = "ID_RADIO",
                                        weight = "value",
                                        wide = TRUE,
                                        se = FALSE)

### ╚ Within de cada Entorno urbano -------------------

  cli::cli_li(col_yellow("Calcular segregación dentro de cada Entorno urbano (SEG4)."))

  RES$SEG4 <- left_join(
    db_aux2 |>
      count(TH, name, wt = value) |>
      group_by(TH) |>
      mutate(p = n/sum(n) * 100) |>
      pivot_wider(id_cols = TH, names_from = name, values_from = p),
    segregation::mutual_within(db_aux2[!is.na(db_aux2$TH), ], unit = "ID_RADIO",
                               group = "name", weight = "value", se = F,
                               within = "TH", wide = T)[,c("TH", "p", "M")],
    by = "TH"
  )

  RES$SEG4 <- bind_rows(RES$SEG4,
                        data.frame(TH = "TOTAL",
                                   db_aux1 |>
                                     summarise(across(c(PRI:SUP, p = HOG),
                                                      ~sum(.x)),
                                               across(PRI:SUP, ~.x/p * 100)),
                                   M = RES$SEG1[3, 2])
  )

  rm(db_aux2)

# Figuras  ------------------------------------

### ╠ FIG 4.1: Distribución espacial de la segregación local (M) ----------------

  cli::cli_li(col_yellow("Figure 4.1: Distribución espacial de la segregación local"))

  FFCC1 <- CARTO$FFCC1 |> filter(linea_2 == "Sarmiento")
  FFCC2 <- CARTO$FFCC2 |> filter(linea == "Sarmiento")

  # AMGBA
  p1_AMBA <- CARTO$RADIO |>
    PhD_ggzoom(dpt = CARTO$DPTO_AMBA,
            tit = "Área Metropolitana Gran Buenos Aires") |>
    list() |>
    c(map(AUX$lim_DETAILS, PhD_geom_detail)) |>
    reduce(`+`) +
    PhD_geom_detail_text(AUX$lim_DETAILS$ZN1,
                     lab_x = 3, hjust = 1, label = "Detalle 1") +
    PhD_geom_detail_text(AUX$lim_DETAILS$ZN2, label = "Detalle 2") +
    PhD_geom_detail_text(AUX$lim_DETAILS$ZN3,
                     color_lab = "cornsilk3", label = "Detalle 3") +
    PhD_geom_detail_text(AUX$lim_DETAILS$ZN4,
                     color_lab = "cornsilk3", label = "Detalle 4") +
    coord_sf(xlim = AUX$lim_AMBA[c(1,3)],
             ylim = AUX$lim_AMBA[c(2,4)]) +
    labs(fill = "Segregación local (M)") +
    theme(legend.position = "inside",
          legend.position.inside = c(0.90, 0.80))

  p1_DET <- list(AUX$ZN,
             c("Detalle 1: CABA y Zona Norte\n(San Isidro y Vte. López)",
               "Detalle 2: Expansión norte\n(Pilar, Escobar y Tigre)",
               "Detalle 3: Cascos urbanos ferroviarios \n(Ej: FFCC Sarmiento)",
               "Detalle 4: Periferia deteriorada\n(Ej: R. Calzada, Solano, Gdor. Costa)" ),
             AUX$lim_DETAILS) |>
    pmap(PhD_ggzoom) |>
    set_names(letters[1:4])

  # Agregar FFCC en Zona 3 (Morón)
  p1_DET$c <- p1_DET$c +
    geom_sf(data = FFCC1, color = "cornsilk4") +
    geom_sf(data = FFCC2, shape = 21, color = "cornsilk", fill = "coral") +
    geom_sf_text(data = FFCC2, aes(label = nombre), nudge_y = 150,
                 color = "cornsilk", size = 3) +
    coord_sf(xlim = AUX$lim_DETAILS$ZN3[c(1,3)],
             ylim = AUX$lim_DETAILS$ZN3[c(2,4)])

  p1_d1 <- ggarrange(p1_DET$a, p1_DET$c, common.legend = T, legend = "none",
                    nrow = 2, heights = c(6, 5))
  p1_d2 <- ggarrange(p1_DET$b, p1_DET$d, common.legend = T, legend = "none",
                    nrow = 2, heights = c(5, 6))

  p_blank <- ggplot() + theme_void()

  p1_d <- ggarrange(p1_d1, p_blank, p1_d2, common.legend = F,
                    nrow = 1, widths = c(5, 0.2, 5) )

  # Versión vertical
  p1 <- ggarrange(p1_AMBA, p_blank, p1_d, common.legend = F,
                 ncol = 1, heights = c(10, 0.2, 9))

  cli::cli_alert_info(col_silver("Guardando {.path figures/Segregacion_AMBA.png}"))
  ggsave(fs::path("figures", "Segregacion_AMBA.png"),
         plot = p1,
         width = 26, height = 42, unit = "cm", dpi = 300)

  rm(p1, p1_DET, p1_AMBA, p1_d, p1_d1, p1_d2, p_blank)

### ╠ FIG 4.2: Distribución de grupos en CA ----------------

  cli::cli_li(col_yellow("Figure 4.2: Distribución de grupos en CA"))

  res_CA <- db_aux1 |>
    select(PRI, SEC, SUP) |>
    FactoMineR::CA(graph = F)

  coord_lab <- res_CA$col$coord |>
    as_tibble(rownames = "lab") |>
    mutate(`Dim 1` = `Dim 1` * 1/sqrt(res_CA$eig[1, 1]),
           `Dim 2` = `Dim 2` * 1/sqrt(res_CA$eig[2, 1]))

  p2a <- res_CA |>
    factoextra::fviz_ca_biplot(alpha.row = 0, alpha.col = 0,
                               map = "rowprincipal",
                               geom = "point", title = NULL) +
    geom_polygon(data = coord_lab, col = 'grey60', fill = NA,
                 linetype = 2, linewidth = 0.5,
                 aes(x = `Dim 1`, y = `Dim 2`)) +
    geom_point(data = res_CA$row$coord, alpha = 0.2,
               aes(x = `Dim 1`, y = `Dim 2`, color = db_aux1$ls)) +
    geom_text(data = coord_lab, color = "grey10",
              aes(x = `Dim 1` + (0.05 * `Dim 1`),
                  y = `Dim 2` + (0.05 * `Dim 2`),
                  label = lab) ) +
    scale_color_viridis_c("Segregación", option = "B", limits = c(0, 1.1)) +
    theme(legend.position = "inside", legend.direction = "horizontal",
          legend.title.position = "top",
          legend.position.inside = c(0.80, 0.15))

  p2b <- res_CA |>
    factoextra::fviz_ca_biplot(alpha.row = 0, alpha.col = 0,
                               map = "rowprincipal",
                               geom = "point", title = NULL) +
    geom_polygon(data = coord_lab, col = 'grey60', fill = NA,
                 linetype = 2, linewidth = 0.5,
                 aes(x = `Dim 1`, y = `Dim 2`)) +
    geom_point(data = res_CA$row$coord,
               aes(x = `Dim 1`, y = `Dim 2`,
                   alpha = 0.4,#db_aux1$ls,
                   color = I(db_aux1$color))) +
    geom_text(data = coord_lab, color = "grey10",
              aes(x = `Dim 1` + (0.05 * `Dim 1`),
                  y = `Dim 2` + (0.05 * `Dim 2`),
                  label = lab) ) +
    guides(alpha = "none") +
    annotation_custom(ggtern::ggplotGrob(PhD_ggtern_legend(.step = 4, .alpha = 0.6)),
                      xmin = 0.4, xmax = 1.5, ymin = -1.5, ymax = -0.5)

  p2_text <- paste(" Nota: Se utilizan coordenadas principales",
                   "para la ubicación de columnas (grupos educativos).") |>
    ggparagraph(face = "italic", size = 11, color = "black")

  p2 <- ggarrange(p2a, p2b, ncol = 2)
  p2 <- ggarrange(p2, p2_text, nrow = 2, heights = c(1, 0.05))

  cli::cli_alert_info(col_silver("Guardando {.path figures/Segregacion_CA.png}"))
  ggsave(fs::path("figures", "Segregacion_CA.png"),
         plot = p2,
         width = 26, height = 14, unit = "cm", dpi = 300)

  rm(p2, p2a, p2b, p2_text)
  rm(res_CA, coord_lab)

### ╠ FIG 4.3: Distribución espacial de grupos con CA ----------------------------------------------------

  cli::cli_li(col_yellow("Figure 4.3: Distribución espacial de grupos con CA"))

  p3_AMBA <- CARTO$RADIO |>
    left_join(db_aux1[c("ID_RADIO", "color")], by = "ID_RADIO") |>
    PhD_ggzoom(dpt = CARTO$DPTO_AMBA, variable = "color", #lim = 0.11,
               tit = "Área Metropolitana Gran Buenos Aires") |>
    list() |>
    c(map(AUX$lim_DETAILS, PhD_geom_detail, color_cuad = "grey20")) |>
    reduce(`+`) +
    PhD_geom_detail_text(AUX$lim_DETAILS$ZN1, color_lab = "grey20",
                     lab_x = 3, hjust = 1, label = "Detalle 1") +
    PhD_geom_detail_text(AUX$lim_DETAILS$ZN2, label = "Detalle 2",
                     color_lab = "grey20") +
    PhD_geom_detail_text(AUX$lim_DETAILS$ZN3,
                     color_lab = "grey20", label = "Detalle 3") +
    PhD_geom_detail_text(AUX$lim_DETAILS$ZN4,
                     color_lab = "grey20", label = "Detalle 4") +
    coord_sf(xlim = AUX$lim_AMBA[c(1,3)],
             ylim = AUX$lim_AMBA[c(2,4)]) +
    annotation_custom(ggtern::ggplotGrob(PhD_ggtern_legend()),
                      xmin = AUX$lim_AMBA[1],
                      xmax = AUX$lim_AMBA[1] +  (AUX$lim_AMBA[3] - AUX$lim_AMBA[1])/2.5,
                      ymin = AUX$lim_AMBA[2] - (AUX$lim_AMBA[4] - AUX$lim_AMBA[2])/4,
                      ymax = AUX$lim_AMBA[2] + (AUX$lim_AMBA[4] - AUX$lim_AMBA[2])/2)

  p3_DET <- list(AUX$ZN |> map(~left_join(.x, db_aux1[c("ID_RADIO", "color")], by = "ID_RADIO")),
                 c("Detalle 1: CABA y Zona Norte\n(San Isidro y Vte. López)",
                   "Detalle 2: Expansión norte\n(Pilar, Escobar y Tigre)",
                   "Detalle 3: Cascos urbanos ferroviarios \n(Ej: FFCC Sarmiento)",
                   "Detalle 4: Periferia deteriorada\n(Ej: R. Calzada, Solano, Gdor. Costa)" ),
                 AUX$lim_DETAILS) |>
    pmap(PhD_ggzoom, variable = "color"#, lim = 0.11
         ) |>
    set_names(letters[1:4])

  # Agregar FFCC en Zona 3 (Morón)
  p3_DET$c <- p3_DET$c +
    geom_sf(data = FFCC1, color = "cornsilk4") +
    geom_sf(data = FFCC2, shape = 21, color = "cornsilk", fill = "coral") +
    geom_sf_text(data = FFCC2, aes(label = nombre), nudge_y = 150,
                 color = "cornsilk", size = 3) +
    coord_sf(xlim = AUX$lim_DETAILS$ZN3[c(1,3)],
             ylim = AUX$lim_DETAILS$ZN3[c(2,4)])

  p3_d1 <- ggarrange(p3_DET$a, p3_DET$c, common.legend = T, legend = "none",
                     nrow = 2, heights = c(6, 5))
  p3_d2 <- ggarrange(p3_DET$b, p3_DET$d, common.legend = T, legend = "none",
                     nrow = 2, heights = c(5, 6))

  p_blank <- ggplot() + theme_void()

  p3_d <- ggarrange(p3_d1, p_blank, p3_d2, common.legend = F,
                    nrow = 1, widths = c(5, 0.2, 5) )

  # Versión vertical
  p3 <- ggarrange(p3_AMBA, p_blank, p3_d, common.legend = F,
                  ncol = 1, heights = c(10, 0.2, 9))

  cli::cli_alert_info(col_silver("Guardando {.path figures/Segregacion_CA_AMBA.png}"))
  ggsave(fs::path("figures", "Segregacion_CA_AMBA.png"),
         plot = p3,
         width = 26, height = 42, unit = "cm", dpi = 300)

  rm(p3, p3_AMBA, p3_DET, p3_d, p3_d1, p3_d2, p_blank)
  rm(FFCC1, FFCC2)

  # Visualización ternaria
  # db_aux1 |>
  #   select(PRI, SEC, SUP) |>
  #   ggtern(aes(x = PRI, y = SEC, z = SUP)) +
  #   geom_point(alpha = 0.01) +
  #   stat_density_tern(geom = 'polygon',
  #                     n         = 1000,
  #                     aes(fill  = ..level..,
  #                         alpha = ..level..)) +
  #   scale_fill_viridis_c(option = "A")  +
  #   guides(fill = guide_colorbar(order = 1), alpha = guide_none()) +
  #   theme_ggtern()
  #
  # a <- db_aux1 |>
  #   summarise(across(PRI:SUP, ~sum(.x, na.rm = T) )) |>
  #   mutate(TOT = PRI + SEC + SUP,
  #          across(PRI:SUP, ~.x/TOT * 100 ) )
  #
  # PhD_ggtern_legend() +
  #   geom_point(data = a, color = "black") +
  #   geom_point(data = db_aux1, alpha = 0.01, color = "grey50")

### ╠ FIG App B.1: Comparación distancia X^2 y Ls ------------

  cli::cli_li(col_yellow("Figure B.1: Comparación distancia X^2 y Local Segregation (M)"))

  tot <- db_aux1 |>
    summarise(across(c(HOG,PRI:SUP), sum)) |>
    mutate(across(PRI:SUP, ~.x/HOG)) |>
    select(PRI:SUP)

  perfil <- db_aux1 |>
    mutate(across(PRI:SUP, ~.x/HOG)) |>
    select(PRI:SUP)

  dist_chi <- map2_df(perfil, tot, ~(.x -.y)^2 / .y) |>
    reduce(`+`) |>
    sqrt()
  dist_euc <- map2_df(perfil, tot, ~(.x -.y)^2) |>
    reduce(`+`) |>
    sqrt()

  db_res <- db_aux1 |>
    select(ID_RADIO, IDPROV, IDDPTO, TH,
           HOG, ls, PRI:SUP) |>
    mutate(dist_chi = dist_chi,
           dist_euc = dist_euc#,
           # dist_eig = dist_chi^2 * HOG/sum(HOG)
    )

  p_comp <- list(
    # p1 = db_res |>
    #   ggplot(aes(dist_euc, dist_chi)) +
    #   geom_point(alpha = 0.1, color = "tomato4") +
    #   geom_smooth(se = F, color = "darkblue", alpha = 0.2, linewidth = 0.5) +
    #   geom_abline(slope = 1, intercept = 0, color = "grey20", linetype = "dashed") +
    #   labs(title = "Distancia euclidiana vs Distancia Chi Cuadrada",
    #        x = "Distancia euclidiana",
    #        y = "Distancia Chi Cuadrada") +
    #   # coord_equal() +
    #   theme_minimal(),
    p2 = db_res |>
      ggplot(aes(ls, dist_chi)) +
      geom_point(alpha = 0.1, color = "tomato4") +
      geom_smooth(se = F, color = "darkblue", alpha = 0.2, linewidth = 0.5) +
      geom_abline(slope = 1, intercept = 0, color = "grey20", linetype = "dashed") +
      labs(title = "Segregación local (M) vs Distancia Chi Cuadrada",
           x = "Segregación local (M)",
           y = "Distancia Chi Cuadrada") +
      # coord_equal() +
      theme_minimal(),
    p3 = db_res |>
      ggplot(aes(ls, dist_chi^2)) +
      geom_point(alpha = 0.1, color = "tomato4") +
      geom_smooth(se = F, color = "darkblue", alpha = 0.2, linewidth = 0.5) +
      geom_abline(slope = 1, intercept = 0, color = "grey20", linetype = "dashed") +
      labs(title = "Segregación local (M) vs Distancia Chi Cuadrada",
           subtitle = "Chi cuadrado con transformación cuadrática",
           x = "Segregación local (M)",
           y = "Distancia Chi Cuadrada (transformación cuadrática)") +
      # coord_equal() +
      theme_minimal()
    # p3 = db_res |>
    #   ggplot(aes(ls, dist_eig)) +
    #   geom_point(alpha = 0.1, color = "tomato4") +
    #   geom_smooth(se = F, color = "darkblue", alpha = 0.2, linewidth = 0.5) +
    #   geom_abline(slope = 1, intercept = 0, color = "grey20", linetype = "dashed") +
    #   labs(title = "Segregación local (M) vs Inercia",
    #        x = "Segregación local (M)",
    #        y = TeX("Inercia = masa . (chi dist)^2")) +
    #   # coord_equal() +
    #   theme_minimal()
  )

  p4 <- ggarrange(plotlist = p_comp, nrow = 1)

  cli::cli_alert_info(col_silver("Guardando {.path figures/Segregacion_comparacion_distancias.png}"))
  ggsave(plot = p4, filename = fs::path("figures", "Segregacion_comparacion_distancias.png"),
         width = 25, height = 14, units = "cm", dpi = 300)

  rm(db_aux1)
  rm(db_res, dist_chi, dist_euc, perfil, tot, p_comp, p4)

# Tablas  ------------------------------------

### ╠ TAB 4.1 --------------------------------------

  cli::cli_li(col_yellow("Table 4.1: Segregación residencial explicada x nivel de análisis"))

  RES$TAB1 <- RES$SEG2[-1] |>
    as_tibble(rownames = "TH") |>
    mutate(TH = str_replace(TH, "_", " ")) |>
    flextable::flextable() |>
    flextable::set_header_labels(TH = " ",
                                 WITHIN = "Dentro",
                                 BETWEEN = "Entre") |>
    flextable::set_caption(caption = paste0("Segregación residencial explicada por ",
                                            "diferencias dentro/entre diferentes niveles ",
                                            "de análisis (Jurisdicciones, Departamentos y ",
                                            "Entornos urbanos)"),
                           autonum = officer::run_autonum(seq_id = "tab",
                                                          bkm = "segres-tab1",
                                                 bkm_all = T))  |>
    flextable::align(align ='center', part = 'all') %>%
    flextable::align(j = 1, align ='left', part = 'all') %>%
    flextable::colformat_double(digits = 1) %>%
    flextable::add_footer_lines(values = as_paragraph(
      paste0("Los valores tienen corrección de sesgo. ",# AUX$replic,
             " Bootstrap replicaciones."))) |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 7, part = "all") |>
    flextable::autofit(part = "all")

### ╠ TAB 4.2 ----------------------

  cli::cli_li(col_yellow("Table 4.2: Segregación residencial explicada x nivel educativo"))

  RES$TAB2 <- RES$SEG3[ , c("name", "ls", "p")] |>
    mutate(name = factor(name, levels = c("PRI", "SEC", "SUP"),
                         labels = c("Primario", "Secundario", "Superior")),
           p = paste0(round(p*100,1), "%")) |>
    flextable::flextable() |>
    flextable::set_header_labels(name = "Nivel",
                                 ls = "Segregación local",
                                 p = "Porcentaje") |>
    flextable::set_caption(caption = "Segregación residencial explicada por cada nivel educativo",
                           autonum = officer::run_autonum(seq_id = "tab",
                                                          bkm = "segres-tab2",
                                                          bkm_all = T))  |>
    flextable::align(align ='center', part = 'all') |>
    flextable::align(j = 1, align ='left', part = 'all') |>
    flextable::colformat_double(digits = 3) |>
    flextable::add_footer_lines(values = flextable::as_paragraph(
      "Los porcentajes se calcularon sin aplicar corrección de sesgo. ")) |>
      # paste0("Los valores tienen corrección de sesgo. ", AUX$replic,
      #        " Bootstrap replicaciones."))) |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 7, part = "all") |>
    flextable::autofit(part = "all")

### ╠ TAB 4.3 ----------------------

  cli::cli_li(col_yellow("Table 4.3: Descripción de patrones"))

  RES$TAB3 <- readxl::read_xlsx(fs::path("figures", "patrones.xlsx")) |>
    flextable::flextable() |>
    flextable::set_caption(caption = paste("Descripción de las principales",
                                           "características de los patrones de",
                                           "segregación"),
                           autonum = officer::run_autonum(seq_id = "tab",
                                                          bkm = "segres-tab3",
                                                          bkm_all = T))  |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 7, part = "all") |>
    flextable::align(align ='left', part = 'all') |>
    flextable::valign(valign = "top", part = "all") |>
    # flextable::autofit(part = "all")
    flextable::width(j = 1, 2.3, unit = "cm") |>
    flextable::width(j = 2, 2.3, unit = "cm") |>
    flextable::width(j = 3:6, 1.9, unit = "cm") |>
    flextable::width(j = 7, 4.6, unit = "cm") |>
    flextable::width(j = 8, 3.2, unit = "cm")

### ╠ TAB App A.1 --------------------------

  cli::cli_li(col_yellow("Table App A: Índices de segregación"))

  RES$appA_TAB1 <- RES$SEG1 |>
    flextable() |>
    flextable::set_header_labels(Index = "Indice") |>
    flextable::set_caption(caption = paste0("Índices de segregación residencial"),
                           autonum = officer::run_autonum(seq_id = "tab",
                                                          bkm = "app1-tab1",
                                                          bkm_all = T))  |>
    flextable::align(align ='center', part = 'all') |>
    flextable::align(j = 1, align ='left', part = 'all') |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 7, part = "all") |>
    flextable::autofit()

### ╚ TAB App A.2 --------------------------

  cli::cli_li(col_yellow("Table App A: Descripción de Entornos urbano"))

  RES$appA_TAB2 <- RES$SEG4 |>
    relocate(M, .after = TH) |>
    mutate(p = p * 100) |>
    flextable::flextable() |>
    flextable::set_header_labels(TH = "Entorno urbano",
                                 M = "Segregación local (M)",
                                 PRI = "Hasta primario completo",
                                 SEC = "Secundario completo",
                                 SUP = "Superior completo",
                                 p = "Porcentaje del total AMBA") |>
    add_header_row(values = c("Entorno urbano",
                              "Segregación local (M)",
                              "Composición interna (%)",
                              "Composición interna (%)",
                              "Composición interna (%)",
                              "Porcentaje del total AMBA")) |>
    merge_h(part = "header") |>
    merge_v(part = "header") |>
    flextable::set_caption(caption = paste0("Descripción de Entornos urbano"),
                           autonum = officer::run_autonum(seq_id = "tab",
                                                          bkm = "app1-tab2",
                                                          bkm_all = T))  |>
    flextable::colformat_double(j = 2, digits = 3) |>
    flextable::colformat_double(j = 3:5, digits = 1, suffix = "%") |>
    flextable::colformat_double(j = 6, digits = 1, suffix = "%") |>
    flextable::colformat_double(i = 10, j = 6, digits = 0, big.mark = " ") |>
    flextable::color(j = 3:5, color = "grey", part = "body") |>
    flextable::hline(i = 9) |>
    flextable::bold(i = 10) |>
    flextable::align(align ='center', part = 'all') |>
    flextable::align(j = 1, align ='left', part = 'all') |>
    flextable::valign(valign = "top", part = "body") |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 7, part = "all") |>
    # flextable::autofit(part = "all")
    flextable::width(j = 1, 4.5, unit = "cm") |>
    flextable::width(j = 2, 2.2, unit = "cm") |>
    flextable::width(j = 3:6, 2.2, unit = "cm")

# Guardar resultados ----------------------------------------------------

  usethis::use_data(RES, overwrite = TRUE)
