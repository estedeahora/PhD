# Homogamy

cli::cli_h2(col_blue("Análisis de Homogamia (HOM)"))

HOM <- list()

# Cargar datos --------------------------------------

if(!exists("CARTO", inherits = F)){
  f <- here::here("data-workflow", "final-data", "CARTO.rda")
  cli_alert_info(
    col_silver("Cargando cartografía (`CARTO`): {.path {fs::path_rel(f)}}")
  )

  load(file = f)
  rm(f)
}

if(!exists("AUX", inherits = F)){
  f <- here::here("data-workflow", "final-data", "AUX-data.rda")

  cli_alert_info(
    col_silver("Cargando datos auxiliares (`AUX`): {.path {fs::path_rel(f)}}")
  )

  load(file = f)
  rm(f)
}

if(!exists("HOG", inherits = F)){
  f <- here::here("data-workflow", "final-data", "HOG.Rda")

  cli_alert_info(
    col_silver("Cargando datos de hogares (`HOG`): {.path {fs::path_rel(f)}}")
  )

  load(file = f)
  rm(f)
}

# Preparar datos --------------------------------------------------------

  cli::cli_li(col_yellow("Crear bases de datos auxiliares"))

  limit <- 30
  replic <- CARTO$RADIO$PRI_p3 |> attributes() |> pluck("montecarlo") |> pluck("replic")
  conf <-  CARTO$RADIO$PRI_p3 |> attributes() |> pluck("montecarlo") |> pluck("conf")

  db_aux <- CARTO$RADIO |>
    st_drop_geometry() |>
    filter(!is.na(TH) & !is.na(HOG_HOM)) |>
    select(ID_RADIO, TH, ls,
           starts_with("HOG"),
           ends_with("_p1"),
           ends_with("_p3"),
           ends_with("_IC"),
           ends_with("_sig"),
           ends_with("_MEAN"),
           ends_with("_SD")) |>
    unnest(ends_with("_IC")) |>
    mutate(across(HOG_PRI:HOG_SUP,
                  ~ifelse(.x != 0, .x/HOG, 0) )) |>
    select(ID_RADIO, TH, ls,
           starts_with("HOG"),
           -HOG_color,
           starts_with("PRI"),
           starts_with("SEC"),
           starts_with("SUP")) |>
    rename_with(.cols = starts_with("HOG_"),
                .fn = ~ str_c(str_remove(.x, "HOG_"), "_HOG"))

  db_aux1 <- db_aux |>
    pivot_longer(cols = c(PRI_p1, SEC_p1, SUP_p1,
                          PRI_HOG, SEC_HOG, SUP_HOG,
                          PRI_MEAN, SEC_MEAN, SUP_MEAN,
                          PRI_sig, SEC_sig, SUP_sig) ) |>
    mutate(tipo = case_when(str_ends(name, "_p1") ~ "Probabilidad",
                            str_ends(name, "_HOG") ~ "Proporcion",
                            str_ends(name, "_MEAN") ~ "H0",
                            str_ends(name, "_sig") ~ "Significatividad"),
           g = str_sub(name, 1, 3) ) |>
    pivot_wider(id_cols = c(ID_RADIO, TH, HOG, g),
                names_from = tipo,
                values_from = value) |>
    mutate(g = factor(g,
                      levels = c("PRI", "SEC", "SUP"),
                      labels = c("Primario", "Secundario", "Superior") ),
           Significatividad = as.logical(Significatividad),
           w = HOG * Proporcion,
           # Corregir variables sin casos (Proporción = 0)
           Probabilidad = ifelse(Proporcion == 0, 0, Probabilidad),
           H0 = ifelse(Proporcion == 0, 0, H0),
           Significatividad = ifelse(Proporcion == 0, F, Significatividad),
           # Imputar valor esperado cuando no existe significatividad
           Probabilidad_corr = ifelse(Significatividad, Probabilidad, H0),
           # Calcular diferencias
           # dif_prop = Probabilidad - Proporcion,
           dif_H0 = Probabilidad - H0,
           dif_H0_cor = Probabilidad_corr - H0,
           # Calcular ratios
           # Ratio1 =  Proporcion / Probabilidad,
           # Ratio2 =  Proporcion / Probabilidad_corr,
           # Ratio4 = H0 / Probabilidad_corr
           )

  db_aux2 <- CARTO$RADIO |>
    left_join(db_aux1 |>
                filter(!is.na(Significatividad)) |>
                select(ID_RADIO, g, dif_H0_cor),
              by = join_by(ID_RADIO))

# Análisis de Homogamia -------------------------------
### ╠ Porcentaje de viviendas con parejas homogámicas ------------------
  # Porcentaje de homogamia y distancia educativa promedio sobre
  #   el total de parejas (no incluye hogares no-matrimoniales)

  cli::cli_li(col_yellow("Calcular indicadores globales de homogamia (HOM1)."))

  HOM$HOM1 <- rbind(HOG |> PhD_homog_summarise(TH),
                    HOG |> PhD_homog_summarise(PROV),
                    HOG |> PhD_homog_summarise("TOTAL") )

  # CARTO$RADIO |>
  #   st_drop_geometry() |>
  #   filter(!is.na(TH)) |>
  #   group_by(TH) |>
  #   summarise(across(ends_with("_sig"),
  #                    ~sum(!.x, na.rm = T) / n() * 100) )

### ╠ Tabla matrimonial (sin distinguir género) ------------------------------------
  # No diferencia Varón y Mujer
  #   (P1 y P2 son ordenados en función del que tenga mayor Nivel educativo)

  cli::cli_li(col_yellow("Generar tabla matrimonial global (HOM2)."))

  HOM$HOM2 <-  HOG |>
    mutate(P1 = ifelse(is.na(P1), EDU, P1 )) |>
    count(P1, P2)

  # Porcentajes de combinación P1-P2
  # HOM$HOM2 |>
  #   mutate(n = round(n /sum(n) * 100, 1)) |>
  #   pivot_wider(id_cols = P1,
  #               names_from = P2,
  #               values_from = n)

### ╠ Análisis Chi.square -------------------------------------------------

  cli::cli_li(col_yellow("Calcular Chi-square de tabla de homogamia (HOM3)."))

  tabla_mat <- HOM$HOM2 |>
    filter(!is.na(P2)) |>
    pivot_wider(id_cols = P1,
                names_from = P2,
                values_from = n, values_fill = 0) |>
    select(-1) |>
    as.matrix()

  # Distribuimos el triángulo superior en mitades en el triángulo superior e inferior
  tabla_mat[lower.tri(tabla_mat)] <- tabla_mat[upper.tri(tabla_mat)] <- t(tabla_mat)[lower.tri(tabla_mat)] / 2

  # Chi-Square test
  HOM$HOM3 <- chisq.test(tabla_mat,
                         simulate.p.value = T,
                         B = 10000)
  rm(tabla_mat)

### ╚ Porcentaje de Radios con Probabilidad de homogamia no-significativa -----------

  cli::cli_li(col_yellow(paste0("Calcular porcentaje de radios con probabilidad",
                                "de homogamia no-significativa (HOM4).")))

  HOM$HOM4 <- CARTO$RADIO |>
    st_drop_geometry() |>
    filter(!is.na(TH) & !is.na(PRI_p1)) |>
    # group_by(TH) |>
    summarise(across(contains("_sig_"), #c(PRI_sig, SEC_sig, SUP_sig),
                     ~sum(.x, na.rm = T) * 100 / sum(!is.na(.x)  ) ) ) |>
    pivot_longer(cols = contains("_sig_")) |>
    mutate(nivel = str_sub(name, 1, 3),
           tipo = str_sub(name, 9, -1)) |>
    pivot_wider(id_cols = nivel, names_from = tipo, values_from = value)  |>
    mutate(signific = down + up,
           no_signific = 100 - signific)

# Figuras ---------------------------------

### ╠ FIG 5.2: Density plot de probabilidad de homogamia por entorno urbano ----------

  cli::cli_li(col_yellow("Figure 5.2: Densidad de la probabilidad de homogamia."))

  p2_TOT <- CARTO$RADIO |>
    filter(!is.na(TH)) |>
    st_drop_geometry() |>
    select(ID_RADIO, TH, ends_with("_p1")) |>
    pivot_longer(cols =  ends_with("_p1")) |>
    ggplot(aes(x = value, color = name)) +
    geom_density() +
    labs(title = "Área Metropolitana Gran Buenos Aires",
         x = "Probabilidad de homogamia", y = "", color = "Nivel educativo") +
    scale_color_manual(values = c("tomato3", "goldenrod1", "darkolivegreen4"),
                       labels = c("Primario", "Secundario", "Superior")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

  p2_TAB <- CARTO$RADIO |>
    st_drop_geometry() |>
    filter(!is.na(TH)) |>
    summarise(across(PRI_p1:SUP_p1,
                     list(Media   = ~mean(.x, na.rm = T),
                          Mediana = ~median(.x, na.rm = T),
                          Desvío  = ~sd(.x, na.rm = T)))) |>
    pivot_longer(cols = everything()) |>
    mutate(Nivel = str_sub(name, 1, 3),
           Nivel = factor(Nivel, levels = c("PRI", "SEC", "SUP"),
                          labels = c("Primario", "Secundario", "Superior")),
           name = str_sub(name, 8, -1),
           value = round(value, 3)) |>
    pivot_wider(id_cols = Nivel, names_from = name, values_from = value) |>
    rename("Probabilidad\nde homogamia" = Nivel ) |>
    ggtexttable(rows = NULL,
                theme = ttheme("blank",
                               tbody.style = tbody_style(hjust = 0, x = 0.1)) ) |>
    tab_add_hline(at.row = c(1:2), row.side = "top", linewidth = 2) |>
    tab_add_hline(at.row = 4, row.side = "bottom", linewidth = 2) |>
    table_cell_font(row = 2, column = 1, color =  "tomato3") |>
    table_cell_font(row = 3, column = 1, color =  "goldenrod1") |>
    table_cell_font(row = 4, column = 1, color =  "darkolivegreen4")

  HOM_aux <- HOM$HOM1[1:9, ] |>
    select(TH = E, PRI_p1:SUP_p1) |>
    pivot_longer(cols = PRI_p1:SUP_p1) #|>
    # mutate(name = paste0(name, 1))

  p2_TH <- p2_TOT  +
    geom_vline(data = HOM_aux,
               mapping = aes(xintercept = value, color = name),
               alpha = .6, linetype = 2) +
    facet_wrap(~TH) +
    guides(color = "none")+
    labs(title = NULL)

  p_blank <- ggplot() + theme_minimal()

  p2 <- ggarrange(ggarrange(p_blank,
                            p2_TOT + scale_x_continuous("") + guides(color = "none"),
                            p_blank,
                            p2_TAB, p_blank,
                            nrow = 1, widths = c(1.5, 6, 1, 5, 2)),
                  p2_TH,
                  nrow = 2, heights = c(4, 8) )

  cli::cli_alert_info(col_silver("Guardando {.path figures/Homogamia_probabilidad-densidad-x-TH.png}"))
  ggsave(filename = here::here("figures",
                               "Homogamia_probabilidad-densidad-x-TH.png"),
         plot = p2, height = 18, width = 22, units = "cm", dpi = 300)

  rm(HOM_aux, p2, p2_TOT, p2_TAB, p2_TH, p_blank)

### ╠ FIG 5.3: Distribución espacial de la probabilidad de homogamia (por tipo) ----------------
  ### Local analysis (spatialized analysis)

  cli::cli_li(col_yellow(paste0("Figure 5.3: Distribución espacial de ",
                                "la probabilidad de homogamia.")))

  p3_vln <- CARTO$RADIO |>
    filter(!is.na(TH)) |>
    st_drop_geometry() |>
    select(ID_RADIO, ends_with("_p3")) |>
    pivot_longer(cols =  ends_with("_p3")) |>
    mutate(name = factor(name,
                         levels = paste0(c("PRI", "SEC", "SUP"), "_p3"),
                         labels = paste("Homogamia de Nivel",
                                        c("Primario", "Secundario", "Superior") ) ) ) |>
    filter(!is.na(value)) |>
    ggplot(aes(y = 1, x = value)) +
    geom_boxplot(width = 0.3) +
    geom_violin(aes(fill = name), color = NA, alpha = 0.3) +
    facet_wrap(~name, strip.position = "top") +
    labs(y = "", x = "") +
    guides(fill = "none", y = "none") +
    scale_fill_manual(values = c("tomato3", "goldenrod1", "darkolivegreen4")) +
    theme_light() +
    theme(strip.text.x = element_text(size = 16),
          panel.background = element_rect(fill = 'transparent'),
          plot.background = element_rect(fill='transparent', color=NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  p3_AMBA <- CARTO$RADIO |>
    filter(!is.na(HOG_HOM) & !is.na(TH)) |>
    pivot_longer(cols = c("PRI_p3", "SEC_p3", "SUP_p3"),
                 names_to = "variable", values_to = "value") |>
    mutate(value = ifelse(HOG_CON >= limit, value, NA) ) |>
    ggplot() +
    geom_sf(data = CARTO$AMGBA, aes(size = ""),
            fill = NA, alpha = 0.4) +
    geom_sf(aes(fill  = value), linewidth = 0.01, color = NA) +
    geom_sf(data = CARTO$DPTO, color = "grey80", fill = NA) +
    scale_fill_distiller("Probabilidad de\nhomogamia",
                         palette = "RdPu", direction = 1,
                         na.value = "grey70") +
    facet_wrap(~variable, strip.position = "bottom") +
    coord_sf(xlim = AUX$lim_AMBA[c(1,3)],
             ylim = AUX$lim_AMBA[c(2,4)]) +
    guides(size = guide_legend("No significativo", order = 2,
                               override.aes = list(shape = 15, size = 15, fill = "grey70")),
           fill = guide_colorbar(order = 1)) +
    theme_void() +
    theme(legend.position = "bottom",
          strip.text = element_blank(),
          legend.key.height = unit(1.2, 'cm'),
          legend.key.width = unit(1.8, 'cm'),
          legend.title = element_text(size = 16)
    )

  p3 <- ggarrange(p3_vln, p3_AMBA,
                  nrow = 2,  heights = c(2, 7),
                  common.legend = T, legend = "bottom")

  cli::cli_alert_info(col_silver("Guardando {.path figures/Homogamia_probabilidad-x-tipo.png}"))
  ggsave(filename = here::here("figures", "Homogamia_probabilidad-x-tipo.png"),
         plot = p3, height = 18 * 1.5, width = 38 * 1.5, units = "cm", dpi = 300)

  rm(p3_vln, p3_AMBA, p3)

### ╠ FIG 5.4: Ratio entre proporción y probabilidad de homogamia -----------

  cli::cli_li(col_yellow("Figure 5.4: Ratio probabilidad y proporción de homogamia"))

  aux_p4 <- pivot_longer((RES$SEG4[10, 2:4]/100), cols = everything(),
               names_to = "g", values_to = "intercept") |>
    mutate(g = factor(g,
                      levels = c("PRI", "SEC", "SUP"),
                      labels = c("Primario", "Secundario", "Superior") ) )

  p_blank <- ggplot() + theme_minimal()

  p4_center <- db_aux1 |>
    filter(!is.na(Significatividad) & Proporcion > 0.03) |>
    ggplot(aes(x = Proporcion, y = Probabilidad_corr, color = g, group = g )) +
    geom_point(aes(y = Probabilidad, shape = Significatividad, size = w),
               alpha = 0.03) +
    geom_segment(data = aux_p4,
                 aes(x = intercept, xend = intercept, color = g),
                 y = 0.75, yend = 0.6, alpha = 0.6, linetype = 1,
                 arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    geom_text(x = 0.33, y = 0.78, color = "grey20", size = 3,
              label = "Proporción promedio de cada grupo") +
    geom_text(data = aux_p4, angle = 90,  size = 3,
              aes(x = intercept, y = 0.7, label = g),
              color = "grey20", vjust = -0.05) +
    geom_smooth(aes(weight = w), linewidth = 0.4) +
    geom_smooth(aes(y = H0, weight = w), se = F,
                linewidth = 0.4, linetype = 2) +
    scale_x_continuous(name = "Proporción del grupo en la unidad censal",
                       limits = c(0, 1), expand = c(0,0)) +
    scale_y_continuous(name = "Probabilidad de homogamia",
                       limits = c(0, 0.8), expand = c(0,0) ) +
    scale_color_manual(values = c("tomato3", "goldenrod1", "darkolivegreen4")) +
    scale_shape_manual(values = c(4, 16)) +
    coord_fixed() +
    theme_minimal() +
    theme(legend.position = "none")

  p4_marg <- ggExtra::ggMarginal(p4_center, type = "density",
                                 groupColour = TRUE, groupFill = TRUE) |>
    ggpubr::as_ggplot()

  p4_leyenda <- list(
    p1 = ggplot(data.frame()) +
      geom_point(x = NA, y = NA,
                 aes(color = c("Primario", "Secundario", "Superior")) ) +
      scale_color_manual(values = c("tomato3", "goldenrod1", "darkolivegreen4")) +
      labs(color = "Nivel") +
      theme_minimal() ,
    p2 = ggplot(data.frame()) +
        geom_smooth(aes(x = 1:6, y = 1:6,
                        linetype = factor(rep(c(1:2), 3))),
                    color = "black", se = FALSE) +
        scale_linetype_discrete(name="",
                                breaks=c(1, 2),
                                labels = c("Observado", "Hipótesis nula (H0)")) +
        theme_minimal()
    ) |>
    map(~(.x  +  theme(legend.position = "bottom")) |>  ggpubr::get_legend() |>  ggpubr::as_ggplot())

  p4_leyenda <- ggarrange(plotlist = list(a_ = p_blank, b_ = p4_leyenda[[1]],
                                          c_ = p4_leyenda[[2]], d_ = p_blank),
                          ncol = 4, widths = c(3, 6, 6, 3))

  p4_ppal <- ggarrange(p4_marg, p4_leyenda,
                       ncol = 1, heights = c(10,0.5))

  p4_add1 <- db_aux1 |>
    filter(!is.na(Significatividad) & Proporcion > 0.03) |>
    ggplot(aes(x = Proporcion, y = dif_H0_cor, color = g, weight = w)) +
    geom_smooth(se = F, linewidth = 0.4) +
    geom_smooth(se = F, linewidth = 0.4, color = "grey60", linetype = 2,
                method = 'glm', formula = y ~ poly(x, 3)) +
    facet_wrap(~"Total", ncol = 1, strip.position = "right") +
    scale_color_manual(values = c("tomato3", "goldenrod1", "darkolivegreen4")) +
    scale_x_continuous(name = NULL, limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(name = "", limits = c(0, 1), expand = c(0, 0),
                       labels = scales::percent) +
    theme_light() +
    theme(panel.background = element_rect(fill = 'transparent'),
          plot.background = element_rect(fill='transparent', color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="lightblue4"),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none")

  p4_add2 <- db_aux1 |>
    mutate(g_col = case_when(Significatividad ~ g,
                             T~ "No Significativo"),
           g_col = factor(g_col, levels = c(levels(g), "No Significativo"))) |>
    filter(!is.na(Significatividad) & (Proporcion > 0.03)) |>
    ggplot(aes(x = Proporcion, y = dif_H0_cor, color = g_col, weight = w)) +
    geom_point(alpha = 0.02, aes(y = dif_H0, shape = Significatividad)) +
    geom_smooth(aes(group = g),
                color = "grey25", se = T, linewidth = 0.4) +
    geom_hline(yintercept = 0, linetype = 2, color = "grey10", linewidth = 0.4) +
    facet_wrap(~g, ncol = 1, strip.position="right") +
    scale_color_manual(values = c("tomato3", "goldenrod1", "darkolivegreen4", "grey50")) +
    scale_shape_manual(values = c(4, 16)) +
    scale_x_continuous(name = "Proporción del grupo \nen la unidad espacial",
                       limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(name = "Porcentaje de homogamia no-explicada",
                       limits = c(NA, 1), expand = c(0, 0),
                       labels = scales::percent) +
    theme_light() +
    theme(strip.text.x = element_text(size = 8),
          panel.background = element_rect(fill = 'transparent'),
          plot.background = element_rect(fill='transparent', color = NA),
          strip.background = element_rect(fill="lemonchiffon4"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")

  ggarrange(p4_add1, p4_add2, ncol = 1, heights = c(1, 4))

  p4 <- ggarrange(p4_ppal,
                  ggarrange(p4_add1, p4_add2, p_blank,
                            ncol = 1, heights = c(1, 4, 0.35)),
                  widths = c(3, 1.2))

  cli::cli_alert_info(col_silver("Guardando {.path figures/Homogamia_por-vs-prob.png}"))
  ggsave(here::here("figures",
                    "Homogamia_por-vs-prob.png"),
         p4, width = 27.5, height = 22, units = "cm", dpi = 300)

  rm(p4, p4_ppal, p4_center, p4_marg, p4_add1, p4_add2,
     p4_leyenda, p_blank, aux_p4)

### ╠ FIG 5.5: Composición (AC) y Probabilidad de homogamia no-explicada ------------------------------------------

  cli::cli_li(col_yellow("Figure 5.5: Composición (AC) y Probabilidad de homogamia no-explicada"))

  res_CA <- db_aux |>
    rename_with(~str_remove(.x, "_HOG")) |>
    mutate(across(PRI:SUP, ~.x * HOG)) |>
    select(PRI, SEC, SUP) |>
    FactoMineR::CA(graph = F)

  coord_lab <- res_CA$col$coord |>
    as_tibble(rownames = "lab") |>
    mutate(`Dim 1` = `Dim 1` * 1/sqrt(res_CA$eig[1, 1]),
           `Dim 2` = `Dim 2` * 1/sqrt(res_CA$eig[2, 1]))

  coord_rad <- left_join(db_aux1,
                         bind_cols(ID_RADIO = db_aux$ID_RADIO,
                                   res_CA$row$coord),
                         by = "ID_RADIO") |>
    mutate(size_adapt = (HOG - 10) * Significatividad + 10)

  p5 <- res_CA |>
    factoextra::fviz_ca_biplot(alpha.row = 0, alpha.col = 0,
                               map = "rowprincipal",
                               geom = "point", title = NULL) +
    geom_polygon(data = coord_lab, col = 'grey60', fill = NA,
                 linetype = 2, linewidth = 0.5,
                 aes(x = `Dim 1`, y = `Dim 2`)) +
    geom_point(data = coord_rad, alpha = 0.5,
               aes(x = `Dim 1`, y = `Dim 2`,
                   size = size_adapt,
                   color = dif_H0_cor, shape = Significatividad)) +
    geom_text(data = coord_lab, color = "grey10",
              aes(x = `Dim 1` + (0.05 * `Dim 1`),
                  y = `Dim 2` + (0.05 * `Dim 2`),
                  label = lab) ) +
    facet_wrap(~g) +
    labs(size = "Cantidad de hogares") +
    guides(shape = "none") +
    scale_shape_manual(values = c(4, 19)) +
    scale_color_gradient2("Probabilidad\nde Homogamia\nno explicada",
                          mid = "lightcyan2",
                          low = scales::muted("green"),
                          high = scales::muted("red"),
                          transform = "log1p") +
    theme(legend.direction = "horizontal", legend.position = "bottom")

  cli::cli_alert_info(col_silver("Guardando {.path figures/Homogamia_CA-homogamy.png}"))
  ggsave(filename = here::here("figures",
                               "Homogamia_CA-homogamy.png"),
         plot = p5, width = 30, height = 13, units = "cm", dpi = 300)

  rm(res_CA, coord_lab, coord_rad, p5)

### ╠ FIG App C.1: Relaciones entre P'(Ha|a) y P''(H|A) ---------------------------

  cli::cli_li(col_yellow("Figure C.1: Relaciones entre P'(Ha|a) y P''(H|A)"))

  l = 4000
  p_aux <- tibble(A = sample(x = 5:500, replace = T, size = l)) |>
    mutate(HA = A * runif(n = l, min = 0, max = 0.5),
           HA = round(HA) * 2) |>
    distinct() |>
    mutate(P_ind = HA/A,
           P_hog = P_ind / (2 - P_ind),
           ratio = P_ind / P_hog,
           ratio = ifelse(is.nan(ratio), NA, ratio) )

  p6a <- p_aux |>
    ggplot(aes(x = P_ind, y = P_hog, color = ratio)) +
    geom_point(alpha = 0.4) +
    scale_color_viridis_c(option = "B", direction = 1, limits = c(1,2)) +
    theme_minimal() +
    labs(x = latex2exp::TeX("$P\\prime(H_a|a)$"),
         y = latex2exp::TeX("$P\\prime(H_A|A)$"),
         color = latex2exp::TeX("$Ratio_{(P\\prime/P{\\prime\\prime})} = 2-P\\prime$")) +
    geom_text(x = 0.25, y = 0.75, parse = TRUE, size = 7, color = "black",
              label = latex2exp::TeX("$P{\\prime\\prime}(H_A|A) = \\frac{P\\prime(H_a|a)}{2-P\\prime(H_a|a)}$",
                                     output = "character"))+
    theme(legend.position = "bottom")

  p6b <- p_aux |>
    filter(P_hog != 0) |>
    ggplot(aes(x = P_ind, y = ratio, color = P_hog)) +
    geom_point() +
    labs(x = latex2exp::TeX("$P\\prime(H_a|a)$"),
         y = latex2exp::TeX("$Ratio_{(P\\prime/P{\\prime\\prime})}$"),
         color = latex2exp::TeX("$P{\\prime\\prime}(H_A|A)$")) +
    scale_color_viridis_c(option = "D", direction = 1) +
    theme_minimal() +
    theme(legend.position = "bottom")

  cli::cli_alert_info(col_silver("Guardando {.path figures/Homogamia_relation_P1-P2.png}"))
  ggsave(plot = ggpubr::ggarrange(p6a, p6b),
         filename = here::here("figures", "Homogamia_relation_P1-P2.png"),
         height = 20, width = 30, dpi = 300, units = "cm")

  rm(p6a, p6b)
  rm(p_aux, l)

### ╠ FIG App D.1: Probabilidad de homogamia bajo H_0 ---------------------------

  cli::cli_li(col_yellow("Figure D.1: Probabilidad de homogamia bajo H_0"))

  p7 <- db_aux |>
    # left_join(a, by = "ID_RADIO") |>
    pivot_longer(cols = c(PRI_MEAN, SEC_MEAN, SUP_MEAN,
                          # PRI_head, SEC_head, SUP_head,
                          PRI_HOG, SEC_HOG, SUP_HOG) ) |>
    mutate(tipo = ifelse(str_ends(name, "_MEAN"), "Probabilidad", "Proporción"),
           g = case_when(str_detect(name, "PRI") ~ "PRI",
                         str_detect(name, "SEC") ~ "SEC",
                         str_detect(name, "SUP") ~ "SUP")) |>
    pivot_wider(id_cols = c(ID_RADIO, TH, g),
                names_from = tipo, values_from = value) |>
    ggplot(aes(x = Proporción, y = Probabilidad, color = g)) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = F, linewidth = 0.5,
                method = "lm", formula = y ~ poly(x, 2)) +
    geom_abline(slope = 1, linetype = 2, alpha = 0.4, color = "grey30") +
    scale_x_continuous("Proporción del grupo en la unidad censal") +
    scale_y_continuous(unname(latex2exp::TeX( "Probabilidad de homogamia bajo $H_0$"))) +
    scale_color_manual(values = c("tomato3", "goldenrod1", "darkolivegreen4")) +
    theme_minimal() +
    coord_fixed() +
    guides(color = "none")

  cli::cli_alert_info(col_silver("Guardando {.path figures/Homogamia_porcentaje-vs-prob-H0.png}"))
  ggsave(here::here("figures", "Homogamia_porcentaje-vs-prob-H0.png"),
         plot = p7, width = 19, height = 19, units = "cm", dpi = 300)

  rm(p7)

### ╚ FIG App ?.?: Distribución espacial de prob homogamia no-explicada ------------------------------------------

  cli::cli_li(col_yellow("Figure D.1: Distribución espacial de prob homogamia no-explicada"))

  p8 <- db_aux2 |>
    filter(!is.na(g)) |>
    ggplot() +
    geom_sf(data = CARTO$AMGBA, fill = "white", alpha = 0.5) +
    geom_sf(aes(fill = dif_H0_cor), color = NA) +
    scale_fill_gradient2("% de Homogamia\nno-explicada",
                         mid = "lightcyan2",
                         low = scales::muted("green"),
                         high = scales::muted("red"),
                         transform = "log1p", limits = c(NA, 1)) +
    facet_wrap(~g, strip.position = "bottom") +
    coord_sf(xlim = AUX$lim_AMBA[c(1,3)],
             ylim = AUX$lim_AMBA[c(2,4)]) +
    theme_void() +
    theme(legend.position="bottom")

  cli::cli_alert_info(col_silver("Guardando {.path figures/Homogamia_homog-explicada.png}"))
  ggsave(filename = here::here("figures",
                               "Homogamia_homog-explicada.png"),
         plot = p8, height = 13, width = 30, units = "cm", dpi = 300)

  rm(p8)

### Tablas ------------------------------

  # ╠ TAB 5.1 ---------------------------------

  HOM$TAB1 <- HOM$HOM1[10:12, ] |>
    mutate(across(c(CON_p:POR_h_SUP), ~round(.x, 1)),
           across(c(PRI_p1:SUP_p1), ~round(.x, 3)),
           emp = NA) |>
    select(1:7, emp, 8:10) |>
    flextable::flextable() |>
    flextable::set_caption(caption = "Indicadores de homogamia educativa según jurisdiccion",
                           autonum = officer::run_autonum(seq_id = "tab",
                                                          bkm = "HOM-TAB1",
                                                          bkm_all = T))  |>
    flextable::add_header_row(values = c("Jurisdiccion", "Cantidad de hogares",
                                         "Porcentaje de hogares conyugales",
                                         "Porcentaje de homogamia",
                                         " ",
                                         "Probabilidad de homogamia"),
                              colwidths = c(1, 1, 1, 4, 1, 3)) |>
    flextable::set_header_labels(E = "Jurisdiccion", HOG = "Cantidad de hogares",
                                 CON_p = "Porcentaje de hogares conyugales",
                                 POR_h = "Total", POR_h_PRI = "Primario",
                                 POR_h_SEC = "Secundario", POR_h_SUP = "Superior",
                                 emp = " ",  PRI_p1 = "Primario",
                                 SEC_p1 = "Secundario", SUP_p1 = "Superior") |>
    flextable::merge_v(j = c(1:3, 8), part = "header") |>
    flextable::align(align ='center', part = 'all') |>
    flextable::align(j = 1, align ='left', part = 'all')  |>
    flextable::colformat_double(j = 2, digits = 0) |>
    flextable::colformat_double(j = 3:7, digits = 1, suffix = "%") |>
    flextable::colformat_double(j = 9:11, digits = 3) |>
    flextable::bold(i = 3, part = "body") |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::width(j = 1, 1.5, unit = "cm") |>
    flextable::width(j = 2, 1.9, unit = "cm") |>
    flextable::width(j = 3, 2.1, unit = "cm") |>
    flextable::width(j = 4:7, 1.5, unit = "cm") |>
    flextable::width(j = 8, 0.1, unit = "cm") |>
    flextable::width(j = 9:11, 1.5, unit = "cm")

  # ╚ TAB 5.2 ---------------------------------

  HOM$TAB2 <- HOM$HOM4 |>
    mutate(nivel = factor(nivel, levels = c("PRI", "SEC", "SUP"),
                          labels = c("Primario", "Secundario", "Superior"))) |>
    select(nivel, no_signific, everything()) |>
    flextable::flextable() |>
    flextable::set_caption(caption = "Porcentaje de unidades espaciales con probabilidad de homogamia significativamente diferente de H0",
                           autonum = officer::run_autonum(seq_id = "tab",
                                                          bkm = "HOM-TAB2",
                                                          bkm_all = TRUE))  |>
    flextable::add_header_row(values = c("Nivel",
                                         "No Significativos",
                                         "Significativamente diferentes de H0"),
                              colwidths = c(1, 1, 3)) |>
    flextable::set_header_labels(nivel = "Nivel", no_signific = "No Significativos",
                                 down = "Debajo", up = "Sobre",
                                 signific = "Total") |>
    flextable::merge_v(j = 1:5, part = "header") |>
    flextable::align(align ='center', part = 'all') |>
    flextable::align(j = 1, align ='left', part = 'all')  |>
    flextable::colformat_double(j = 2:5, digits = 2, suffix = "%") |>
    flextable::bg(j = 3:4, part = "body", bg = "grey90") |>
    flextable::color(j = 3:4, part = "body", color = "grey50") |>
    flextable::add_footer_lines(as_paragraph(as_b("Notas:"))) |>
    flextable::add_footer_lines(paste0("Valores obtenidos mediante ",
                                       format(replic, big.mark = ".",
                                              decimal.mark = ","),
                                       " replicaciones Montecarlo. ",
                                       "Nivel de confianza: ", 1 - conf)) |>
    flextable::footnote(j = 3, i = 1, part = "header",
                        ref_symbols = "*", inline = F,
                        value = flextable::as_paragraph(paste0("Porcentaje de simulaciones con valores ",
                                                               "por debajo / sobre el ", 1 - conf,
                                                               " de las simulaciones"))) |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::autofit()

  # Anexo? ----------

  # # Tabla matrimonial
  # HOM$HOM2 |>
  #   filter(!is.na(P2)) |>
  #   pivot_wider(id_cols = P1,
  #               names_from = P2,
  #               values_from = n, values_fill = 0) |>
  #   mutate(P1 = factor(P1, levels = 1:3,
  #                      labels = c("Primario", "Secundario", "Superior")),
  #          across(where(is.numeric), ~ifelse(.x == 0, NA, .x))) |>
  #   rename_with(.fn = ~c(" ", "Primario", "Secundario", "Superior") ) |>
  #   flextable() |>
  #   set_caption(caption = "Tabla matrimonial (Total AMGBA)",
  #               autonum = officer::run_autonum(seq_id = "tab", bkm = "AP3-TAB1",
  #                                              bkm_all = T))

  # # Prueba Chi2
  # HOM$HOM3$stdres[lower.tri(HOM$HOM3$stdres)] <- NA
  # HOM$HOM3$stdres |>
  #   as_tibble(rownames = "P1", ) |>
  #   mutate(P1 = factor(P1, levels = 1:3,
  #                      labels = c("Primario", "Secundario", "Superior")),
  #          across(where(is.numeric), round)) |>
  #   set_names(c(" ", "Primario", "Secundario", "Superior")) |>
  #   flextable::flextable(defaults = set_flextable_defaults(digits = 0)) |>
  #   flextable::set_caption(caption = "Residuos estándar de la prueba Chi Cuadrado",
  #                          autonum =  officer::run_autonum(seq_id = "tab", bkm = "AP3-TAB1",
  #                                                          bkm_all = T)) |>
  #   flextable::add_footer_lines(values = paste0(HOM$HOM3$method, ". ",
  #                                    "Confianza (1-p): ",
  #                                    round((1 - HOM$HOM3$p.value) * 100,2), "%")) |>
  #   flextable::font(fontname='Alegreya Sans', part = "all") |>
  #   flextable::fontsize(size = 9, part = "all") |>
  #   flextable::autofit()

# Guardar resultados ----------------------------------------------------

  HOM$replic <- replic
  HOM$conf <- conf

  usethis::use_data(HOM, overwrite = TRUE)

rm(db_aux, db_aux1, db_aux2)
rm(limit, replic, conf)

# Multi domain: Segregación residencial y segregación conyugal

# Cuadrático (con no-significativos imputados a ratio 1)
# gPRI:  0,44901 + 0,45858 * x + 0,04969 * x^2
# gSEC:  0,1674  + 1,26936 * x - 0,92511 * x^2
# gSUP:  0,10504 + 1,96351 * x - 1,61034 * x^2

# Lineal (con no-significativos imputados a ratio 1)
# gPRI:  0,4398  + 0,5075 * x
# gSEC:  0,2426  + 0,7063 * x
# gSUP:  0,1537  + 1,1834 * x

# Cuadrático (sólo significativos)
# gPRI:  0,4769286 + 0,3512016 * x + 0,1503480 * x^2
# gSEC:  0,2322916 + 0,9372811 * x + -0,351221 * x^2
# gSUP:  0,2796899 + 0,9021898 * x + -0,1613751

# Lineal  (sólo significativos)
# gPRI:  0,449489  + 0,497645  * x
# gSEC:  0,260907  + 0,723853  * x
# gSUP:  0,286555  + 0,817905  * x
