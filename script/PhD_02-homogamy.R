# Homogamia

HOM <- list()

# Homogamia -----------------------------------
### ╠ Porcentaje de viviendas con parejas homogámicas ------------------
  # Porcentaje de homogamia y distancia educativa promedio sobre
  #   el total de parejas (no incluye hogares no-matrimoniales)

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

### ╠ Porcentaje de Radios con Probabilidad de homogamia no-significativa -----------

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

### ╠ Density plot de probabilidad de homogamia por entorno urbano ----------

  p1_TOT <- CARTO$RADIO |>
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

  p1_TAB <- CARTO$RADIO |>
    st_drop_geometry() |>
    filter(!is.na(TH)) |>
    summarise(across(PRI_p1:SUP_p1,
                     list(Media   = ~mean(.x, na.rm = T),
                          Mediana = ~median(.x, na.rm = T),
                          Desvío  = ~sd(.x, na.rm = T)))) |>
    # across(PRI_p2:SUP_p2, ~mean(.x, na.rm = T), .names = "{col}_Media"),
    #         across(PRI_p2:SUP_p2, ~median(.x, na.rm = T), .names = "{col}_Mediana"),
    #         across(PRI_p2:SUP_p2, ~sd(.x, na.rm = T), .names = "{col}_Desvío")
    pivot_longer(cols = everything()) |>
    mutate(Nivel = str_sub(name, 1, 3),
           Nivel = factor(Nivel, levels = c("PRI", "SEC", "SUP"),
                          labels = c("Primario", "Secundario", "Superior")),
           name = str_sub(name, 7, -1),
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
    select(TH = E, PRI_p:SUP_p) |>
    pivot_longer(cols = PRI_p:SUP_p) |>
    mutate(name = paste0(name, 1))

  p1_TH <- p1_TOT  +
    geom_vline(data = HOM_aux,
               mapping = aes(xintercept = value, color = name),
               alpha = .6, linetype = 2) +
    facet_wrap(~TH) +
    guides(color = "none")+
    labs(title = NULL)

  p_blank <- ggplot() + theme_minimal()

  p1 <- ggarrange(ggarrange(p_blank,
                            p1_TOT + scale_x_continuous("") + guides(color = "none"),
                            p_blank,
                            p1_TAB, p_blank,
                            nrow = 1, widths = c(2, 6, 1, 5, 2)),
                  p1_TH,
                  nrow = 2, heights = c(4, 8) )

  ggsave(filename = here::here("figures",
                               "Homogamia_probabilidad-densidad-x-TH.png"),
         plot = p1,
         height = 18, width = 22, units = "cm", dpi = 300)

  rm(HOM_aux, p1, p1_TOT, p1_TAB, p1_TH, p_blank)

### ╚ Distribución espacial de la probabilidad de homogamia (por tipo) ----------------
  ### Local analysis (spatialized analysis)

  p2_vln <- CARTO$RADIO |>
    st_drop_geometry() |>
    select(ID_RADIO, ends_with("_p3")) |>
    pivot_longer(cols =  ends_with("_p3")) |>
    mutate(name = factor(name,
                         levels = c("PRI_p3", "SEC_p3", "SUP_p3"),
                         labels = paste("Homogamia de Nivel",
                                        c("Primario", "Secundario", "Superior") ) ) ) |>
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

  # p2_AMBA <- CARTO$RADIO |>
  #   filter(!is.na(n_hom) & !is.na(TH)) |>
  #   pivot_longer(cols = c("PRI_p3", "SEC_p3", "SUP_p3"),
  #                names_to = "variable", values_to = "value") |>
  #   mutate(value = ifelse(n_con >= AUX$limit, value, NA) ) |>
  #   ggplot() +
  #   geom_point(data = data.frame(x = 1, y = 1),
  #              aes(x = x, y = y, size = ""), shape = NA, colour = "grey70")+
  #   geom_sf(data = CARTO$AMGBA, #aes(color = ""),
  #           fill = NA, alpha = 0.4) +
  #   geom_sf(aes(fill  = value), linewidth = 0.01, color = NA) +
  #   geom_sf(data = CARTO$DPTO, color = "grey80", fill = NA) +
  #   scale_fill_distiller("Probabilidad de\nhomogamia",
  #                        palette = "RdPu", direction = 1,
  #                        na.value = "grey70") +
  #   facet_wrap(~variable, strip.position = "bottom") +
  #   coord_sf(xlim = AUX$AMGBA_lim[c(1,3)],
  #            ylim = AUX$AMGBA_lim[c(2,4)]) +
  #   guides(size = guide_legend("No significativo", order = 2,
  #                              override.aes = list(shape = 15, size = 15)),
  #          fill = guide_colorbar(order = 1)) +
  #   theme_void() +
  #   theme(legend.position="bottom",
  #         strip.text = element_blank(),
  #         legend.key.height = unit(1.2, 'cm'),
  #         legend.key.width = unit(1.8, 'cm'),
  #         legend.title = element_text(size = 16)
  #   )
  #
  # p2 <- ggarrange(p2_vln, p2_AMBA,
  #                 nrow = 2,  heights = c(2, 7),
  #                 common.legend = T, legend = "bottom")
  #
  # ggsave(filename = here::here("figures", "Homogamia_probabilidad-x-tipo.png",
  #        plot = p2,
  #        height = 18 * 1.5, width = 38 * 1.5, units = "cm", dpi = 300)
  #
  # rm(p2_vln, p2_AMBA, p2)


### Tablas ------------------------------

  # ╠ Tabla 5.1 ---------------------------------

  HOM$TAB1 <- HOM$HOM1[10:12, ] |>
      mutate(across(c(CON_p:POR_h_SUP), ~round(.x, 1)),
             across(c(PRI_p:SUP_p), ~round(.x, 3)),
             emp = NA) |>
      select(1:7, emp, 8:10) |>
      flextable() |>
      set_caption(caption = "Indicadores de homogamia educativa según jurisdiccion",
                  autonum = run_autonum(seq_id = "tab", bkm = "HOM-TAB1",
                                        bkm_all = T))  |>
      add_header_row(values = c("Jurisdiccion", "Cantidad de hogares",
                                "Porcentaje de hogares conyugales",
                                "Porcentaje de homogamia",
                                " ",
                                "Probabilidad de homogamia"),
                     colwidths = c(1, 1, 1, 4, 1, 3)) |>
      set_header_labels(E = "Jurisdiccion", HOG = "Cantidad de hogares",
                        CON_p = "Porcentaje de hogares conyugales",
                        POR_h = "Total", POR_h_PRI = "Primario",
                        POR_h_SEC = "Secundario", POR_h_SUP = "Superior",
                        emp = " ",  PRI_p = "Primario",
                        SEC_p = "Secundario", SUP_p = "Superior") |>
      merge_v(j = c(1:3, 8), part = "header") |>
      align(align ='center', part = 'all') |>
      align(j = 1, align ='left', part = 'all')  |>
      colformat_double(j = 2, digits = 0) |>
      colformat_double(j = 3:7, digits = 1, suffix = "%") |>
      colformat_double(j = 9:11, digits = 3) |>
      bold(i = 3, part = "body") |>
      autofit()


  # ╚ Tabla 5.2 ---------------------------------

  HOM$TAB2 <- HOM$HOM4 |>
    mutate(nivel = factor(nivel, levels = c("PRI", "SEC", "SUP"),
                          labels = c("Primario", "Secundario", "Superior"))) |>
    flextable() |>
    set_caption(caption = "Porcentaje de unidades espaciales con probabilidad de homogamia significativamente diferente de H0",
                autonum = run_autonum(seq_id = "tab", bkm = "HOM-TAB2",
                                      bkm_all = T))  |>
    add_header_row(values = c("Nivel",
                              "Significativamente diferentes de H0",
                              "No Significativos"),
                   colwidths = c(1, 3, 1)) |>
    set_header_labels(nivel = "Nivel", down = "Debajo", up = "Sobre",
                      signific = "Total",
                      no_signific = "No Significativos") |>
    merge_v(j = c(1, 5), part = "header") |>
    align(align ='center', part = 'all') |>
    align(j = 1, align ='left', part = 'all')  |>
    colformat_double(j = 2:5, digits = 2, suffix = "%") |>
    bg(j = 2:3, part = "body", bg = "grey90") |>
    color(j = 2:3, part = "body", color = "grey50") |>
    add_footer_lines(as_paragraph(as_b("Notas:"))) |>
    add_footer_lines(paste0("Valores obtenidos mediante ",
                            format(AUX$replic, big.mark = "."),
                            " replicaciones Montecarlo. ",
                            "Nivel de confianza: ", 1 - AUX$conf)) |>
    footnote(j = 2, i = 1, part = "header",
             ref_symbols = "*", inline = F,
             value = as_paragraph(paste0("Porcentaje de simulaciones con valores ",
                            "por debajo / sobre el ", 1 - AUX$conf,
                            " de las simulaciones"))) |>
    autofit()

### Guardar resultados ----------------------------------------------------

usethis::use_data(HOM, overwrite = TRUE)


# Multi domain: Segregación residencial y segregación conyugal ---------------------

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

### ╠ Preparar datos --------------------------------------------------------

  MULTIDOM <- CARTO$RADIO |>
    st_drop_geometry() |>
    filter(!is.na(TH) & !is.na(n_hom)) |>
    select(ID_RADIO, TH, ls,
           ends_with("_head"),
           ends_with("_p"),
           ends_with("_p3"),
           ends_with("_IC"),
           ends_with("_sig"),
           ends_with("_MEAN"),
           ends_with("_SD")) |>
    unnest(ends_with("_IC")) |>
    mutate(across(PRI_head:SUP_head,
                  ~ .x/(n_head))) |>
    select(ID_RADIO, TH, ls, n_head,
           starts_with("PRI"), starts_with("SEC"),
           starts_with("SUP"))

  MULTIDOM1 <- MULTIDOM |>
    pivot_longer(cols = c(PRI_p, SEC_p, SUP_p,
                          PRI_head, SEC_head, SUP_head,
                          PRI_sig, SEC_sig, SUP_sig) ) |>
    mutate(tipo = case_when(str_ends(name, "_p") ~ "Probabilidad",
                            str_ends(name, "_head") ~ "Proporcion",
                            str_ends(name, "_sig") ~ "Significatividad"),
           g = str_sub(name, 1, 3) ) |>
    pivot_wider(id_cols = c(ID_RADIO, TH, g),
                names_from = tipo,
                values_from = value) |>
    mutate(g = factor(g,
                      levels = c("PRI", "SEC", "SUP"),
                      labels = c("Primario", "Secundario", "Superior") ),
           Significatividad = as.logical(Significatividad),
           Probabilidad_2 = ifelse(Significatividad, Probabilidad, Proporcion),
           dif = Probabilidad - Proporcion,
           Ratio =  Proporcion / Probabilidad,
           Ratio2 =  Proporcion / Probabilidad_2,
           Ratio3 = ifelse(is.infinite(Ratio),
                           1 - abs(rnorm(n = length(is.infinite(Ratio)),
                                         mean = 0, sd = 0.30)),
                           Ratio) )

  modelo <- lm(Probabilidad_2 ~ g + Proporcion + g * Proporcion + g * I(Proporcion^2),
               data = MULTIDOM1 |> filter(!is.na(Significatividad) & (Proporcion > 0.05 | Significatividad)))

  MULTIDOM1 <- MULTIDOM1 |>
    mutate(predict = predict(object = modelo, MULTIDOM1),
           resid = Probabilidad_2 - predict)

  MULTIDOM2 <- CARTO$RADIO |>
    left_join(MULTIDOM1 |>
                filter(!is.na(Significatividad)) |>
                select(ID_RADIO, g, Ratio),
              by = join_by(ID_RADIO)) |>
    mutate(Ratio = ifelse(is.infinite(Ratio), 1,  Ratio ))

  rm(modelo)

### ╠ Ratio entre proporción y probabilidad de homogamia -----------

  p3 <- MULTIDOM1 |>
    filter(!is.na(Significatividad) & (Proporcion > 0.05 | Significatividad)) |>
    ggplot(aes(x = Proporcion, y = Probabilidad, color = g, group = g )) +
    geom_abline(slope = 1, linetype = 2, alpha = 0.4, color = "grey30") +
    geom_point(alpha = 0.02) +
    geom_vline(data = pivot_longer((RES$SEG4[10, 2:4]/100), cols = everything(),
                                   names_to = "g", values_to = "intercept") |>
                 mutate(g = factor(g,
                                   levels = c("PRI", "SEC", "SUP"),
                                   labels = c("Primario", "Secundario", "Superior") ) ),
               aes(xintercept = intercept, color = g),
               alpha = 0.7, linetype = 2) +
    geom_smooth(aes(y = Probabilidad_2),
                color = "grey40", linewidth = 0.3, fill = "grey20",
                method = "lm", formula = y ~ poly(x, 2)) +
    geom_point(aes(y = predict), alpha = 0.01) +
    annotate("text", x = 0.22, y = 0.2 , color = "grey45",
             angle = 45, label = TeX("Hipotesis nula ($H_0$)") ) +
    scale_x_continuous(name = "Proporción del grupo en la unidad censal",
                       limits = c(0, 1), expand = c(0,0)) +
    scale_y_continuous(name = "Probabilidad de homogamia",
                       limits = c(0, 1), expand = c(0,0)) +
    scale_color_manual(values = c("tomato3", "goldenrod1", "darkolivegreen4")) +
    coord_fixed() +
    theme_minimal() +
    theme(legend.position = "none")

  p3 <- ggMarginal(p3, type = "density",
                   groupColour = TRUE, groupFill = TRUE)

  p3 <- ggarrange(p3 |> as_ggplot(),
                  ggplot(data.frame()) +
                    geom_point(x = NA, y = NA,
                               aes(color = c("Primario", "Secundario", "Superior")) ) +
                    scale_color_manual(values = c("tomato3", "goldenrod1", "darkolivegreen4")) +
                    labs(color = "Nivel") +
                    theme_minimal(),
                  widths = c(10,0),
                  common.legend = T,
                  legend = "bottom")

  p3_add1 <- MULTIDOM1 |>
    filter(!is.na(Significatividad) & (Proporcion > 0.05 | Significatividad)) |>
    ggplot(aes(x= Proporcion, y = 1-Ratio2, color = g)) +
    geom_smooth(se = F, linewidth = 0.4,
                method = 'glm', formula = y ~ sqrt(x)   ) +
    geom_smooth(se = F, linewidth = 0.4, color = "grey60", linetype = 2,
                method = 'glm', formula = y ~ sqrt(x)  ) +
    facet_wrap(~"Total", ncol = 1, strip.position="right") +
    scale_color_manual(values = c("tomato3", "goldenrod1", "darkolivegreen4")) +
    guides(color = "none") +
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
          axis.ticks.x = element_blank() )

  p3_add2 <- MULTIDOM1 |>
    filter(!is.na(Significatividad) & (Proporcion > 0.05 | Significatividad)) |>
    ggplot(aes(x= Proporcion, y = 1-Ratio3, color = g)) +
    geom_point(alpha = 0.01) +
    geom_point(data = MULTIDOM1 |> filter(!Significatividad & (Proporcion > 0.05 | Significatividad)),
               color = "grey50", alpha = 0.05, shape = 4) +
    geom_smooth(aes(y = 1-Ratio2, group = g),
                color = "grey25", se = T, linewidth = 0.4,
                method = 'glm', formula = y ~ sqrt(x)   ) +
    facet_wrap(~g, ncol = 1, strip.position="right") +
    scale_color_manual(values = c("tomato3", "goldenrod1", "darkolivegreen4")) +
    guides(color = "none") +
    scale_x_continuous(name = "Proporción del grupo \nen la unidad espacial",
                       limits = c(0, 1), expand = c(0,0)) +
    scale_y_continuous(name = "Porcentaje de homogamia no-explicada",
                       limits = c(0, 1), expand = c(0,0),
                       labels = scales::percent) +
    theme_light() +
    theme(strip.text.x = element_text(size = 8),
          panel.background = element_rect(fill = 'transparent'),
          plot.background = element_rect(fill='transparent', color = NA),
          strip.background = element_rect(fill="lemonchiffon4"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  # ggarrange(p1_add1, p1_add2, ncol = 1, heights = c(1, 4))
  p_blank <- ggplot() + theme_minimal()

  p3 <- ggarrange(p3,
                  ggarrange(p3_add1, p3_add2, p_blank,
                            ncol = 1, heights = c(1, 4, 0.35)),
                  widths = c(3,1.2))

  ggsave("figures/Multidomain_por-vs-prob_prop+0.05.png",
         p3, width = 27.5, height = 22, units = "cm", dpi = 300)

  rm(p3, p_blank, p3_add1, p3_final)

### ╚ Distribución espacial de Ratio e/proporción y probabilidad de homogamia ------------------------------------------

  # p4 <- MULTIDOM2 |>
  #   filter(!is.na(g)) |>
  #   ggplot() +
  #   geom_sf(data = CARTO$AMGBA, fill = "white", alpha = 0.5) +
  #   geom_sf(aes(fill  = (1-Ratio)* 100), color = NA) +
  #   scale_fill_gradient("% de Homogamia no-explicada",
  #                       low = "wheat1", high = "indianred3",
  #                       limits = c(0, 110)) +
  #   facet_wrap(~g, strip.position = "bottom") +
  #   coord_sf(xlim = AUX$AMGBA_lim[c(1,3)],
  #            ylim = AUX$AMGBA_lim[c(2,4)]) +
  #   theme_void() +
  #   theme(legend.position="bottom")
  #
  # ggsave(filename = "figures/Multidomain_homog-explicada.png", plot = p4,
  #        height = 18, width = 30, units = "cm", dpi = 300)
  #
  # rm(MULTIDOM, MULTIDOM1, MULTIDOM2)
