# Homogamy

cli::cli_h2(col_blue("Análisis de Movilidad (MOV)"))

MOV <- list()

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

  if(!exists("ENMODO", inherits = F)){
    f <- fs::path("data-workflow", "final-data", "ENMODO", ext = "rda")

    cli_alert_info(
      col_silver("Cargando datos de hogares (`ENMODO`): {.path {fs::path_rel(f)}}")
    )

    load(file = f)
    rm(f)
  }

# Preparar datos -----------------------

  cli::cli_li(col_yellow("Crear bases de datos auxiliares"))

  db_mim <- ENMODO$MIM
  db_act <- ENMODO$ACT_IND

  replic <- 1000
  conf   <- 0.95

  # Residencial: db_hog
  cli_alert_success(col_silver("Base de segregación residencial (por FRACCIÓN ENMODO)"))
  ## UA: Hogar (considera a inmóviles)
  ## Escala: Fracción
  ## Variable: Máximo nivel educativo del hogar

  db_hog <- ENMODO$HOG |>
    left_join(CARTO$RADIO |> select(ID_RADIO, TH), by = "ID_RADIO") |>
    filter(!is.na(TH)) |>
    mutate(ID_FRAC = str_sub(ID_RADIO, start = 1, end = 7 ) ) |>
    group_by(ID_FRAC) |>
    summarise(PRI = sum(PONDERA_HOG * (EDU %in% c("Sin estudios", "Primario")), na.rm = T),
              SEC = sum(PONDERA_HOG * (EDU %in% c("Secundario")), na.rm = T),
              SUP = sum(PONDERA_HOG * (EDU %in% c("SNU", "Universitario", "Post-Universitario")), na.rm = T)) |>
    pivot_longer(cols = PRI:SUP)

  # Destinos: db_dest
  cli_alert_success(col_silver("Base de segregación de movilidades (por FRACCIÓN ENMODO)"))
  ## UA: Tiempo en destinos
  ## Escala: Fracción
  ## Variable: Máximo nivel educativo del hogar

  # Es equivalente (más preciso por redondeo) a:
  # ENMODO$prop$prop_in |> transmute(DES, across(in_PRI_t:in_SUP_t, ~.x * in_time * 10))
  # De acá sale in_ls

  db_dest <- db_mim |>
    group_by(DES) |> #, MOT = v_MOT
    summarise(PRI = sum(w_time * (p_EDU == "pEDU_Primario"), na.rm = T),
              SEC = sum(w_time * (p_EDU == "pEDU_Secundario"), na.rm = T),
              SUP = sum(w_time * (p_EDU == "pEDU_Superior"), na.rm = T))  |>
    ungroup() |>
    pivot_longer(cols = PRI:SUP) |>
    mutate(value = round(value / 60, 1)) # Corrección para achicar números

  cli_alert_success(col_silver("Base con diferencia de segregación residencial entre residencia y destino"))

  db_diffseg <- ENMODO$MOV_IND |>
    filter(id_viaje2 != 0 & MOT != "Hogar" & EDAD >= 16) |>
    mutate(across(PRI_FROM:SUP_FROM, ~.x/(PRI_FROM + SEC_FROM + SUP_FROM)*100 ),
           across(PRI_DES:SUP_DES, ~.x/(PRI_DES + SEC_DES + SUP_DES)*100 ),
           diff_PRI = PRI_DES - PRI_FROM,
           diff_SEC = SEC_DES - SEC_FROM,
           diff_SUP = SUP_DES - SUP_FROM,
           diff_ls = ls_DES - ls_FROM) |>
    select(ID, PONDERA, PONDERA_time,
           EDU_red,
           starts_with("diff_") )

  # Base de actividades period-person (anos-persona)
  cli_alert_success(col_silver("Base de actividades period-person (anos-persona)"))
  setDT(db_act)
  # Filtrar actividades con duración positiva
  db_act_pp <- db_act[DUR > 0, .(ID, PLACE, EDU_red, w, DUR, PLACE_TH, ACT)]

  # Duplicar filas según duración de la actividad
  db_act_pp <- db_act_pp[rep(seq_len(nrow(db_act_pp)), times = DUR), !c("DUR")]

  # Crear variable tiempo (0 a 1440)
  db_act_pp[ , time := 1:(.N) - 1, keyby = ID]

  # Crear variable hora (0 a 23)
  db_act_pp[ , time_h := cut(time, breaks = 24, labels = paste0(0:23, "h"))]

  # Crear base con tiempo de cada nivel educativo por hora y lugar
  db_act_seg <- db_act_pp[!is.na(PLACE), .(N = sum(w)), by = .(PLACE, time_h, EDU_red)] |>
    split(~time_h)


  invisible(gc())

  cli_alert_success(col_silver("Base de secuencia de actividades (formato STS)"))

  aux_seq <- db_act |>
    group_by(ID) |>
    summarise(w = first(w),
              EDU = first(EDU_red))

  cnames <- ifelse ((0:1439 %% 60) == 0, paste((0:1439 / 60), "H"), paste0(0:1439,"min") )

  seq_ACT <- dcast(db_act_pp, ID ~ time, value.var = "ACT")
  rownames(seq_ACT) <- seq_ACT$ID
  seq_ACT$ID <- NULL

  seq_ACT <- seqdef(data = seq_ACT, weights = aux_seq$w,
                    informat = "STS", alphabet = levels(db_act$ACT),
                    cnames = cnames, xtstep = 60, tick.last = TRUE,
                    cpal = c("#F4A261", "#2B6CB0", "#F9C74F",
                             "#9D4EDD", "#52B788", "#E63946") )

  cli_alert_success(col_silver("Base de secuencia de entornos urbanos (formato STS)"))

  seq_TH <- dcast(db_act_pp, ID ~ time, value.var = "PLACE_TH")
  rownames(seq_TH) <- seq_TH$ID
  seq_TH$ID <- NULL

  seq_TH <- seqdef(data = seq_TH, weights = aux_seq$w,
                   informat = "STS", alphabet = levels(db_act$PLACE_TH),
                   cnames = cnames, xtstep = 60, tick.last = TRUE,
                   cpal = c("#cae1ff", "#836fff", "#2e8b57",
                            "#7d26cd", "#00ee76", "#ee5c42",
                            "#c0ff3e", "#eedd82", "#ffa500",
                            "#8e8e8e")
  )

  rm(db_act, db_act_pp, aux_seq)
  invisible(gc())

# Análisis de movilidades --------------------------------------

### ╠ Descripción de viajes por TH y motivo ---------------------------------

  cli::cli_li(col_yellow("Descripción de viajes por TH y motivo (MOV1)"))

  TH_aux <- CARTO$RADIO |>
    filter(!is.na(TH)) |>
    group_by(TH) |>
    summarise() |>
    mutate(sup = st_area(geometry),
           sup = set_units(sup, value = "km2"),
           sup = drop_units(sup),
           sup_p = round(sup / sum(sup) * 100, 1) ) |>
    st_drop_geometry()

  VIA_TOT <- db_mim |>
    group_by(TH = in_TH) |>
    summarise(N = sum(in_time, na.rm = T))

  VIA_MOT <- db_mim |>
    group_by(TH = in_TH, v_MOT) |>
    summarise(N = sum(in_time, na.rm = T)) |>
    pivot_wider(id_cols = TH, names_from = v_MOT, values_from = "N", values_fill = 0) |>
    ungroup()

  # PODRIA SUMARSE LA DISTRIBUCION RESIDENCIAL DE LA MUESTRA?
  # ALGO COMO:
  # db_mim |> count(p_TH, wt = w) |> mutate(p = n/sum(n)*100)
  VIA <- TH_aux |>
    left_join(VIA_TOT, by = "TH") |>
    left_join(VIA_MOT, by = "TH") |>
    mutate(across(.cols = N:Recreación,
                  .fns = ~round(.x/sum(.x) * 100, 1)),
           sup = round(sup, 1) )

  rm(VIA_TOT, VIA_MOT, TH_aux)

  TIE <- rbind(
    db_mim |> summarise(v_MOT = "N", Dur = round(weighted.mean(v_TVIA, w, na.rm = T), 1)),
    db_mim |> group_by(v_MOT) |> summarise(Dur = round(weighted.mean(v_TVIA, w, na.rm = T), 1))
  ) |>
    pivot_wider(names_from = v_MOT, values_from = "Dur")

  MOV$MOV1 <- rbind(VIA, c(TH = NA, sup = NA,
                          sup_p = NA,
                          TIE) ) |>
    mutate(TH = fct_na_value_to_level(TH, "Tiempo promedio de viaje (minutos)"),
           Ratio = N/sup_p)

  rm(VIA, TIE)

### ╠ Segregación: Diferencia residencial-actividad de destino (ENMODO) -----------------

  # Variables de segregación en base db_mim
  # * p_lsRES:  Segregación residencial local (TOMADA DE RADIO. UA: RADIO)
  # * in_lsRES: Segregación residencial local (TOMADA DE RADIO. UA: RADIO)
  # * in_ls:    Segregación de destino local (de la encuesta ponderada por tiempo. UA: FRACCIÓN)

  cli::cli_li(col_yellow("Diferencia de segregación residencia-actividades de destino (MOV2)"))

  # Comparación de segregación
  MOV$MOV2 <- mutual_difference(db_dest |> rename(ID_FRAC = DES), db_hog,
                               unit = "ID_FRAC", group = "name", weight = "value",
                               se = T, CI = conf, n_bootstrap = replic) |>
    mutate(est = round(est, 3),
           CI2 = map(CI, ~paste0(format(round(.x, 3), nsmall = 3), collapse = ";")),
           CI2 = paste0("(", CI2, ")")) |>
    select(stat, est, CI2)

  rm(db_dest)

### ╠ Segregación: por hora del día -----------------------------

  cli::cli_li(col_yellow("Segregación de actividades por hora del día `Mutual information Index` (MOV3)"))

  # Calcular segregación por hora

  MOV$MOV3 <- db_act_seg |>
    imap(\(db, nombre){
      res <- db |>
        rename(ID_FRAC = PLACE, name = EDU_red, value = N) |>
        mutate(name = fct_recode(name, PRI = "Primario",
                                 SEC = "Secundario", SUP = "Superior")) |>
        mutual_difference(db_hog, unit = "ID_FRAC", group = "name",
                          weight = "value", se = F) |>
        set_names("stat", paste0("S", nombre))
    }) |>
    reduce(left_join, by = "stat") |>
    pivot_longer(cols = S0h:S23h, names_to = "hora", values_to = "value") |>
    pivot_wider(id_cols = "hora", names_from = "stat", values_from = "value") |>
    mutate(hora = str_match(hora, "[0-9][0-9]*")[,1] |> as.numeric())

  cli::cli_li(col_yellow("Segregación de actividades por hora del día `Eta2` (MOV4)"))

  MOV$MOV4 <- db_act_seg |>
    map(\(db){
      db |>
        pivot_wider(id_cols = "PLACE", names_from = "EDU_red",
                    values_from = "N", values_fill = 0) |>
        select(Primario, Secundario, Superior) |>
        # OasisR::ISDuncan() |>
        OasisR::Eta2() |>
        set_names(c("Primario", "Secundario", "Superior"))

    }) |>
    bind_rows(.id = "Hora")

  rm(db_act_seg, db_hog)

### ╚ Análisis de red Bipartita (MCA) -------------------------------------------

  # tibble(nombre = names(db_mim),
  #        n_levels = map(db_mim, ~{if(is.character(.x) | is.factor(.x)) length(unique(.x)) else "cuanti"}),
  #        levs = map(db_mim, ~{if((is.character(.x) | is.factor(.x)) & length(unique(.x)) < 15) paste(levels(.x), collapse = "; ") else ""}),
  #        na_ = map_int(db_mim, ~sum(is.na(.x) ) ) ) |>
  #   # filter(n_levels != "cuanti")
  #   View()

  cli::cli_li(col_yellow("Análisis de red bipartita mediante MCA (MOV3)"))

  var_act <- AUX$VAR_mca |> filter(MCA == "x" & !is.na(MCA)) |> pluck("Variable")
  var_sup <- AUX$VAR_mca |> filter(MCA == "Sup." & !is.na(MCA)) |> pluck("Variable")
  var_quanti <- AUX$VAR_mca |> filter(MCA == "Cuanti" & !is.na(MCA)) |> pluck("Variable")

  MOV$MOV4 <- db_mim |>
    select(all_of(c(var_act, var_sup, var_quanti))) |>
    MCA(row.w = db_mim$w_time, graph = FALSE,
        quali.sup = var_sup,
        quanti.sup = var_quanti) |>
    PhD_eig_corrected()

  rm(var_act, var_sup, var_quanti)

# Figuras ------------------------------------

# Descripción de viajes ---------------------------------------------------

### ╠ FIG 5.3: Mapeo de zonas más visitadas --------------------------------

  p1 <- CARTO$FRAC |>
    left_join(ENMODO$prop$prop_in, by = c("ID_FRAC" = "DES")) |>
    # mutate(in_time = ifelse(in_time_lab %in% c("inT_Q5", "inT_Q4", "inT_Q3"),
    #                         in_time, NA) ) |>
    mutate(A = st_area(geometry) |> set_units("km2") |> drop_units(),
           des_time = in_time / A) |>
    ggplot() +
    geom_sf(aes(fill = des_time), color = NA, alpha = 0.8) +
    geom_sf(data = CARTO$DPTO, fill = NA, color = "grey70" ) +
    scale_fill_viridis_c(paste0("Tiempo total\nen el destino\n(miles de hs./km²)"),
                         direction = 1, option = "viridis",
                         na.value = "#08014e", #limits = c(25, 800),
                         # breaks = 10^(-1:3),#c(25, 50, 100, 200, 400, 800),
                         labels = ~format(.x, scientific = F), #c("25 (o menos)", 50, 100, 200, 400, 800),
                         trans = 'log10') +
    coord_sf(xlim = AUX$lim_AMBA[c(1,3)],
             ylim = AUX$lim_AMBA[c(2,4)]) +
    theme_void()

  ggsave(filename = fs::path("figures", "Movilidad-Destino-tiempo-total", ext = "png"),
         plot =  p1, width = 17, height = 15, units = "cm", dpi = 300)

  rm(p1)

### ╠ FIG 5.4: Destino de viaje según nivel educativo ---------------------------

  prom <- db_mim |>
    summarise(across(ends_with("_t"),
                     ~weighted.mean(.x, w = in_time ) ) ) |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "p")

  db_edu <- CARTO$FRAC["ID_FRAC"] |>
    right_join(ENMODO$prop$prop_in, by = c("ID_FRAC" = "DES" ) ) |>
    filter(n >= 4) |>
    select(ID_FRAC, in_PRI_t:in_SUP_t, in_ls)

  p2 <- map2(.x = prom$var, .y = prom$p, ggmapa)

  p2 <- ggarrange(plotlist = p2, ncol = 3,
                  labels = c("Primario", "Secundario", "Superior") )

  ggsave(filename = fs::path("figures", "Movilidad_Destino-x-nivel-educativo",
                             ext = "png"),
         plot = p2, width = 35, height = 15, units = "cm", dpi = 300)

  rm(p2, prom)


### ╠ FIG 5.5: Descripción de secuencias por Actividad y Entorno  ---------------------------

  png(filename = fs::path("figures", "Movilidad_SA-ACT-Hist", ext = "png"),
      width = 1200, height = 1200)
  seqdplot(seq_ACT, with.legend = T, border=NA, space = 0)
  dev.off()

  png(filename = fs::path("figures", "Movilidad_SA-TH-Hist", ext = "png"),
      width = 1200, height = 1200)
  seqdplot(seq_TH, with.legend = T, border=NA, space = 0)
  dev.off()

  # png(filename = fs::path("figures", "Movilidad_SA-ACTxEDU-Hist", ext = "png"),
  #     width = 1200, height = 1200)
  # seqdplot(seq_ACT, with.legend = T, border=NA, space = 0, group = aux_seq$EDU)
  # dev.off()

  png(filename = fs::path("figures", "Movilidad_SA-THxEDU-Hist", ext = "png"),
      width = 1200, height = 1200)
  seqdplot(seq_TH, with.legend = T, border=NA, space = 0, group = aux_seq$EDU)
  dev.off()



### ╠ FIG 5.6: Mapeo de segregación local de destino ---------------------------

  p3 <- db_edu |>
    ggplot() +
    geom_sf(aes(fill =  in_ls), colour = NA) +
    geom_sf(data = CARTO$DPTO, fill = NA, color = "grey10" ) +
    scale_fill_gradient2("Segregación local\nen destino",
                         low = "#dedede", mid = "#dad9be", high = "#aa0808",
                         midpoint = median(db_edu$in_ls)
    ) +
    coord_sf(xlim = AUX$AMGBA_lim[c(1,3)],
             ylim = AUX$AMGBA_lim[c(2,4)]) +
    theme_void()

  # ggsave(filename = "figures/Destino-Segregacion-local.png", p3,
  #        width = 17, height = 15, units = "cm", dpi = 300)

  rm(p3, db_edu)

### ╠ FIG 5.7: Comparación entre segregación residencial de residencia y lugar de destino -------------------------


### ╚ FIG 5.8: Relación entre TH destino y origen por nivel educativo -----------

  p4 <- db_mim |>
    group_by(in_TH, p_TH, p_EDU) |>
    summarise(N = sum(in_time, na.rm = T)) |>
    group_by(p_TH, p_EDU) |>
    mutate(p = round(N/sum(N) * 100, 1)) |>
    ggplot(aes(x = in_TH, y = p_TH, fill = p, label = p)) +
    geom_tile() +
    geom_text(col = "black") +
    facet_wrap(~p_EDU) +
    theme_minimal() +
    scale_fill_gradient2(low = "white", mid = "#FFBE33", high = "tomato3",
                         midpoint = 30, na.value = "white")

  rm(p4)

### ╠ FIG 5.9: Segregación de actividades por hora del día (M) ---------------------

  p5a <- bind_rows(MOV$MOV4,
                   MOV$MOV4[1, ] |> mutate(hora = 24)) |>
    ggplot(aes(x = hora, y = M1)) +
    geom_line(color = "grey") +
    geom_point() +
    geom_hline(yintercept = MOV$MOV4$M2[1], linetype = "dashed", color = "tomato4") +
    geom_text(label = "Segregación residencial", color = "tomato4",
              x = 10, y = MOV$MOV4$M2[1] + 0.01) +
    labs(x = "Hora del día", y = "Segregación de actividades") +
    scale_x_continuous(breaks = seq(0, 24, by = 3)) +# labels = paste0(0:24, "h")) +
    scale_y_continuous(limits = c(0, 0.21),
                       breaks = seq(0, 0.2, by = 0.02)) +
    theme_minimal()

  p5b <- bind_rows(MOV$MOV4, MOV$MOV4[1, ] |> mutate(hora = 24)) |>
    ggplot() +
    geom_point(data = data.frame(x = c(-1,-1), y = c(-1,-1), color = factor(1:2)),
               aes(x, y, color = color)) +
    geom_line(aes(x = hora, y = structural), color = "grey") +
    geom_point(aes(x = hora, y = structural), color = "tomato4") +
    geom_line(aes(x = hora, y = diff), color = "grey") +
    geom_point(aes(x = hora, y = diff), color = "darkblue") +
    scale_color_manual("Diferencia de segregación",
                       values = c("tomato4", "darkblue"),
                       labels = c("Estructural", "Absoluta")) +
    scale_x_continuous("Hora del día", breaks = seq(0, 24, by = 3),
                       limits = c(0, 24)) +
    scale_y_continuous("Diferencia de segregación", limit = c(0, NA),
                       breaks = seq(0, 0.14, by = 0.02)) +
    theme_minimal() +
    theme(legend.position = "top")

  p5 <- ggarrange(p5a, p5b, ncol = 2) #, labels = c("A", "B"),
  ggsave(p5, filename = fs::path("figures", "Movilidad_Segregacion-x-hora", ext = "png"),
         width = 25, height = 15, units = "cm", dpi = 300)

### ╠ FIG 5.10: Segregación de actividades por hora del día (ETA2) ---------------------

  p6 <- MOV$MOV4 |>
    pivot_longer(cols = Primario:Superior,
                 names_to = "EDU", values_to = "IS") |>
    mutate(Hora = str_remove(Hora, "h") |> as.numeric()) |>
    ggplot(aes(x = Hora, y = IS, color = EDU)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(0, 24, by = 3)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.05),
                       limits = c(0, NA)) +
    labs(x = "Hora del día", y = "Índice de Duncan") +
    theme_minimal()

# MCA -----------------------------------------------------

### ╠ Carga de datos MCA -------------------------------------------------------------

  res_mca <- MOV$res_mca
  coor <- rbind(res_mca$var$coord, res_mca$quali.sup$coord)
  aux_coor <- coor |>
    as_tibble(rownames = "Categ", .name_repair = "universal") |>
    full_join(AUX$VAR_cat, by = "Categ") |>
    mutate(grupo = str_wrap(grupo, 20),
           Categ = str_replace_all(Categ, "_", " "),
           Categ = str_wrap(Categ, 20))

### ╠ FIG 5.11: Proyección de variables en MCA ---------------------------------

  p5 <- fviz_mca_var(res_mca,  geom.var = "point", col.quali.sup = NA, alpha.var = 0, title = "") +
    geom_text_repel(aes(x = Dim.1, y = Dim.2, color = grupo, label = Categ),
                    data = aux_coor |> filter(sel == 1)) +
    scale_color_d3() +
    labs(color= "Grupo de variables") +
    theme(legend.position = "bottom")

  # ggsave(filename = "figures/MCA-Variables-Categ.png", p5,
  #        width = 30, height = 20, units = "cm", dpi = 300)

  rm(p5)

### ╠ FIG 5.12: Proyección de unidades por nivel educativo en MCA ----------------

  aux_lab <- c("Primario", "Secundario", "Superior") |>
    set_names(levels(db_mim$p_EDU))

  p6 <- fviz_mca_ind(res_mca, habillage = "p_EDU", title = "",
                     alpha.ind = 0.02, geom.ind = "point") +
    stat_density_2d(geom = "polygon", contour = TRUE,
                    alpha = 0.25, colour = "grey85", bins = 8,
                    aes(fill = p_EDU, group = p_EDU )) +
    geom_text(data = aux_coor |> filter(ilust == 1), size =3,
              aes(x = Dim.1, Dim.2, label = Categ)) +
    facet_wrap(~p_EDU, ncol = 3, labeller = as_labeller(aux_lab)) +
    xlim(c(-1.2, 1.5)) + ylim(c(-1, 1.9)) +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white", colour = "black"),
          strip.background = element_rect(fill = "grey85", colour = "black"))

  # ggsave(filename = "figures/MCA-Nivel-educativo.png", p6,
  #        width = 30, height = 15, units = "cm", dpi = 300)

  rm(p6, aux_lab, aux_coor)

### ╠ Figura(Anexo): Porcentaje de varianza explicada MCA -----------------------------

  p7a <- fviz_eig(res_mca,
                  barcolor = "tomato3", barfill = "tomato3",
                  linecolor = "grey30",
                  addlabels = T, hjust = 0.5,
                  main = "Varianza explicada (Ajuste: Greenacre)",
                  xlab = "Dimensión",
                  ylab = "Porcentaje de varianza explicada [%]")

  p7b <- res_mca$eig |>
    as.data.frame() |>
    rename(Greenacre = "cumulative percentage of variance") |>
    mutate(Benzécri = eigenvalue / sum(eigenvalue) * 100,
           Benzécri = cumsum(Benzécri)) |>
    slice_head(n = 10) |>
    mutate(Indicator = res_mca$eig_original[1:10, 3],
           Dim = 1:10) |>
    pivot_longer(cols = c("Greenacre", "Benzécri", "Indicator")) |>
    # mutate(factor(name, levels = c("Greenacre", "Benzécri", "Indicator"), ordered = T)) |>
    ggplot(aes(x = Dim, y = value, color = name, group = name))+
    geom_point(shape = 4) +
    geom_line(aes(linetype = !(name == "Greenacre") )) +
    scale_x_continuous("Dimensión", breaks = 0:10 ) +
    scale_y_continuous("Porcentaje de varianza acumulada [%]", limits = c(0, 100)) +
    labs(title = "Varianza acumulada", color = "Método") +
    scale_color_igv() +
    guides(linetype = "none") +
    theme_minimal()

  # ggsave(filename = here::here("figures/anexo_B_eig.png"),
  #        plot = ggarrange(p7a, p7b),
  #        width = 30, height = 15, units = "cm")

  rm(p7a, p7b)

### ╠ Figura(Anexo): Correlación entre variables y dimensiones de análisis ------------

  var_exp <- paste0("Dim ", 1:4, " (",round(res_mca$eig[1:4, 2], 1), "%)")

  coor_var <- list(Activas = res_mca$var$eta2, Suplementarias = res_mca$quali.sup$eta2) |>
    map(~as_tibble(.x, rownames = "Var", .name_repair = "universal")) |>
    bind_rows(.id = "Rol")

  p8a <- coor_var |>
    filter(Dim.1 > mean(Dim.1) | Dim.2 > mean(Dim.2) ) |>
    ggplot(aes(x = Dim.1, y = Dim.2, label = Var, color = Rol)) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_text_repel() +
    scale_color_uchicago() +
    coord_fixed() +
    labs(x = var_exp[1], y = var_exp[2]) +
    lims(x = c(0,1), y = c(0,1)) +
    theme_minimal()

  p8b <- coor_var |>
    filter(Dim.3 > mean(Dim.3) | Dim.4 > mean(Dim.4) ) |>
    ggplot(aes(x = Dim.3, y = Dim.4, label = Var, color = Rol)) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point() +
    geom_text_repel() +
    scale_color_uchicago() +
    coord_fixed() +
    labs(x = var_exp[3], y = var_exp[4]) +
    lims(x = c(0,1), y = c(0,1)) +
    theme_minimal()

  # ggarrange(p8a, p8b, common.legend = T, legend = "bottom",
  #           labels = paste("Dimensiones", c("1 y 2", "3 y 4")) ) |>
  #   ggsave(file = "figures/anexo_B_dist-var.png",
  #          width = 30, height = 15, units = "cm", dpi = 300)

  rm(p8a, p8b, coor_var, var_exp)

### ╚ Figura(Anexo): Descripción de variables ordinales MCA -------------------------------------

  VAR <- AUX$VAR_cat |>
    filter(Ordenada == 1) |>
    mutate(tit = paste(Nivel, var_lab, sep = ": ") )

  coor_ord <- coor[VAR$Categ, ] |>
    as_tibble(rownames = "Variable", .name_repair = "universal") |>
    mutate(lab = str_split_i(Variable, pattern = "_", -1))

  p9 <- unique(VAR$Variable) |>
    map(~fviz_variables(.x, MCA_obj = res_mca, V = VAR, d = coor_ord) ) |>
    set_names(unique(VAR$Variable))

  ggarrange(plotlist = p9[c(5:7, 11:13)] ) |>
    ggsave(file = "figures/anexo_B_var1.png",
           width = 40, height = 20, units = "cm", dpi = 300)

  # ggarrange(plotlist = p9[c(9:10, 14:17)] ) |>
  #   ggsave(file = "figures/anexo_B_var2.png",
  #          width = 40, height = 20, units = "cm", dpi = 300)
  #
  # # ggarrange(plotlist = p9[c(8, 18)] ) |>
  # #   ggsave(file = "figures/anexo_B_var2.png",
  # #          width = 20, height = 10, units = "cm", dpi = 300)
  #
  # ggarrange(plotlist = p9[c(8, 4, 1, 18, 3, 2)] ) |>
  #   ggsave(file = "figures/anexo_B_var3.png",
  #          width = 40, height = 20, units = "cm", dpi = 300)

  rm(VAR, coor, coor_ord, p9)
  rm(res_mca)


# Tablas ------------------------------------

### ╠ TAB 5.1 ----------------------

  MOV$TAB1 <-  MOV$MOV1 |>
    select(-sup, -Ratio) |>
    relocate(N, .after = last_col() ) |>
    rename("Entorno urbano de destino" = TH,
           "Superfice (%)" = sup_p, "Total" = N ) |>
    flextable() |>
    set_caption(caption = paste("Porcentaje de destino de los viajes",
                                "en función del tipo de entorno urbano",
                                "del destino y el motivo de la actividad",
                                "de destino (ponderado por minutos totales",
                                "permanecidos)"),
                autonum = run_autonum(seq_id = "tab", bkm = "DES-tabla",
                                      bkm_all = T))  |>
    add_header_row(values = c("Entorno urbano de destino",
                              "Superfice (%)",
                              "Entorno urbano de destino según motivo de viaje (%)"),
                   colwidths = c(1, 1, 5)) |>
    merge_v(j = c(1:3, 8), part = "header") |>
    align(align ='center', part = 'all') |>
    align(j = 1, align ='left', part = 'all')  |>
    color(j = 2, color = "grey50") |>
    bg(i = nrow(MOV$MOV1), bg = "grey80") |>
    hline(i = nrow(RES$VIA) - 1) |>
    bold(j = 7, part = "all") |>
    italic(j = 7, part = "all") |>
    autofit() |>
    fit_to_width(max_width = 6)

### ╠ TAB 5.2 ---------------------

  d <- MOV$SEG$est[MOV$SEG$stat == "diff"]
  f <- paste("Los valores tienen una corrección de sesgo a partir del cálculo de",
             replic, "iteraciones con Bootstrap. Todos los valores tienen",
             "una significancia estadística superior al ", conf, "%.") |>
    str_wrap(60)

  lab <- data.frame(stat = MOV$SEG$stat,
                    st = c("Segregación de destino (M1)", "Segregación residencial (M2)",
                           "Diferencia (M2-M1)", "Unidades agregadas",
                           "Unidades removidas", "Marginal de grupos",
                           "Marginal de unidades", "Estructural"))
  MOV$TAB2 <- MOV$SEG |>
    left_join(lab, by = "stat") |>
    mutate(p = ifelse(stat %in% c("M1", "M2"), NA, est / d * 100) ) |>
    select(st, est, p) |>
    flextable() |>
    set_caption(caption = paste("Descomposición de la diferencia entre la",
                                "segregación residencial y la segregación de",
                                "destinos"),
                autonum = run_autonum(seq_id = "tab", bkm = "SEG-tabla",
                                      bkm_all = T)) |>
    set_header_labels(st = " ", est = "Segregación (M)", p = "Δ%") |>
    add_footer_lines(values = "Método de esimación: Shapley.") |>
    flextable::footnote(i = 1, j = 2, part = "header", ref_symbols = "a",
                        value = as_paragraph(f) ) |>
    colformat_double(j = 3 , digits = 1) |>
    align(i = 1:2, j = 2, align = "center") |>
    align(i = 3:8, j = 2, align = "right") |>
    align(i = 4:8, j = 1, align = "right") |>
    hline(i = 2:3) |>
    italic(4:8) |>
    color(4:8, color = "grey60") |>
    autofit()

  rm(d, f, lab)
