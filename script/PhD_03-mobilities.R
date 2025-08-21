# Mobilities

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

### ╠ Base de segregación residencial (db_hog) y de destino (db_dest) ----------------------

  # Residencial: db_hog
  cli_alert_success(col_silver("Base de segregación residencial (por FRACCIÓN ENMODO)"))
  ## UA: Hogar (considera a inmóviles)
  ## Escala: Fracción
  ## Variable: Máximo nivel educativo del hogar

  db_hog <- ENMODO$HOG |>
    left_join(CARTO$RADIO |> select(ID_RADIO, TH), by = "ID_RADIO") |>
    filter(!is.na(TH)) |>
    mutate(ID_FRAC = str_sub(ID_RADIO, start = 1, end = 7 ) ) |>
    group_by(ID_FRAC, TH) |>
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
    group_by(DES, in_TH) |> #, MOT = v_MOT
    summarise(PRI = sum(w_time * (p_EDU == "pEDU_Primario"), na.rm = T),
              SEC = sum(w_time * (p_EDU == "pEDU_Secundario"), na.rm = T),
              SUP = sum(w_time * (p_EDU == "pEDU_Superior"), na.rm = T))  |>
    ungroup() |>
    pivot_longer(cols = PRI:SUP) |>
    mutate(value = round(value / 60, 1)) # Corrección para achicar números

### ╠ Base de actividades period-person (años-persona) ----------------------

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

### ╚ Bases en formato STS para Análisis de secuencias ---------------------

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
                            "#8e8e8e") )

  attr(seq_TH, "props") <- attr(seq_ACT, "props") <- aux_seq

  rm(db_act, db_act_pp, aux_seq, cnames)
  invisible(gc())

# Análisis de movilidades --------------------------------------

### ╠ Descripción de viajes por TH y motivo ---------------------------------

  cli::cli_li(col_yellow("Descripción de viajes por TH y motivo (MOV1)"))

  TH_aux <- CARTO$RADIO |>
    filter(!is.na(TH)) |>
    group_by(TH) |>
    summarise(.groups = "drop_last") |>
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
  # CARTO$RADIO |> st_drop_geometry()|> count(TH, wt = HOG) |> mutate(p = n/sum(n)*100)
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

### ╠ Segregación: diferencia entre segregación residencial de origen y destino ----------------------

  cli::cli_li(col_yellow("Diferencia entre segregación residencial de origen y destino (MOV3)"))

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

  MOV$MOV3 <- db_diffseg |>
    group_by(EDU_red) |>
    summarise(across(starts_with("diff_"),
                     ~weighted.mean(.x, w = PONDERA_time, na.rm = T)))

  MOV$MOV3 <- bind_rows(MOV$MOV3,
                        summarise(db_diffseg,
                                  EDU_red = "Total",
                                  across(starts_with("diff_"),
                                         ~weighted.mean(.x, w = PONDERA_time, na.rm = T)))) |>
    rename_with(~str_remove(.x, "diff_"), starts_with("diff_")) |>
    mutate(across(PRI:SUP, ~round(.x, 1)),
           ls = round(ls, 3))

  rm(db_diffseg)

### ╠ Segregación: por hora del día -----------------------------

  cli::cli_li(col_yellow("Segregación de actividades por hora del día (MOV4)"))

  cli::cli_alert_success(col_grey("Cálculo de segregación (`Mutual information Index`)"))

  aux_SEG_M <- db_act_seg |>
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
    pivot_longer(cols = S0h:S23h, names_to = "Hora", values_to = "value") |>
    pivot_wider(id_cols = "Hora", names_from = "stat", values_from = "value") |>
    mutate(Hora = str_remove(Hora, "^S"))
    # mutate(Hora = str_match(Hora, "[0-9][0-9]*")[,1] |> as.numeric())

  cli::cli_alert_success(col_grey("Cálculo del índice de aislamiento normalizado (`Eta^2`)"))

  aux_SEG_ETA <- db_act_seg |>
    map(\(db){
      db |>
        pivot_wider(id_cols = "PLACE", names_from = "EDU_red",
                    values_from = "N", values_fill = 0) |>
        select(Primario, Secundario, Superior) |>
        OasisR::Eta2() |>
        set_names(c("Primario", "Secundario", "Superior"))

    }) |>
    bind_rows(.id = "Hora")

  cli::cli_alert_success(col_grey("Cálculo de segregacion entre pares (`Dissimilarity pair-index`)"))

  aux_SEG_DI <- db_act_seg |>
    map(\(db){
      res <- db |>
        pivot_wider(id_cols = "PLACE", names_from = "EDU_red",
                    values_from = "N", values_fill = 0) |>
        select(Primario, Secundario, Superior) |>
        OasisR::DIDuncan()

      res <- c("D_PRI_SEC" = res[1, 2],
               "D_SEC_SUP" = res[2, 3],
               "D_PRI_SUP" = res[1, 3] )

      return(res)

    }) |>
    bind_rows(.id = "Hora")

  # cli::cli_alert_success(col_grey("Cálculo de interacción entre pares (`xPy`)"))
  #
  # aux_SEG_xPy <- db_act_seg |>
  #   map(\(db){
  #     P <- db |>
  #       pivot_wider(id_cols = "PLACE", names_from = "EDU_red",
  #                   values_from = "N", values_fill = 0) |>
  #       select(Primario, Secundario, Superior) |>
  #       OasisR::xPy() |>
  #       as.numeric()
  #
  #     res <- tibble(Grupo = paste0(c("PRI", "SEC", "SUP"), "_",
  #                                  c(rep("PRI", 3), rep("SEC", 3), rep("SUP", times = 3))),
  #                   res = P )
  #     return(res)
  #
  #   }) |>
  #   bind_rows(.id = "Hora")

  cli::cli_alert_success(col_grey("Cálculo del índice de interacción entre pares (`Dissimilarity pair-index`)"))

  aux_SEG_xPy_norm <- db_act_seg |>
    map(\(db){

      marg <- db |> count(EDU_red, wt = N)

      marg_matrix <- matrix( rep(marg$n, 3), nrow = 3, byrow = T)

      marg_sum <-  cross_join(marg, marg) |>
        mutate(N = n.x + n.y) |>
        pivot_wider(id_cols = 1, names_from = 3, values_from = N) |>
        select(-1) |>
        as.matrix()

      P <- db |>
        pivot_wider(id_cols = "PLACE", names_from = "EDU_red",
                    values_from = "N", values_fill = 0) |>
        select(Primario, Secundario, Superior) |>
        OasisR::xPy(exact = TRUE) |>
        as.matrix()

      xPy <- (P  * marg_sum / marg_matrix) |> unname()

      res <- c(P_PRI_SEC = xPy[1, 2], P_PRI_SUP = xPy[1, 3], P_SEC_SUP = xPy[2, 3])
            # (P_PRI_PRI = xPy[1, 1], P_SEC_SEC = xPy[2, 2], P_SUP_SUP = xPy[3, 3])

      return(res)

    }) |>
    bind_rows(.id = "Hora")

  MOV$MOV4 <- aux_SEG_M |>
    left_join(aux_SEG_ETA, by = "Hora") |>
    left_join(aux_SEG_xPy_norm, by = "Hora") |>
    left_join(aux_SEG_DI, by = "Hora")

  rm(db_act_seg, db_hog, aux_SEG_M, aux_SEG_xPy_norm, aux_SEG_ETA, aux_SEG_DI)

### ╚ Análisis de red Bipartita (MCA) -------------------------------------------

  # tibble(nombre = names(db_mim),
  #        n_levels = map(db_mim, ~{if(is.character(.x) | is.factor(.x)) length(unique(.x)) else "cuanti"}),
  #        levs = map(db_mim, ~{if((is.character(.x) | is.factor(.x)) & length(unique(.x)) < 15) paste(levels(.x), collapse = "; ") else ""}),
  #        na_ = map_int(db_mim, ~sum(is.na(.x) ) ) ) |>
  #   # filter(n_levels != "cuanti")
  #   View()

  cli::cli_li(col_yellow("Análisis de red bipartita mediante MCA (MOV5)"))

  var_act <- AUX$VAR_mca |> filter(MCA == "x" & !is.na(MCA)) |> pluck("Variable")
  var_sup <- AUX$VAR_mca |> filter(MCA == "Sup." & !is.na(MCA)) |> pluck("Variable")
  var_quanti <- AUX$VAR_mca |> filter(MCA == "Cuanti" & !is.na(MCA)) |> pluck("Variable")

  MOV$MOV5 <- db_mim |>
    select(all_of(c(var_act, var_sup, var_quanti))) |>
    MCA(row.w = db_mim$w_time, graph = FALSE,
        quali.sup = var_sup,
        quanti.sup = var_quanti) |>
    PhD_eig_corrected()

  rm(var_act, var_sup, var_quanti)

# Figuras ------------------------------------

# ══► Descripción de viajes ---------------------------------------------------

### ╠ FIG 6.3: Descripción de secuencias por Actividad y Entorno  ---------------------------

  cli::cli_li(col_yellow("Figure 6.3: Descripción de secuencias por Actividad y Entorno"))

  png(filename = fs::path("figures", "Movilidad_SA-ACT-Hist", ext = "png"),
      width = 1500, height = 1200)
  seqdplot(seq_ACT, with.legend = TRUE, legend.prop = 0.1,
           border = NA, space = 0)
  dev.off()

  png(filename = fs::path("figures", "Movilidad_SA-TH-Hist", ext = "png"),
      width = 1500, height = 1200)
  seqdplot(seq_TH, with.legend = TRUE, legend.prop = 0.1,
           border = NA, space = 0)
  dev.off()

### ╠ FIG 6.4: Descripción de secuencias por Entorno + Nivel educativo  ---------------------------

  cli::cli_li(col_yellow("Figure 6.4: Descripción de secuencias por Entorno según Nivel educativo"))

  aux_seq <- attr(seq_TH, "props")
  png(filename = fs::path("figures", "Movilidad_SA-THxEDU-Hist", ext = "png"),
      width = 4000, height = 1500)
  seqdplot(seq_TH, group = aux_seq$EDU, rows = 2, cols = 3,
           with.legend = TRUE, legend.prop = 0.1,
           border = NA, space = 0)
  dev.off()

  rm(seq_ACT, seq_TH, aux_seq)

### ╠ FIG 6.5: Mapeo de zonas más visitadas --------------------------------

  cli::cli_li(col_yellow("Figure 6.5: Mapeo de zonas más visitadas"))

  p5 <- CARTO$FRAC |>
    left_join(ENMODO$prop$prop_in, by = c("ID_FRAC" = "DES")) |>
    mutate(A = st_area(geometry) |> set_units("km2") |> drop_units(),
           des_time = in_time / A) |>
    ggplot() +
    geom_sf(aes(fill = des_time), color = NA, alpha = 0.8) +
    geom_sf(data = CARTO$DPTO, fill = NA, color = "grey80" ) +
    geom_sf(data = CARTO$FFCC1 |> filter(sentido == 1),
            color = "grey25", alpha = 0.5, linewidth = 0.2) +
    geom_sf(data = CARTO$FFCC2, color = "grey25",
            alpha = 0.5, size = 0.75) +
    scale_fill_viridis_c(paste0("Tiempo total\nen el destino\n(miles de hs./km²)"),
                         direction = 1, option = "viridis",
                         na.value = "#08014e",
                         labels = ~format(.x, scientific = F),
                         trans = 'log10') +
    coord_sf(xlim = AUX$lim_AMBA[c(1,3)],
             ylim = AUX$lim_AMBA[c(2,4)]) +
    theme_void()

  ggsave(filename = fs::path("figures", "Movilidad-Destino-tiempo-total", ext = "png"),
         plot = p5, width = 17, height = 15, units = "cm", dpi = 300)

  rm(p5)

### ╠ FIG 6.6: Destino de viaje según nivel educativo ---------------------------

  cli::cli_li(col_yellow("Figure 6.6: Destino de viaje según nivel educativo"))

  prom <- db_mim |>
    summarise(across(ends_with("_t"),
                     ~weighted.mean(.x, w = in_time ) ) ) |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "p")

  db_edu <- CARTO$FRAC["ID_FRAC"] |>
    right_join(ENMODO$prop$prop_in, by = c("ID_FRAC" = "DES" ) ) |>
    filter(n >= 4) |>
    select(ID_FRAC, in_PRI_t:in_SUP_t, in_ls)

  p6 <- map2(.x = prom$var, .y = prom$p, PhD_ggmobility, db = db_edu)

  p6 <- ggarrange(plotlist = p6, ncol = 3,
                  labels = c("Primario", "Secundario", "Superior") )

  ggsave(filename = fs::path("figures", "Movilidad_Destino-x-nivel-educativo",
                             ext = "png"),
         plot = p6, width = 35, height = 15, units = "cm", dpi = 300)

  rm(p6, prom, db_edu)

# ══► Segregación -------------------------------------------

### ╠ FIG 6.7: Segregación de actividades por hora del día (M) ---------------------

  cli::cli_li(col_yellow("Figure 6.7: Segregación de actividades por hora del día (M)"))

  aux_M <- bind_rows(MOV$MOV4 ,
                     MOV$MOV4[1, ] |> mutate(Hora = "24h")) |>
    select(Hora:structural) |>
    mutate(Hora = str_remove(Hora, "h") |> as.numeric())

  p7a <- aux_M |>
    ggplot(aes(x = Hora, y = M1)) +
    annotate("rect", xmin = c(0, 18), xmax = c(7, 24), ymin = 0, ymax = Inf,
             alpha = 0.3, fill = "royalblue4") +
    geom_line(color = "grey") +
    geom_point(color = "darkgreen") +
    geom_hline(yintercept = aux_M$M2[1], linetype = "dashed", color = "tomato4") +
    geom_text(label = "Segregación residencial", color = "tomato4",
              x = 11, y = aux_M$M2[1] + 0.005) +
    labs(x = "Hora del día", y = "Segregación de actividades") +
    scale_x_continuous(breaks = seq(0, 24, by = 3), limits = c(0, 24),
                       minor_breaks = seq(1, 24, 1)) +# labels = paste0(0:24, "h")) +
    scale_y_continuous(limits = c(0, 0.21), minor_breaks = seq(1, 24, 1),
                       breaks = seq(0, 0.2, by = 0.02)) +
    theme_minimal()

  p7b <- aux_M |>
    ggplot(aes(x = Hora)) +
    annotate("rect", xmin = c(0, 18), xmax = c(7, 24), ymin = 0, ymax = Inf,
             alpha = 0.3, fill = "royalblue4") +
    geom_point(data = data.frame(x = c(-1,-1), y = c(-1,-1), color = factor(1:2)),
               aes(x, y, color = color)) +
    geom_line(aes(y = structural), color = "grey") +
    geom_point(aes(y = structural), color = "tomato4") +
    geom_line(aes(y = diff), color = "grey") +
    geom_point(aes(y = diff), color = "darkblue") +
    scale_color_manual("Diferencia de segregación",
                       values = c("tomato4", "darkblue"),
                       labels = c("Estructural", "Absoluta")) +
    scale_x_continuous("Hora del día", breaks = seq(0, 24, by = 3),
                       limits = c(0, 24), minor_breaks  = seq(1, 24, 1)) +
    scale_y_continuous("Diferencia de segregación\n(residencial-actividad)",
                       limit = c(0, NA), breaks = seq(0, 0.14, by = 0.02),
                       minor_breaks = seq(1, 24, 1)) +
    theme_minimal() +
    theme(legend.position = "top")

  p7 <- ggpubr::ggarrange(p7a, p7b, ncol = 2) #, labels = c("A", "B"),

  ggsave(p7, filename = fs::path("figures", "Movilidad_Segregacion-x-hora", ext = "png"),
         width = 36, height = 18, units = "cm", dpi = 300)

  rm(p7, p7a, p7b, aux_M)

### ╠ FIG 6.8: Isolation de actividades por hora del día (ETA2) ---------------------

  cli::cli_li(col_yellow("Figure 6.8: Segregación de actividades por hora del día (ETA2)"))

  p8 <- bind_rows(MOV$MOV4, MOV$MOV4[1, ] |> mutate(Hora = "24h")) |>
    select(Hora, Primario, Secundario, Superior) |>
    pivot_longer(cols = Primario:Superior,
                 names_to = "EDU", values_to = "IS") |>
    mutate(Hora = str_remove(Hora, "h") |> as.numeric()) |>
    ggplot(aes(x = Hora, y = IS, color = EDU)) +
    annotate("rect", xmin = c(0, 18), xmax = c(7, 24), ymin = 0, ymax = Inf,
             alpha = 0.3, fill = "royalblue4") +
    geom_line(alpha = 0.5) +
    geom_point(alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 24, by = 3), limit = c(0, 24),
                       minor_breaks = seq(1, 24, 1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.05),
                       limits = c(0, NA), minor_breaks = seq(1, 24, 1)) +
    scale_color_manual(values = c("Primario" = "#E69F00",
                                  "Secundario" = "#56B4E9",
                                  "Superior" = "#1B9E77") ) +
    labs(x = "Hora del día",
         y = "Índice de Aislamiento normalizado (ETA2)",
         color = "Nivel educativo" ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave(p8, filename = fs::path("figures", "Movilidad_Aislamiento-x-hora", ext = "png"),
         width = 18, height = 18, units = "cm", dpi = 300)

  rm(p8)

### ╠ FIG 6.9: Pairwise copresencia de actividades por hora del día (xPy normalizado) ---------------------

  cli::cli_li(col_yellow("Figure 6.9: Copresencia/interacción de actividades por hora del día (xPy_normalizado)"))

  p9 <- bind_rows(MOV$MOV4, MOV$MOV4[1, ] |> mutate(Hora = "24h")) |>
    select(Hora, P_PRI_SEC, P_SEC_SUP, P_PRI_SUP) |>
    pivot_longer(cols = P_PRI_SEC:P_PRI_SUP,
                 names_to = "PAIR_EDU", values_to = "DI") |>
    mutate(Hora = str_remove(Hora, "h") |> as.numeric(),
           PAIR_EDU = factor(PAIR_EDU, levels = c("P_PRI_SEC", "P_SEC_SUP", "P_PRI_SUP"),
                             labels = c("Primario-Secundario",
                                        "Secundario-Superior", "Primario-Superior") )
           ) |>
    ggplot(aes(x = Hora, y = DI, color = PAIR_EDU)) +
    annotate("rect", xmin = c(0, 18), xmax = c(7, 24), ymin = 0, ymax = Inf,
             alpha = 0.3, fill = "royalblue4") +
    geom_line(alpha = 0.5) +
    geom_point(alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 24, by = 3), limit = c(0, 24),
                       minor_breaks = seq(1, 24, 1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                       limits = c(0, NA), minor_breaks = seq(1, 24, 1)) +
    scale_color_manual(values = c("Primario-Secundario" = "#CC7A33",
                                  "Secundario-Superior" = "#b814b8",
                                  "Primario-Superior" = "#8C2F2F")) +
    labs(x = "Hora del día",
         y = "Índice de co-presencia/interacción normalizado entre grupos (xPy*)",
         color = "Segregación entre\npares de grupos") +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave(filename = fs::path("figures", "Movilidad_Pairwise-xPy-norm-x-hora", ext = "png"),
         plot = p9, width = 18, height = 18, units = "cm", dpi = 300)

  rm(p9)

# ══► Matriz bipartita (MCA) -----------------------------------------------------

  res_mca <- MOV$MOV5
  coor <- rbind(res_mca$var$coord, res_mca$quali.sup$coord)
  aux_coor <- coor |>
    as_tibble(rownames = "Categ", .name_repair = "universal") |>
    full_join(AUX$VAR_cat, by = "Categ") |>
    mutate(grupo = str_wrap(grupo, 20),
           Categ = str_replace_all(Categ, "_", " "),
           Categ = str_wrap(Categ, 20))

### ╠ FIG 6.10: Proyección de variables en MCA ---------------------------------

  cli::cli_li(col_yellow("Figure 6.10: Proyección de variables en MCA"))

  p10 <- fviz_mca_var(res_mca,  geom.var = "point", col.quali.sup = NA, alpha.var = 0, title = "") +
    geom_text_repel(aes(x = Dim.1, y = Dim.2, color = grupo, label = Categ),
                    data = aux_coor |> filter(sel == 1)) +
    ggsci::scale_color_d3() +
    labs(color= "Grupo de variables") +
    theme(legend.position = "bottom")

  ggsave(filename = fs::path("figures", "Movilidad_MCA-Variables-Categ", ext = "png"),
         plot = p10, width = 30, height = 30, units = "cm", dpi = 300)

  rm(p10)

### ╠ FIG 6.11: Proyección de unidades por nivel educativo en MCA ----------------

  cli::cli_li(col_yellow("Figure 6.10: Proyección de unidades por nivel educativo en MCA"))

  aux_lab <- c("Primario", "Secundario", "Superior") |>
    set_names(levels(db_mim$p_EDU))

  p11 <- fviz_mca_ind(res_mca, habillage = "p_EDU", title = "",
                     alpha.ind = 0.02, geom.ind = "point",
                     palette = c("#E6550D", "#56B4E9", "#1B9E77")) +
    stat_density_2d(geom = "polygon", contour = TRUE,
                    alpha = 0.25, color = "grey85", bins = 8,
                    aes(fill = p_EDU, group = p_EDU )) +
    geom_text(data = aux_coor |> filter(ilust == 1), size =3,
              aes(x = Dim.1, Dim.2, label = Categ)) +
    facet_wrap(~p_EDU, ncol = 3, labeller = as_labeller(aux_lab)) +
    xlim(c(-1.2, 1.5)) + ylim(c(-1, 1.9)) +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white", color = "black"),
          strip.background = element_rect(fill = "grey85", color = "black"))

  ggsave(filename = fs::path("figures", "Movilidad_MCA-Nivel-educativo", ext = "png"),
         plot = p11, width = 30, height = 15, units = "cm", dpi = 300)

  rm(p11, aux_lab, aux_coor)

### ╠ FIG App F.1: Porcentaje de varianza explicada MCA -----------------------------

  cli::cli_li(col_yellow("Figure App F.1: Porcentaje de varianza explicada MCA"))

  p_f1a <- fviz_eig(res_mca,
                  barcolor = "tomato3", barfill = "tomato3",
                  linecolor = "grey30",
                  addlabels = T, hjust = 0.5,
                  main = "Varianza explicada (Ajuste: Greenacre)",
                  xlab = "Dimensión",
                  ylab = "Porcentaje de varianza explicada [%]")

  p_f1b <- res_mca$eig |>
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

  ggsave(filename = fs::path("figures", "Movilidad_eig", ext = "png"),
         plot = ggarrange(p_f1a, p_f1b),
         width = 30, height = 15, units = "cm")

  rm(p_f1a, p_f1b)

### ╠ FIG App F.2: Correlación entre variables y dimensiones de análisis ------------

  cli::cli_li(col_yellow("Figure App F.2: Correlación entre variables y dimensiones de análisis"))

  var_exp <- paste0("Dim ", 1:4, " (",round(res_mca$eig[1:4, 2], 1), "%)")

  coor_var <- list(Activas = res_mca$var$eta2,
                   Suplementarias = res_mca$quali.sup$eta2) |>
    map(~as_tibble(.x, rownames = "Var", .name_repair = "universal")) |>
    bind_rows(.id = "Rol")

  p_f2a <- coor_var |>
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

  p_f2b <- coor_var |>
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

  ggarrange(p_f2a, p_f2b, common.legend = T, legend = "bottom",
            labels = paste("Dimensiones", c("1 y 2", "3 y 4")) ) |>
    ggsave(file = fs::path("figures", "Movilidad_dist-var", ext = "png"),
           width = 30, height = 15, units = "cm", dpi = 300)

  rm(p_f2a, p_f2b, coor_var, var_exp)

### ╚ FIG App F.3-F.8: Descripción de variables ordinales MCA -------------------------------------

  cli::cli_li(col_yellow("Figure App F.3-F.8: Descripción de variables ordinales MCA"))

  VAR <- AUX$VAR_cat |>
    filter(Ordenada == 1) |>
    mutate(tit = paste(Nivel, var_lab, sep = ": ") )

  coor_ord <- coor[VAR$Categ, ] |>
    as_tibble(rownames = "Variable", .name_repair = "universal") |>
    mutate(lab = str_split_i(Variable, pattern = "_", -1))

  p_f3 <- unique(VAR$Variable) |>
    map(~PhD_fviz_variables(.x, MCA_obj = res_mca,
                            V = VAR, d = coor_ord) ) |>
    set_names(unique(VAR$Variable))

  p_f3$NULO <- ggplot() + theme_void()

  vars <- list(
    # 1. Composición residencial y destino
    c("p_PRI_r_lab", "p_SEC_r_lab", "p_SUP_r_lab",
      "in_PRI_t_lab","in_SEC_t_lab", "in_SUP_t_lab"),
      # c(4:6, 15:17),
    # 2. Segregación residencial y de destino
    c("p_lsRES_lab", "v_DIFSEG_lab", "in_lsRES_lab", "in_ls_lab"),
      # c(3, 10, 22, 25),
    # 3. Tiempo de actividad en destino
    c("in_N_lab", "in_time_lab", "in_timeLAB",
      "in_timeREP", "in_timeREC", "in_timeEST"),
      # c(13:14, 18:21),
    # 4. Ubicación y distancia/tiempo recorrido
    c("p_CENTRALIDAD", "p_POLICENTRAL", "NULO",
      "in_CENTRALIDAD", "in_POLICENTRAL", "in_DIST_to_RESID_lab"),
      # c(7:9, 23:24, 11),
    # 5. Adscriptas y de viaje
    c("p_EDU", "p_EDAD", "NULO",
      "in_TACT_lab", "v_DIST_lab", "v_TVIA_lab")
      # c(1:2, 12)
  )
  # 1. Composición residencial y destino
  ggarrange(plotlist = p_f3[vars[[1]]] ) |>
    ggsave(file = fs::path("figures", "Movilidad_MCA_Variables_ind1", ext = "png"),
           width = 40, height = 20, units = "cm", dpi = 300)

  # 2. Segregación
  ggarrange(plotlist = p_f3[vars[[2]]] ) |>
    ggsave(file = fs::path("figures", "Movilidad_MCA_Variables_ind2", ext = "png"),
           width = 40*2/3, height = 20, units = "cm", dpi = 300)

  # 3. Tiempo de viaje en destino
  ggarrange(plotlist = p_f3[vars[[3]]] ) |>
    ggsave(file = fs::path("figures", "Movilidad_MCA_Variables_ind3", ext = "png"),
           width = 40, height = 20, units = "cm", dpi = 300)

  # 4. Ubicación y recorrido
  ggarrange(plotlist = p_f3[vars[[4]]] ) |>
    ggsave(file = fs::path("figures", "Movilidad_MCA_Variables_ind4", ext = "png"),
           width = 40, height = 20, units = "cm", dpi = 300)

  # 5. adscriptas  y de viaje
  ggarrange(plotlist = p_f3[vars[[5]]] ) |>
    ggsave(file = fs::path("figures", "Movilidad_MCA_Variables_ind5", ext = "png"),
           width = 40, height = 20, units = "cm", dpi = 300)

  rm(VAR, vars, coor, coor_ord, p_f3)
  rm(res_mca)

# Tablas ------------------------------------

### ╠ TAB 6.1 ----------------------

  cli::cli_li(col_yellow("Table 6.1: Porcentaje de destino de los viajes en función del tipo de entorno urbano del destino y el motivo de la actividad de destino (ponderado por minutos totales permanecidos)"))

  MOV$TAB1 <- MOV$MOV1 |>
    select(-sup) |>
    relocate(N, .before = Ratio ) |>
    mutate(Ratio = round(Ratio, 2)) |>
    rename("Entorno urbano de destino" = TH,
           "Superfice (%)" = sup_p, "Total" = N, "Minutos/Km2" = Ratio ) |>
    flextable() |>
    set_caption(caption = paste("Porcentaje de destino de los viajes",
                                "en función del tipo de entorno urbano",
                                "del destino y el motivo de la actividad",
                                "de destino (ponderado por minutos totales",
                                "permanecidos)"),
                autonum = run_autonum(seq_id = "tab", bkm = "segmov-tab1",
                                      bkm_all = T))  |>
    add_header_row(values = c("Entorno urbano de destino",
                              "Superfice (%)",
                              "Entorno urbano de destino según motivo de viaje (%)",
                              "Minutos/Km2"),
                   colwidths = c(1, 1, 5, 1)) |>
    merge_v(j = c(1:3, 8), part = "header") |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 7, part = "all") |>
    align(align ='center', part = 'all') |>
    align(j = 1, align ='left', part = 'all')  |>
    color(j = 2, color = "grey50") |>
    bg(i = nrow(MOV$MOV1), bg = "grey90") |>
    hline(i = nrow(MOV$MOV1) - 1) |>
    bold(j = 7, part = "all") |>
    italic(j = 7, part = "all") |>
    autofit() |>
    fit_to_width(max_width = 6) |>
    flextable::width(j = 1, 2, unit = "cm") |>
    flextable::width(j = c(2,8), 1.8, unit = "cm") |>
    flextable::width(j = c(3:4, 7), 1.2, unit = "cm") |>
    flextable::width(j = 5:6, 1.3, unit = "cm")

### ╠ TAB 6.2 -----------------------------------

  cli::cli_li(col_yellow("Table 6.2: Diferencia promedio de segregación residencial entre lugar de residencia y el lugar de destino"))

  MOV$TAB2 <-  MOV$MOV3 |>
    flextable() |>
    set_caption(caption = paste("Diferencia promedio de los segregación residencial",
                                "entre lugar de residencia y el lugar de destino",
                                "diferenciando por nivel educativo del hogar de",
                                "la persona."),
                autonum = run_autonum(seq_id = "tab", bkm = "segmov-tab2",
                                      bkm_all = T)) |>
    set_header_labels(EDU_red = "Nivel educativo del hogar", PRI = "Primario",
                      SEC = "Secundario", SUP = "Superior",
                      ls = "Diferencia de segregación local\n(ls_DES-ls_RES)") |>
    add_header_row(values = c("Nivel educativo del hogar",
                              "Diferencia de composición (%DES-%RES)",
                              "Diferencia de segregación local\n(ls_DES-ls_RES)"),
                   colwidths = c(1, 3, 1) ) |>
    merge_v(j = c(1, 5), part = "header") |>
    align(j = 1, align ='left', part = 'all')  |>
    align(j = 2:5, align ='center', part = 'header') |>
    bold(i = nrow(MOV$MOV3), part = "body") |>
    italic(i = nrow(MOV$MOV3), part = "body") |>
    bg(i = nrow(MOV$MOV3), bg = "grey90") |>
    hline(i = nrow(MOV$MOV3) - 1) |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 7, part = "all") |>
    flextable::width(j = 1, 2.5, unit = "cm") |>
    flextable::width(j = c(2:4), 1.8, unit = "cm") |>
    flextable::width(j = 5, 2.7, unit = "cm")

### ╠ TAB 6.3 ---------------------

  cli::cli_li(col_yellow("Table 6.3: Descomposición de la diferencia entre segregación residencial y de destino"))

  d <- MOV$MOV2$est[MOV$MOV2$stat == "diff"]
  f <- paste("Los valores tienen una corrección de sesgo a partir del cálculo de",
             replic, "iteraciones con Bootstrap. Todos los valores tienen",
             "una significancia estadística superior al ", conf, "%.") |>
    str_wrap(60)

  lab <- data.frame(stat = MOV$MOV2$stat,
                    st = c("Segregación de destino (M1)", "Segregación residencial (M2)",
                           "Diferencia (M2-M1)", "Unidades agregadas",
                           "Unidades removidas", "Marginal de grupos",
                           "Marginal de unidades", "Estructural"))
  MOV$TAB3 <- MOV$MOV2 |>
    left_join(lab, by = "stat") |>
    mutate(p = ifelse(stat %in% c("M1", "M2"), NA, est / d * 100) ) |>
    select(st, est, p) |>
    flextable() |>
    set_caption(caption = paste("Descomposición de la diferencia entre la",
                                "segregación residencial y la segregación de",
                                "destinos"),
                autonum = run_autonum(seq_id = "tab", bkm = "segmov-tab3",
                                      bkm_all = T)) |>
    set_header_labels(st = " ", est = "Segregación (M)", p = "Δ%") |>
    add_footer_lines(values = "Método de esimación: Shapley.") |>
    flextable::footnote(i = 1, j = 2, part = "header", ref_symbols = "a",
                        value = as_paragraph(f) ) |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 7, part = "all") |>
    colformat_double(j = 3 , digits = 1) |>
    align(i = 1:2, j = 2, align = "center") |>
    align(i = 3:8, j = 2, align = "right") |>
    align(i = 4:8, j = 1, align = "right") |>
    hline(i = 2:3) |>
    italic(4:8) |>
    color(4:8, color = "grey60") |>
    autofit()

  rm(d, f, lab)

### ╚ TAB App E.1 ---------------------------------------

  cli::cli_li(col_yellow("Table App E: Listado de variables usadas en el MCA"))

  f <- paste("Notas:", "ID: Variables identificadoras;", "w: Ponderador;",
             "x: Variable activa en el análisis;",
             "Sup.: Variable suplementaria (proyectada)") |>
    str_wrap(150)

  var <- AUX$VAR_mca |>
    filter(!is.na(MCA) & MCA != "Cuanti") |>
    rename('#' = '# Categorías') |>
    mutate(Indicador = str_wrap(Indicador, 40),
           "Fuente de datos" = str_wrap(`Fuente de datos`, 15) )

  l <- var |>
    transmute(com = Nivel != lead(Nivel),
              com = ifelse(is.na(com), F, com))

  MOV$appE_TAB1 <- var |>
    flextable() |>
    set_caption(caption = paste("Variables usadas en el Análisis de",
                                "Correspondencias Múltiples (MCA)"),
                autonum = run_autonum(seq_id = "tab", bkm = "appE-listado",
                                      bkm_all = T)) |>
    align(j = 3:5, align = "center", part = "all") |>
    flextable::footnote(j = 5, ref_symbols = "a", part = "header",
                        value = as_paragraph(f) ) |>
    merge_v(j = 1) |>
    valign(j = 1:2, valign = "top", part = "body") |>
    hline(i = l$com, border = fp_border(color="grey", width = 0.5) ) |>
    flextable::font(fontname='Alegreya Sans', part = "all") |>
    flextable::fontsize(size = 7, part = "all") |>
    flextable::width(j = 1, 2, unit = "cm") |>
    flextable::width(j = 2, 3.3, unit = "cm") |>
    flextable::width(j = 3, 1.5, unit = "cm") |>
    flextable::width(j = 4:5, 1.1, unit = "cm") |>
    flextable::width(j = 6, 1.7, unit = "cm")

  rm(var, f, l)

# Guardar resultados ----------------------------------------------------

  MOV$replic <- replic
  MOV$conf <- conf

  usethis::use_data(MOV, overwrite = TRUE)

  rm(db_mim, replic, conf)

# DRAFT ----------------------------
### ╠ FIG 6.?: Mapeo de segregación local de destino ---------------------------

  # p <- db_edu |>
  #   ggplot() +
  #   geom_sf(aes(fill =  in_ls), colour = NA) +
  #   geom_sf(data = CARTO$DPTO, fill = NA, color = "grey10" ) +
  #   scale_fill_gradient2("Segregación local\nen destino",
  #                        low = "#dedede", mid = "#dad9be", high = "#aa0808",
  #                        midpoint = median(db_edu$in_ls)
  #   ) +
  #   coord_sf(xlim = AUX$lim_AMBA[c(1,3)],
  #            ylim = AUX$lim_AMBA[c(2,4)]) +
  #   theme_void()
  #
  # ggsave(filename = "figures/Destino-Segregacion-local.png", p,
  #        width = 17, height = 15, units = "cm", dpi = 300)
  #
  # rm(p, db_edu)

### ╠ FIG 6.?: Matriz TH destino y origen por nivel educativo -----------

  # p <- db_mim |>
  #   group_by(in_TH, p_TH, p_EDU) |>
  #   summarise(N = sum(in_time, na.rm = T)) |>
  #   group_by(p_TH, p_EDU) |>
  #   mutate(p = round(N/sum(N) * 100, 1)) |>
  #   ggplot(aes(x = in_TH, y = p_TH, fill = p, label = p)) +
  #   geom_tile() +
  #   geom_text(col = "black") +
  #   facet_wrap(~p_EDU) +
  #   theme_minimal() +
  #   scale_fill_gradient2(low = "white", mid = "#FFBE33", high = "tomato3",
  #                        midpoint = 30, na.value = "white")
  #
  # rm(p)
  #
  # ggsave(filename = "figures/TH-Destino-x-origen-x-nivel-educativo.png", p,

### ╠ FIG 6.?: Comparación entre segregación residencial de residencia y lugar de destino -------------------------

  # # Diagrama ternario
  # db_diffseg |>
  #   filter(!is.na(diff_PRI)) |>
  #   mutate(across(diff_PRI:diff_SUP, ~(.x+100)/3)) |>
  #   ggtern::ggtern(aes(x = diff_PRI, y = diff_SEC, z = diff_SUP)) +
  #   geom_point(aes(color = diff_ls/100, alpha = 0.0001)) +
  #   # geom_point(aes(x = 100/3, y = 100/3, z = 100/3), color = "black", shape = 3) +
  #   facet_wrap(~EDU_red, ncol = 3) +
  #   labs(x = "Diferencia\nPrimario", y = "Diferencia\nSecundario",
  #        z = "Diferencia\nSuperior", color = "Diferencia de segregación",) +
  #   scale_color_gradient2(low = "darkblue", mid = "#FEFDD0", high = "tomato4",
  #                         midpoint = 0, na.value = "grey80") +
  #   guides(size = "none", alpha = "none") +
  #   ggtern::scale_L_continuous()
  #   # ggtern::scale_L_continuous(breaks = seq(0, 1, by= 1/6), labels = seq(-100, 100, 50)) +
  #   # ggtern::scale_R_continuous(breaks = seq(0, 1, by= 0.25), labels = seq(-100, 100, 50)) +
  #   # ggtern::scale_T_continuous(breaks = seq(0, 1, by= 0.25), labels = seq(-100, 100, 50)) +
  #   theme_minimal() +
  #   theme(legend.position = "bottom")

  # # Boxpot / violin
  # db_diffseg |>
  #   filter(!is.na(diff_PRI)) |>
  #   pivot_longer(cols = diff_PRI:diff_SUP) |>
  #   ggplot(aes(x=name, y = value)) +
  #   geom_jitter(aes(color = diff_ls, size = PONDERA_time),
  #               alpha = 0.01, width = 0.2) +
  #   geom_violin(trim = F, fill = NA) +
  #   facet_wrap(~EDU_red) +
  #   scale_color_gradient2(low = "darkblue", mid = "#FEFDD0", high = "tomato4",
  #                         midpoint = 0, na.value = "grey80") +
  #   theme_minimal()
  #
  # rm(db_diffseg)


### Comparación de segregación destino y de hogares por TH ----------------

  left_join(
    # Residencial
    bind_rows(
      mutual_total(db_hog, group = "name", unit = "ID_FRAC",  weight = "value", se = F) |>
        filter(stat == "M") |> mutate(stat = "TOTAL") |> select(stat, est),
      mutual_total(db_hog, group = "name", unit = "TH", weight = "value", se = F) |>
        filter(stat == "M") |> mutate(stat = "Between") |> select(stat, est),
      mutual_total(db_hog, group = "name", unit = "ID_FRAC", within = "TH", weight = "value") |>
        filter(stat == "M") |> mutate(stat = "Within") |> select(stat, est),
    ) |>
      mutate(est = round(est, 3),
             p = est / sum(est[1]) * 100) |>
      select(stat, est_res = est, p_res = p),

    # Destinos
    bind_rows(
      mutual_total(db_dest, group = "name", unit = "DES",  weight = "value", se = F) |>
        filter(stat == "M") |> mutate(stat = "TOTAL") |> select(stat, est),
      mutual_total(db_dest, group = "name", unit = "in_TH", weight = "value", se = F) |>
        filter(stat == "M") |> mutate(stat = "Between") |> select(stat, est),
      mutual_total(db_dest, group = "name", unit = "DES", within = "in_TH", weight = "value") |>
        filter(stat == "M") |> mutate(stat = "Within") |> select(stat, est),
    ) |>
      mutate(est = round(est, 3),
             p = est / est[1] * 100) |>
      select(stat, est_des = est, p_des = p)
  ) |>
    as_tibble()

  left_join(
    mutual_within(db_hog, group = "name", unit = "ID_FRAC",
                  within = "TH", weight = "value", wide = TRUE ) |>
      select(TH, M_hog = M),
    mutual_within(db_dest, group = "name", unit = "DES",
                  within = "in_TH", weight = "value", wide = TRUE) |>
      select(TH = in_TH, M_dest = M),
  ) |>
    mutate(diff = round(M_dest - M_hog, 3) ) |>
    as_tibble() |>
    arrange(-abs(diff))

  segplot(db_dest, group = "name", unit = "DES",  weight = "value")
