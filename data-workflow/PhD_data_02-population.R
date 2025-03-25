# Cargar población
# ---------------*

cli_h2(col_blue("Cargando datos de población (CENSO)"))
cli_alert_info(col_silver("Basado en script TP Dalle.") )

if(!exists("CARTO", inherits = F)){
  cli_alert_info(
    col_silver(
      paste0("Cargando cartografía",
             ": {.path data-workflow/intermediate-data/PhD_02_carto.RData}.")
      )
    )

  load(here::here("data-workflow", "intermediate-data",
                  "PhD_02_carto.RData"))
}

# ╠ Cargar datos CENSO ------------------------------------------------------
# Respaldo de datos disponible en: https://doi.org/10.5281/zenodo.14876596

  cli_li(col_yellow("Cargando datos del CENSO."))
  cli_alert_info(col_silver("Código para generar los datos: {.path data-workflow/CENSO_raw-to-derived-data.R}.") )

  if(!fs::file_exists(here::here("data-workflow", "datafolder.txt"))){

    # Descargar datos de Zenodo
    folder_temp <- tempdir()
    inborutils::download_zenodo(doi = "10.5281/zenodo.14876596",
                                folder_temp)

    CENSO_file <- fs::path(folder_temp, "AMGBA", ext = "RData")
    rm(folder_temp)

  }else{
    CENSO_file <- fs::path(readLines("data-workflow/datafolder.txt")[1],
                            readLines("data-workflow/datafolder.txt")[2],
                            "AMGBA.RData")
  }

  load(CENSO_file)
  rm(CENSO_file)

  # Quitar casos fuera del AMBA (Gran La Plata)
  CENSO <- CENSO |>
    filter(ID_RADIO %in% CARTO$RADIO$ID_RADIO)

# ╠ Base cónyuges (CENSO_CON) ----------------------------------------------
# Cada fila es un cónyuges (P01 %in% 1:2)
# * Se construyen niveles educativos MNI2 y MNI3

  cli_li(col_yellow("Armado de base de cónyuges (`CENSO_CON`)."))

  CENSO_CON <- CENSO |>
    # Filtrado de PSH y cónyuge
    filter(P01 %in% 1:2) |>
    #Construir niveles MNI
    mutate(REL = factor(P01, levels = 1:2, labels = c("PSH", "CON")),
           # MNI2: PRI/SEC/SNU/UNI - Ciclo cumplidos
           MNI2 = case_when(MNI %in% c(0:3, 5) ~ 1,
                            MNI %in% c(4, 7, 9) ~ 2,
                            MNI %in% c(6) ~ 3,
                            MNI %in% c(8) ~ 4),
           # MNI3: PRI/SEC/SUP (SNU + UNI) - Ciclo cumplidos
           MNI3 = ifelse(MNI == 8, 3, MNI2)
    ) |>
    left_join(st_drop_geometry(CARTO$RADIO[c("ID_RADIO", "TH")]),
              by = "ID_RADIO") |>
    select(ID_RADIO, TH, VIVIENDA_REF_ID, HOGAR_REF_ID, REL, MNI3)

# ╠ Base por Hogares (HOG) ----------------------------------------------
# Revisar:
# Existen hogares donde no se declara relación de parentesco (por lo cual
#     pueden tener niños en edad escolar, pero no PSH o cónyuge). Dado que
#     se trata de un número relativamente bajo de hogares (3.597) y con poca
#     concentración espacial, son ignorados en el análisis.
# Código
# ```
#   a1 <- ESTUD |> dplyr::transmute(ID = paste(ID_RADIO, VIVIENDA_REF_ID, HOGAR_REF_ID, sep = "_"), est = T)
#   a2 <- HOG |> dplyr::transmute(ID = paste(ID_RADIO, VIVIENDA_REF_ID, HOGAR_REF_ID, sep = "_"), hog = T)
#   a3 <- full_join(a1, a2, by = "ID")
#   a3 |> filter(is.na(hog)) |> nrow()
#   a3 |> filter(is.na(est)) |> nrow()
#   rm(a1, a2, a3)
# ```

# Base de hogares con información de educación de cónyuges y
#     educación y homogamia del hogar.
# Cada fila es un hogar con:
#   - ID_RADIO:     ID del radio
#   - TH:           Entorno urbano
#   - PROV:         Provincia (CABA o PBA)
#   - EDU:          Máximo nivel educativo entre PSH y CONYUGE (incluye hogares
#                       no-conyugales).
#   - P1 / P2:      Los cónyuges ordenados de nivel educativo mayor a menor en
#                       base a MNI categórico. NA para hogares no-conyugales.
#   - HOM:          Indica si P1 == P2 (homogamia para hogares conyugales). NA
#                       para hogares no-conyugales.
#   - ***_hogcount: Indica cantidad de cónyuges con nivel educativo *** (si
#                       type = "P"), o bien, existencia de cónyuge con nivel
#                       educativo *** (si type = "H"). NA para hogares
#                       no-conyugales).
#   - EST_PRI/SEC:  Cantidad de personas con edad escolar primaria (de 6 a 12)
#                       y secundaria (de 13 a 17) en el hogar.

  cli_li(col_yellow("Armado de base de hogares (`HOG`)."))

  # Cantidad de niños/as x hogar en edad primaria (P03 %in% 6:12)
  #   y en edad secundara (P03 %in% 13:17)
  ESTUD <- CENSO |>
    group_by(ID_RADIO, VIVIENDA_REF_ID, HOGAR_REF_ID) |>
    summarise(EST_PRI = sum(P03 %in% 6:12),
              EST_SEC = sum(P03 %in% 13:17),
              .groups = "drop")

  HOG <- CENSO_CON |>
    pivot_wider(id_cols = c("ID_RADIO", "TH", "VIVIENDA_REF_ID", "HOGAR_REF_ID"),
                names_from = "REL", values_from = MNI3) |>
    mutate(PROV = str_sub(ID_RADIO, 1, 2),
           PROV = factor(PROV, levels = c("02", "06"),
                         labels = c("CABA", "PBA")),
           # HOM: (Si) MNI-PSH == MNI-CON → T
           HOM = ifelse(CON == PSH, TRUE, FALSE),
           # P1 y P2: mayor (P1) y menor (P2) MNI de los cónyuges (solo si hay grupo conyugal)
           P1 = pmin(PSH, CON),
           P2 = pmax(PSH, CON),
           # EDU: MNI del hogar (sea conyugal o no)
           EDU = pmax(PSH, CON, na.rm = T) ) |>
    PhD_homog_hog("P") |> # reemplaza: PhD_pivot_edu()
    left_join(ESTUD, by = c("ID_RADIO", "VIVIENDA_REF_ID", "HOGAR_REF_ID")) |>
    select(ID_RADIO, TH, PROV, EDU,
           P1, P2, HOM, ends_with("_hogcount"),
           EST_PRI, EST_SEC)

  rm(ESTUD)

# ╠ Base por RADIO (RAD) ----------------------------------------
# Cada fila es un radio con:
#   - ID_RADIO:           ID del radio
#   - TH:                 Entorno urbano
#   - HOG:                Cantidad de hogares total
#   - HOG_CON:            Cantidad de hogares conyugales
#   - HOG_HOM:            Cantidad de hogares con homogamia
#   - HOG_PRI/SEC/SUP:    Cantidad de hogares con nivel educativo PRI/SEC/SUP
#   - n_estPRI/SEC:       Cantidad de niños en edad primaria (de 6 a 12) /
#                             secundaria (de 13 a 17)
#   - p_con:              Proporción de hogares conyugales (sobre total de hogares)
#   - p_hom:              Proporción de hogares con homogamia (sobre total de
#                             hogares conyugales)
#   - p_hom_PRI/SEC/SUP:  Proporción de hogares con homogamia por nivel educativo

  cli_li(col_yellow("Armado de base por radio (`RAD`)."))

  RAD <- HOG |>
    group_by(ID_RADIO, TH) |>
    summarise(
      # Total de hogares
      HOG = n(),
      # Cantidad de niños en edad educativa (estudiantes)
      n_estPRI = sum(EST_PRI),
      n_estSEC = sum(EST_SEC),
      # Cantidad de hogares por nivel educativo del hogar (EDU: más alto de ambos)
      HOG_PRI = sum(EDU == 1, na.rm = T),
      HOG_SEC = sum(EDU == 2, na.rm = T),
      HOG_SUP = sum(EDU == 3, na.rm = T),
      # Cantidad de 'hogares conyugales' (= contienen PSH y cónyuge).
      HOG_CON = sum(!is.na(HOM)),
      # Proporción de hogares conyugales (sobre total de hogares).
      p_con = HOG_CON/HOG,
      # Cantidad de 'hogares con homogamia' ( = (P1 == P2) = (HOM == T) ).
      HOG_HOM = sum(HOM, na.rm = T),
      # Proporción de hogares con homogamia (sobre total de hogares conyugales).
      p_hom = HOG_HOM / HOG_CON,
      # Proporción de hogares con homogamia (por grupo educativo),
      #   sobre el total de hogares conyugales.
      p_hom_PRI = sum((HOM & (P1 == 1)), na.rm = T) / HOG_CON,
      p_hom_SEC = sum((HOM & (P1 == 2)), na.rm = T) / HOG_CON,
      p_hom_SUP = sum((HOM & (P1 == 3)), na.rm = T) / HOG_CON,
      # Probabilidad de homogamia (por grupo educativo)
      across(.cols = ends_with("_hogcount"),
             .fns  = ~sum(.x * HOM, na.rm = T) / sum(.x, na.rm = T)),
      across(.cols  = ends_with("_hogcount"),
             .fns   = ~PhD_conv_phom(.x, conversion = "to_P2"), # P2 <- P1 / ( 2 - P1 )
             .names = "{col}2"),
      .groups = "drop") |>
    rename_with(.fn = ~paste0(., "1"), .cols = ends_with("_hogcount")) |>
    rename_with(.fn = ~str_replace(., "_hogcount", "_p"), .cols = matches("_hogcount[0-9]")) |>
    relocate(HOG, HOG_CON, HOG_HOM, HOG_PRI:HOG_SUP, .after = TH)

  # RAD2 <- CENSO_CON |>
  #   group_by(ID_RADIO) |>
  #   summarise(head_PRI = sum(MNI3 == 1),
  #             head_SEC = sum(MNI3 == 2),
  #             head_SUP = sum(MNI3 == 3)) |>
  #   mutate(n_head = head_PRI + head_SEC + head_SUP) |>
  #   ungroup()
  # rm(RAD2)

# ╚ Base de segregación por RADIO (Ls) -------------------------------

  cli_li(col_yellow("Armado de base de segregación local por radio."))

  local_seg <- RAD |>
    filter(!is.na(TH)) |>
    select(ID_RADIO, HOG_PRI:HOG_SUP) |>
    pivot_longer(cols = HOG_PRI:HOG_SUP, names_to = "EDU", values_to = "total") |>
    mutual_local(group = "EDU", unit = "ID_RADIO", weight = "total",
                 wide = TRUE, se = TRUE, n_bootstrap = 1000) |>
    select(ID_RADIO, ls, p)

  # Ls <- CENSO_CON |>
  #   select(ID_RADIO, MNI3) |>
  #   mutual_local(group = "MNI3", unit = "ID_RADIO",
  #                wide = T, se = T) |>
  #   select(ID_RADIO, ls, p)

# Bases finales ---------------------------------------------------

  cli_li(col_yellow("Sumar datos de población a base `CARTO$RADIO`"))

  CARTO$RADIO  <- CARTO$RADIO |>
    left_join(local_seg, by = "ID_RADIO") |>
    left_join(RAD, by = c("ID_RADIO", "TH"))

  rm(RAD, local_seg)
  rm(CENSO)

  cli_alert_info(col_silver("Guradando {.path data-workflow/intermediate-data/PhD_03_carto-censo.RData}."))
  save(CARTO,
       file = here::here("data-workflow", "intermediate-data",
                         "PhD_03_carto-censo.RData"))

  cli_alert_info(col_silver("Guradando datos finales {.path HOG.rda} y {.path CENSO_CON.rda}."))
  # usethis::use_data(HOG, CENSO_CON, overwrite = TRUE, internal = FALSE)
  save(HOG, file = here::here("data-workflow", "final-data", "HOG.rda"))
  save(CENSO_CON, file = here::here("data-workflow", "final-data", "CENSO_CON.rda"))
  rm(HOG, CENSO_CON)
