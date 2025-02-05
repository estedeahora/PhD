# Cargar población
# ---------------*

cli_h2(col_blue("Cargando datos de población (CENSO)"))
cli_alert_info(col_silver("Basado en script TP Dalle: {.path script/ES_01_data.R}.") )

if(!exists("CARTO", inherits = F) | !exists("CALLE", inherits = F)){
  cli_alert_info(
    col_silver(
      paste0("Cargando cartografía",
             ": {.path data-raw/intermediate-data/PhD_02_carto.RData}.")
      )
    )

  load(here::here("data-raw", "intermediate-data",
                  "PhD_02_carto.RData"))
}
# ╠ Cargar datos CENSO ------------------------------------------------------

  cli_li(col_yellow("Cargando datos del CENSO."))
  cli_alert_info(col_silver("Código para generar los datos: {.path data-raw/CENSO_raw-to-derived-data.R}.") )

  load("C:/Users/este.de.ahora/Documents/00_DATA/CENSO AMGBA/derived_data/AMGBA.RData")

  # Quitar casos fuera del AMBA (Gran La Plata)
  CENSO <- CENSO |>
    filter(ID_RADIO %in% CARTO$RADIO$ID_RADIO)

# ╠ Base cónyuges (CENSO_CON) ----------------------------------------------
# * Se retiene sólo cónyuges (P01 %in% 1:2)
# * Se construyen niveles educativos MNI2 y MNI3

  cli_li(col_yellow("Armado de base de cónyuges (CENSO_CON)."))

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
    left_join(st_drop_geometry(CARTO$RADIO[c("ID_RADIO")]),
              by = "ID_RADIO") |>
    select(ID_RADIO, VIVIENDA_REF_ID, HOGAR_REF_ID,
           REL, MNI3)

# ╠ Base por Hogares (HOG) ----------------------------------------------
# * Existen hogares donde no se declara relación de parentesco (por lo cual
#     pueden tener niños en edad escolar, pero no PSH o cónyuge). Dado que
#     se trata de un número relativamente bajo de hogares (3597) y con poca
#     concentración espacial, son ignorados en el análisis.

# Base de hogares con educación categórica de cónyuges
# Cada fila es un hogar con:
#   - ID_RADIO:   ID del radio
#   - PROV:       Provincia (CABA o PBA)
#   - EDU:        Máximo nivel educativo entre PSH y CONYUGE (incluye hogares no-conyugales).
#   - HOM:        Indica si P1 == P2  (homogamia para hogares conyugales).
#   - P1 / P2:    Los cónyuges ordenados de nivel educativo mayor a menor en base a
#                     MNI categórico. NA para hogares no-conyugales.
#   - ***_p:      Indica cantidad de cónyuges con nivel educativo *** (si type = "P"),
#                     o bien, existencia de cónyuge con nivel educativo *** (si type = "H").
#                     NA para hogares no-conyugales).
#   - EST_PRI:    Cantidad

  cli_li(col_yellow("Armado de base de hogares (HOG)."))

  # Cantidad de niños/as x hogar en edad primaria (P03 %in% 6:11)
  #   y en edad secundara (P03 %in% 13:17)
  ESTUD <- CENSO |>
    group_by(ID_RADIO, VIVIENDA_REF_ID, HOGAR_REF_ID) |>
    summarise(EST_PRI = sum(P03 %in% 6:11),
              EST_SEC = sum(P03 %in% 13:17),
              .groups = "drop")

  HOG <- CENSO_CON |>
    pivot_wider(id_cols = c("ID_RADIO", "VIVIENDA_REF_ID", "HOGAR_REF_ID"),
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
           EDU = pmax(PSH, CON, na.rm = T) )

  # a1 <- ESTUD |> dplyr::transmute(ID = paste(ID_RADIO, VIVIENDA_REF_ID, HOGAR_REF_ID, sep = "_"))
  # a2 <- HOG |> dplyr::transmute(ID = paste(ID_RADIO, VIVIENDA_REF_ID, HOGAR_REF_ID, sep = "_"))
  # a3 <- full_join(a1, a2)
  # a3 |> filter(is.na(hog))

  HOG <- HOG |>
    PhD_pivot_edu("P") |>
    left_join(ESTUD, by = c("ID_RADIO", "VIVIENDA_REF_ID", "HOGAR_REF_ID")) |>
    select(ID_RADIO, PROV, EDU,
           P1, P2, HOM, ends_with("_p"),
           EST_PRI, EST_SEC)

  rm(ESTUD)

# ╠ Base por RADIO (RAD) ----------------------------------------
# Para sacar cantidad de un grupo
# phomX * n1/ X_p
#
# Convertir entre P1 y P2
# P2 = P1 / (2 - P1)
# Inversa:
# P1 <- 2 * P2/ (P2 + 1)

cli_li(col_yellow("Armado de base por radio (RAD)."))

RAD <- HOG |>
  group_by(ID_RADIO) |>
  summarise(
    # Total de hogares
    n_hog = n(),
    # Cantidad de niños en edad educativa (estudiantes)
    n_estPRI = sum(EST_PRI),
    n_estSEC = sum(EST_SEC),
    # Cantidad de hogares por nivel EDU (más alto de ambos)
    hog_PRI = sum(EDU == 1, na.rm = T),
    hog_SEC = sum(EDU == 2, na.rm = T),
    hog_SUP = sum(EDU == 3, na.rm = T),
    # Cantidad de hogares conyugales
    n_con = sum(!is.na(SEG1)),
    # Porcentaje de hogares conyugales (sobre total de hogares)
    p_con = n_con/n_hog,
    # Cantidad de hogares con homogamia (total)
    n_hom = sum(SEG1, na.rm = T),
    # Porcentaje de hogares con homogamia (sobre total de hogares conyugales)
    p_hom = n_hom / n_con,
    # Porcentaje de hogares con homogamia (por grupo educativo), sobre total de hogares conyugales
    p_hom1 = sum((SEG1 & (P1 == 1)), na.rm = T) / n_con,
    p_hom2 = sum((SEG1 & (P1 == 2)), na.rm = T) / n_con,
    p_hom3 = sum((SEG1 & (P1 == 3)), na.rm = T) / n_con,
    # Probabilidad de homogamia (por grupo educativo)
    across(.cols = ends_with("_p"),
           .fns  = ~sum(.x * SEG1, na.rm = T) / sum(.x, na.rm = T)),
    across(.cols  = ends_with("_p"),
           .fns   = ~.x / ( 2 - .x ), # P2 <- P1 / ( 2 - P1 )
           .names = "{col}2"),
    .groups = "drop"
    )

RAD2 <- CENSO_CON |>
  group_by(ID_RADIO) |>
  summarise(head_PRI = sum(MNI3 == 1),
            head_SEC = sum(MNI3 == 2),
            head_SUP = sum(MNI3 == 3)) |>
  mutate(n_head = head_PRI + head_SEC + head_SUP) |>
  ungroup()

RAD <- left_join(RAD, RAD2, by = "ID_RADIO") |>
  relocate(n_hog, n_con, n_head, n_hom, .after = ID_RADIO) |>
  relocate(hog_PRI:hog_SUP, .after = last_col() )

rm(RAD2)

# ╚ Base de segregación por RADIO (Ls) -------------------------------
# Cuánto se separa cada radio de la distribución general

cli_li(col_yellow("Armado de base de segregación local por radio."))

Ls <- CENSO_CON |>
  select(ID_RADIO, MNI3) |>
  mutual_local(group = "MNI3", unit = "ID_RADIO",
               wide = T, se = T) |>
  select(ID_RADIO, ls, p)

# Bases finales ---------------------------------------------------

cli_li(col_yellow("Armado de bases finales (población)"))

CARTO$RADIO  <- CARTO$RADIO |>
  left_join(Ls, by = "ID_RADIO") |>
  left_join(RAD, by = "ID_RADIO")

rm(RAD, Ls)
rm(CENSO)

cli_alert_info(col_silver("Guradando {.path PhD_03_CENSO.RData}."))
save.image("D:/Escritorio/PhD_data/PhD_03_CENSO.RData")
