# Cargar población
# ---------------*

# load("D:/Escritorio/PhD_data/PhD_02_urban-net.RData")

cli_h2(col_blue("Cargando datos de población (CENSO)"))
cli_alert_info(col_silver("Basado en script TP Dalle: {.path script/ES_01_data.R}.") )

# ╠ Cargar datos CENSO ------------------------------------------------------
# ver código para generar datos en:
#   ./_rawdata/CENSO_raw-to-derived-data
# Versión en disco
#   C:/Users/este.de.ahora/Documents/00_DATA/CENSO AMGBA/CENSO_raw-to-derived-data
# Versión en drive
#   https://drive.google.com/drive/folders/1Ca-jhFQfCO954bclRiTLX404S1kZ3g-z?usp=drive_link

cli_li(col_yellow("Cargando datos del CENSO."))
cli_alert_info(col_silver("Código para generar los datos: {.path rawdata/CENSO_raw-to-derived-data.R}.)") )

if(Sys.info()[["nodename"]] == "DESKTOP-VP1IN9U"){
  # PC de Escritorio
  cli_alert_info(col_silver("Datos tomados de PC Escritorio."))
  load("D:/Escritorio/PhD_data/AMGBA.RData")
}else{
  # Laptop
  cli_alert_info(col_silver("Datos tomados de Laptop."))
  load("C:/Users/este.de.ahora/Documents/00_DATA/CENSO AMGBA/derived_data/AMGBA.RData")
}

# ╠ Base cónyuges (CENSO_CON) ----------------------------------------------
# Retiene sólo cónyuges (P01 %in% 1:2)

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
# Base de hogares x cónyuges con educación categórica

# Cada fila es un hogar con:
#   - ID:         ID del radio.
#   - EDU:        Máximo nivel educativo entre PSH y CONYUGE (incluye hogares no-conyugales).
#   - SEG1:       Indica si P1 == P2  (homogamia para MNI discreto).
#   - P1 / P2:    Los cónyuges ordenados de nivel educativo mayor a menor en base a
#                     MNI categórico. NA para hogares no-conyugales.
#   - ***_p:      Indica cantidad de cónyuges con nivel educativo *** (si type = "P"),
#                     o bien, existencia de cónyuge con nivel educativo *** (si type = "H").
#                     NA para hogares no-conyugales).
#   - EST_PRI

cli_li(col_yellow("Armado de base de hogares (HOG)."))

# pivot_edu(): Genera dataframe con personas por nivel educativo en cada hogar conyugal
pivot_edu <-  function(db, type = "P"){

  niveles <- 1:3
  names(niveles) <- paste0(c("PRI", "SEC", "SUP"), "_p")

  if(type == "H"){
    # Probabilidad del hogar
    # P(x) DE QUE DADO QUE EN UN HOGAR HAYA UNA PERSONA DE MNI_X, HAYA 2 PERSONAS DE MNI_X
    res <- map(niveles, \(x) ifelse(db$P1 == x | db$P2 == x, T, F)) |>
      as.data.frame()

  }else if(type == "P") {
    # Probabilidad individual
    # P(x) DE QUE DADA UNA PERSONA DE MNI_X, TENGA UNA PAREJA DE IGUAL MNI
    res <- map(niveles, \(x) (db$P1 == x) + (db$P2 == x) ) |>
      as.data.frame()
  }else{
    stop("'type' inválido. Debe ser 'H' o 'P'.")
  }
  return(res)
}

HOG <- CENSO_CON |>
  pivot_wider(id_cols = c("ID_RADIO", "VIVIENDA_REF_ID", "HOGAR_REF_ID"),
              names_from = "REL", values_from = MNI3) |>
  mutate(PROV = str_sub(ID_RADIO, 1, 2),
         PROV = factor(PROV, levels = c("02", "06"),
                       labels = c("CABA", "PBA")),
         # Si MNI-PSH == MNI-CON → T
         SEG1 = ifelse(CON == PSH, T, F),
         # P1 y P2 indican menor y maypr MNI para parejas
         P1 = pmin(PSH, CON),
         P2 = pmax(PSH, CON),
         # EDU indica MNI del hogar (sea conyugal o no)
         EDU = pmax(PSH, CON, na.rm = T) )

# Cantidad de niños/as x hogar en edad primaria (P03 %in% 6:11) / secundara (P03 %in% 13:17)
ESTUD <- CENSO |>
  group_by(ID_RADIO, VIVIENDA_REF_ID, HOGAR_REF_ID) |>
  summarise(EST_PRI = sum(P03 %in% 6:11),
            EST_SEC = sum(P03 %in% 13:17),
            .groups = "drop")

HOG <- HOG |>
  cbind(pivot_edu(HOG, type = "P")) |>
  left_join(ESTUD, by = c("ID_RADIO", "VIVIENDA_REF_ID", "HOGAR_REF_ID")) |>
  select(ID_RADIO, PROV, EDU,
         P1, P2, SEG1, ends_with("_p"),
         EST_PRI, EST_SEC)

rm(pivot_edu, ESTUD)

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
