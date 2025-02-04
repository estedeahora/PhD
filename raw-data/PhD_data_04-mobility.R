# Cargar Movilidad
# ---------------*

# load("D:/Escritorio/PhD_data/PhD_04_escuela-con-datos.RData")

cli_h2(col_blue("Cargando datos de movilidad (ENMODO2018)"))
cli_alert_warning("Basado en script TP Goinheix: 'script/SNA_01_data.R'.")

# ulid <- cli_ul()

# Cargar datos ENMODO2018 -------------------------------------------------

cli_li(col_yellow("Cargando datos ENMODO2018"))

cli_ul(col_silver("ENMODO2018 HOGAR") )

HOGAR <- read_excel("_rawdata/raw-data/ENMODO18_Hogar.xlsx") |>
  rename(ID_RADIO = RADIO_CENSAL_hogar) |>
  # Correcciones manuales
  mutate(ID_RADIO = ifelse(ID_RADIO == "60281222", "060281202", ID_RADIO ))

cli_ul(col_silver("ENMODO2018 PERSONA") )

PERSONA <- read_excel("_rawdata/raw-data/ENMODO18_Personas.xlsx") |>
  mutate(MNI = case_when(IV_2_edad < 15 ~ 1,
                         IV_12_nivel_aprobado %in% c(7, 22) ~ 1,
                         T ~ IV_12_nivel_aprobado),
         MNI_lab = factor(MNI, levels = 0:6,
                          labels = c("Sin estudios", "Sin estudios", "Primario", "Secundario",
                                     "Terciario", "Universitario", "Post Universitario"))
         ) |>
  rename(EDAD = IV_2_edad, SEXO = IV_4_sexo, REL = IV_6_relacionConJefeDeHogar)

cli_ul(col_silver("ENMODO2018 VIAJE") )

VIAJE <- read_excel("_rawdata/raw-data/ENMODO18_Viajes.xlsx") |>
  rename(ORI = radio_origen, DES = radio_destino,
         ORI_ACT = VII_2_activida_en_el_origen,
         DES_ACT = VII_3_activida_en_el_destino,
         INI = hora_inicio, FIN = hora_final,
         DUR_VIA = duracion_minutos,
         FREC = VII_9_veces_por_semana_viaja) |>
  mutate(across(.cols = c(ORI, DES),
                .fns = ~str_pad(.x, width = 9, pad = "0")),
         # Categorizar las actividades de destino
         MOT = case_when(DES_ACT %in% c(2:3, 14)            ~ 1,
                         DES_ACT %in% c(4:5)                ~ 2,
                         DES_ACT %in% c(6:7, 13, 15:17, 22) ~ 3,
                         DES_ACT %in% c(8:12)               ~ 4),
         MOT = factor(MOT, levels = 1:4,
                      labels = c("Trabajo", "Estudio",
                                 "Reproducción", "Recreación") ),
         # Correcciones manuales
         DES = ifelse(DES == 20010300, 20010305, DES),
         DES = ifelse(DES == 20141002, 20141001, DES))

cli_ul(col_silver("ENMODO2018 ETAPA") )

ETAPA <- read_excel("_rawdata/raw-data/ENMODO18_Etapas.xlsx")

# Crear variables -----------------------------------
# Base HOGAR:
#   * Máximo Nivel Educativo del Hogar (EDU)
#   * Ponderación del Hogar (PONDERA_HOG)

cli_li(col_yellow("Procesando bases"))

HOGAR_aux <- PERSONA |>
  group_by(id_hogar) |>
  summarise(EDU1 = max(MNI * (REL %in% 1:2)),
            EDU2 = max(MNI),
            PONDERA_HOG = sum(PONDERA, na.rm = T),
            .groups = "drop") |>
  mutate(# EDU incluye sólo a PSH y CON
         EDU = factor(EDU1, levels = 0:6,
                      labels = c("Sin estudios", "Sin estudios", "Primario", "Secundario",
                                 "Terciario", "Universitario", "Post Universitario")),
         # EDU2 incluye todos los integrantes del hogar
         EDU2 = factor(EDU2, levels = 0:6,
                      labels = c("Sin estudios", "Sin estudios", "Primario", "Secundario",
                                 "Terciario", "Universitario", "Post Universitario"))
         )

HOGAR <- HOGAR |>
  left_join(HOGAR_aux, by = "id_hogar")

rm(HOGAR_aux)

# Guardar resultado en lista ENMODO --------------

cli_li(col_yellow("Armado de bases finales (movilidad)"))

ENMODO = list(HOG = HOGAR, PER = PERSONA,
              VIA = VIAJE, ETA = ETAPA)

rm(HOGAR, PERSONA, VIAJE, ETAPA)

# cli_end(ulid)
# rm(ulid)

cli_alert_info("Guradando {.path PhD_05_ENMODO.RData}.")
save.image("D:/Escritorio/PhD_data/PhD_05_ENMODO.RData")
file.copy(from = "D:/Escritorio/PhD_data/PhD_05_ENMODO.RData",
          to = "data/PhD_processed-data.RData",
          overwrite = T)

# Elementos auxiliares

# RADIOS / DEPTO en la muestra de hogares
# AUX$MUESTRA <- HOGAR$ID_RADIO |> unique()
# AUX$MUESTRA_DPTO <- str_sub(AUX$MUESTRA, end = 5)|> unique()

# RADIOS / DEPTO que son ORI/DES de los viajes
# AUX$VIAJE <- c(VIAJE$ORI,  VIAJE$DES)|> unique()
# AUX$VIAJE <- AUX$VIAJE[!is.na(AUX$VIAJE)]
# AUX$VIAJE_DEPTO <- str_sub(AUX$VIAJE, end = 5) |> unique()

# load("data/PhD_processed-data.RData")
