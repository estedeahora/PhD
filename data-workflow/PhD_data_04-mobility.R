# Cargar Movilidad
# ---------------*
# Basado en script TP Goinheix

cli_h2(col_blue("Cargando datos de movilidad (ENMODO2018)"))

# Cargar datos ENMODO2018 -------------------------------------------------

  cli_li(col_yellow("Cargando datos ENMODO2018"))

  cli_ul(col_silver("ENMODO2018 HOGAR") )

  HOGAR <- read_excel(here::here("data-workflow", "raw-data", "ENMODO18_Hogar.xlsx") ) |>
    rename(ID_RADIO = RADIO_CENSAL_hogar) |>
    # Correcciones manuales
    mutate(ID_RADIO = ifelse(ID_RADIO == "60281222", "060281202", ID_RADIO ))

  cli_ul(col_silver("ENMODO2018 PERSONA") )

  PERSONA <- read_excel(here::here("data-workflow", "raw-data", "ENMODO18_Personas.xlsx") ) |>
    mutate(MNI = case_when(IV_2_edad < 15 ~ 1,
                           IV_12_nivel_aprobado %in% c(7, 22) ~ 1,
                           T ~ IV_12_nivel_aprobado),
           MNI_lab = factor(MNI, levels = 0:6,
                            labels = c("Sin estudios", "Sin estudios", "Primario", "Secundario",
                                       "Terciario", "Universitario", "Post Universitario"))
           ) |>
    rename(EDAD = IV_2_edad, SEXO = IV_4_sexo, REL = IV_6_relacionConJefeDeHogar)

  cli_ul(col_silver("ENMODO2018 VIAJE") )

  VIAJE <- read_excel(here::here("data-workflow", "raw-data", "ENMODO18_Viajes.xlsx") ) |>
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

  ETAPA <- read_excel(here::here("data-workflow", "raw-data", "ENMODO18_Etapas.xlsx") )

# Crear variables -----------------------------------
  # Base HOGAR:
  #   * Máximo Nivel Educativo del Hogar (EDU)
  #   * Ponderación del Hogar (PONDERA_HOG)
  #   * Máximo Nivel Educativo del Hogar (EDU2)
  cli_li(col_yellow("Procesando bases de movilidad"))

  HOGAR_aux <- PERSONA |>
    group_by(id_hogar) |>
    summarise(EDU1 = max(MNI * (REL %in% 1:2)),
              EDU2 = max(MNI),
              # Corregir casos sin PSH o CON
              REL_OK = sum(REL %in% 1:2) > 0,
              EDU1 = ifelse(REL_OK, EDU1, EDU2),
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
           ) |>
    select(-REL_OK)

  HOGAR <- HOGAR |>
    left_join(HOGAR_aux, by = "id_hogar")

  rm(HOGAR_aux)

# Guardar resultado en lista ENMODO --------------

  cli_li(col_yellow("Armado de bases finales (movilidad)"))

  ENMODO = list(HOG = HOGAR, PER = PERSONA,
                VIA = VIAJE, ETA = ETAPA)

  rm(HOGAR, PERSONA, VIAJE, ETAPA)

  cli_alert_info(col_silver("Guradando datos finales {.path ENMODO.rda}."))
  # usethis::use_data(ENMODO, overwrite = TRUE, internal = FALSE)
  save(ENMODO, file = here::here("data-workflow", "final-data", "ENMODO.rda"))

  rm(ENMODO)
