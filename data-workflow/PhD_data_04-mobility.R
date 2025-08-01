# Cargar Movilidad
# ---------------*
# Basado en script TP Goinheix

# TODO    Agregar distancia al centro (pza de mayo). Idealmente debería calcularse
#           por red de calles ponderando.
# TODO    Corregir datos para duraciones de actividades negativas.
# TODO    Armar base de actividades
# DONE    Calcular segregación local de fracción de destino
# DONE    No sumar movilidades dobles de persona entre dos lugares.
# DONE    Revisar FRAC (0200113 y otros están sin población).
# DONE    Calcular segregación local de radio de residencia.

cli_h2(col_cyan("Cargando datos de movilidad (ENMODO2018)"))

if(!exists("CARTO", inherits = F) || is.null(CARTO$ESCUELAS)){
  cli_alert_info(
    col_silver(
      paste0("Cargando cartografía",
             ": {.path data-workflow/intermediate-data/PhD_05_carto-educ.RData}.")
    )
  )

  load(fs::path("data-workflow", "intermediate-data",
                "PhD_05_carto-educ", ext = "RData"))
}

# ╠ Cargar datos ENMODO2018 -----------------

  # 1. Cargar bases

  cli_li(col_yellow("Cargando datos ENMODO2018"))

  cli_ul(col_silver("ENMODO2018 HOGAR") )

  HOGAR <- read_excel(fs::path("data-workflow", "raw-data", "ENMODO18_Hogar.xlsx") ) |>
    rename(ID_RADIO = RADIO_CENSAL_hogar)

  cli_ul(col_silver("ENMODO2018 PERSONA") )

  PERSONA <- read_excel(fs::path("data-workflow", "raw-data", "ENMODO18_Personas.xlsx") ) |>
    rename(EDAD = IV_2_edad, SEXO = IV_4_sexo, REL = IV_6_relacionConJefeDeHogar)

  cli_ul(col_silver("ENMODO2018 VIAJE") )

  VIAJE <- read_excel(fs::path("data-workflow", "raw-data", "ENMODO18_Viajes.xlsx") ) |>
    rename(ORI = radio_origen, DES = radio_destino,
           ORI_ACT = VII_2_activida_en_el_origen,
           DES_ACT = VII_3_activida_en_el_destino,
           INI = hora_inicio, FIN = hora_final,
           DIST = distancia_viaje_km,
           DUR_VIA = duracion_minutos,
           FREC = VII_9_veces_por_semana_viaja)

  cli_ul(col_silver("ENMODO2018 ETAPA") )

  ETAPA <- read_excel(fs::path("data-workflow", "raw-data", "ENMODO18_Etapas.xlsx") )

  # 2. Correcciones manuales de datos

  cli_ul(col_cyan("Corrección de datos mal cargados"))

  HOGAR <- HOGAR |>
    mutate(ID_RADIO = ifelse(ID_RADIO == "60281222", "060281202", ID_RADIO ))

  VIAJE <- VIAJE |>
    mutate(DES = ifelse(DES == 20010300, 20010305, DES),
           DES = ifelse(DES == 20141002, 20141001, DES),
           # Horarios de viajes
           INI = case_when(id_hogar == 510   & id_persona == 1 & id_viaje == 4 ~ 12+1/12,
                           id_hogar == 619   & id_persona == 1 & id_viaje == 2 ~ 9,
                           id_hogar == 689   & id_persona == 1 & id_viaje == 2 ~ 8 +1/12,
                           id_hogar == 737   & id_persona == 3 & id_viaje == 2 ~ 10.75,
                           id_hogar == 896   & id_persona == 1 & id_viaje == 2 ~ 13.5,
                           id_hogar == 937   & id_persona == 1 & id_viaje == 4 ~ 16.25,
                           id_hogar == 1014  & id_persona == 3 & id_viaje == 3 ~ 14,
                           id_hogar == 1706  & id_persona == 2 & id_viaje == 2 ~ (7 + 5/12),
                           id_hogar == 1766  & id_persona == 1 & id_viaje == 2 ~ 10.5,
                           id_hogar == 1865  & id_persona == 1 & id_viaje == 2 ~ (13 + 5/12),
                           id_hogar == 1874  & id_persona == 1 & id_viaje == 2 ~ 12,
                           id_hogar == 1920  & id_persona == 4 & id_viaje == 3 ~ 17.25,
                           id_hogar == 1957  & id_persona == 3 & id_viaje == 4 ~ 16.75,
                           id_hogar == 2031  & id_persona == 2 & id_viaje == 2 ~ 17,
                           id_hogar == 2164  & id_persona == 2 & id_viaje == 3 ~ 17 + 1/3,
                           id_hogar == 2166  & id_persona == 2 & id_viaje == 6 ~ 17 + 59/60,
                           id_hogar == 2991  & id_persona == 1 & id_viaje == 2 ~ 14 + 2/3,
                           id_hogar == 3177  & id_persona == 1 & id_viaje == 2 ~ 7 + 2/3,
                           id_hogar == 3292  & id_persona == 1 & id_viaje == 2 ~ 11,
                           id_hogar == 3294  & id_persona == 3 & id_viaje == 2 ~ 21,
                           id_hogar == 3295  & id_persona == 2 & id_viaje == 2 ~ 12 + 5/12,
                           id_hogar == 3516  & id_persona == 1 & id_viaje == 1 ~ 12,
                           id_hogar == 3516  & id_persona == 1 & id_viaje == 2 ~ 13 + 5/6,
                           id_hogar == 3617  & id_persona == 1 & id_viaje == 1 ~ 13 + 1/3,
                           id_hogar == 3617  & id_persona == 1 & id_viaje == 2 ~ 14,
                           id_hogar == 3751  & id_persona == 1 & id_viaje == 2 ~ 7 + 1/6,
                           id_hogar == 3828  & id_persona == 1 & id_viaje == 3 ~ 21,
                           id_hogar == 3828  & id_persona == 1 & id_viaje == 4 ~ 22,
                           id_hogar == 3876  & id_persona == 1 & id_viaje == 2 ~ 16,
                           id_hogar == 3879  & id_persona == 1 & id_viaje == 2 ~ 12+1/3,
                           id_hogar == 3936  & id_persona == 1 & id_viaje == 6 ~ 12+1/12,
                           id_hogar == 4115  & id_persona == 1 & id_viaje == 2 ~ 10+7/12,
                           id_hogar == 4128  & id_persona == 5 & id_viaje == 3 ~ 17+1/6,
                           id_hogar == 4145  & id_persona == 1 & id_viaje == 4 ~ 12+1/12,
                           id_hogar == 4247  & id_persona == 2 & id_viaje == 2 ~ 7.5,
                           id_hogar == 4255  & id_persona == 4 & id_viaje == 2 ~ 8+1/12,
                           id_hogar == 4815  & id_persona == 1 & id_viaje == 2 ~ 8+1/6,
                           id_hogar == 4843  & id_persona == 1 & id_viaje == 1 ~ 6.5,
                           id_hogar == 4873  & id_persona == 1 & id_viaje == 4 ~ 17+1/3,
                           id_hogar == 4893  & id_persona == 1 & id_viaje == 2 ~ 19+5/6,
                           id_hogar == 4895  & id_persona == 1 & id_viaje == 2 ~ 8,
                           id_hogar == 4897  & id_persona == 1 & id_viaje == 2 ~ 12.5,
                           id_hogar == 5131  & id_persona == 1 & id_viaje == 2 ~ 7.25,
                           id_hogar == 5245  & id_persona == 1 & id_viaje == 2 ~ 10,
                           id_hogar == 5373  & id_persona == 2 & id_viaje == 1 ~ 7,
                           id_hogar == 5373  & id_persona == 2 & id_viaje == 3 ~ 16,
                           id_hogar == 5463  & id_persona == 3 & id_viaje == 2 ~ 8+1/3,
                           id_hogar == 5469  & id_persona == 1 & id_viaje == 3 ~ 12,
                           id_hogar == 5591  & id_persona == 1 & id_viaje == 2 ~ 18+5/12,
                           id_hogar == 5612  & id_persona == 1 & id_viaje == 2 ~ 11+5/6,
                           id_hogar == 6073  & id_persona == 1 & id_viaje == 2 ~ 16,
                           id_hogar == 6172  & id_persona == 3 & id_viaje == 2 ~ 17.5,
                           id_hogar == 6308  & id_persona == 1 & id_viaje == 4 ~ 17+1/3,
                           id_hogar == 6320  & id_persona == 1 & id_viaje == 2 ~ 10.5,
                           id_hogar == 6321  & id_persona == 1 & id_viaje == 2 ~ 18+17/60,
                           id_hogar == 6349  & id_persona == 2 & id_viaje == 1 ~ 6.5,
                           id_hogar == 6424  & id_persona == 2 & id_viaje == 2 ~ 11.5,
                           id_hogar == 6471  & id_persona == 5 & id_viaje == 2 ~ 8+2/3,
                           id_hogar == 6508  & id_persona == 1 & id_viaje == 1 ~ 12+2/3,
                           id_hogar == 6596  & id_persona == 1 & id_viaje == 6 ~ 11.75,
                           id_hogar == 6687  & id_persona == 1 & id_viaje == 2 ~ 10+1/3,
                           id_hogar == 6820  & id_persona == 1 & id_viaje == 2 ~ 7.25,
                           id_hogar == 6868  & id_persona == 1 & id_viaje == 2 ~ 10+11/12,
                           id_hogar == 6907  & id_persona == 3 & id_viaje == 3 ~ 19.25,
                           id_hogar == 7000  & id_persona == 1 & id_viaje == 3 ~ 11+5/6,
                           id_hogar == 7170  & id_persona == 1 & id_viaje == 2 ~ 8+7/12,
                           id_hogar == 7170  & id_persona == 2 & id_viaje == 2 ~ 8+7/12,

                           T ~ INI),
           FIN = case_when(id_hogar == 510   & id_persona == 1 & id_viaje == 3 ~ 12,
                           id_hogar == 619   & id_persona == 1 & id_viaje == 1 ~ (6 + 2/3),
                           id_hogar == 689   & id_persona == 1 & id_viaje == 1 ~ (7 + 5/6),
                           id_hogar == 737   & id_persona == 3 & id_viaje == 1 ~ (10 + 5/12),
                           id_hogar == 937   & id_persona == 1 & id_viaje == 3 ~ 16,
                           id_hogar == 1014  & id_persona == 3 & id_viaje == 3 ~ 15,
                           id_hogar == 1681  & id_persona == 4 & id_viaje == 1 ~ (12 + 2/3),
                           id_hogar == 1706  & id_persona == 2 & id_viaje == 1 ~ 7.4,
                           id_hogar == 1766  & id_persona == 1 & id_viaje == 1 ~ (10 + 5/12),
                           id_hogar == 1865  & id_persona == 1 & id_viaje == 1 ~ (13 + 1/60),
                           id_hogar == 1874  & id_persona == 1 & id_viaje == 1 ~ 11.5,
                           id_hogar == 1920  & id_persona == 4 & id_viaje == 2 ~ (17 + 1/6),
                           id_hogar == 1957  & id_persona == 3 & id_viaje == 4 ~ 12.25,
                           id_hogar == 2031  & id_persona == 2 & id_viaje == 1 ~ (16 + 5/6),
                           id_hogar == 2166  & id_persona == 2 & id_viaje == 5 ~ (17 + 31/60),
                           id_hogar == 2991  & id_persona == 1 & id_viaje == 1 ~ (13 + 1/3),
                           id_hogar == 3177  & id_persona == 1 & id_viaje == 1 ~ (7 + 7/12),
                           id_hogar == 3292  & id_persona == 1 & id_viaje == 1 ~ (10 + 1/6),
                           id_hogar == 3294  & id_persona == 3 & id_viaje == 1 ~ 18,
                           id_hogar == 3295  & id_persona == 2 & id_viaje == 1 ~ 12 + 1/6,
                           id_hogar == 3516  & id_persona == 1 & id_viaje == 1 ~ 13,
                           id_hogar == 3516  & id_persona == 1 & id_viaje == 2 ~ 14 + 5/6,
                           id_hogar == 3617  & id_persona == 1 & id_viaje == 1 ~ 14,
                           id_hogar == 3617  & id_persona == 1 & id_viaje == 2 ~ 14.5,
                           id_hogar == 3758  & id_persona == 3 & id_viaje == 1 ~ 10.5,
                           id_hogar == 3828  & id_persona == 1 & id_viaje == 3 ~ 21+1/6,
                           id_hogar == 3828  & id_persona == 1 & id_viaje == 4 ~ 22+1/6,
                           id_hogar == 3876  & id_persona == 1 & id_viaje == 2 ~ 14.5,
                           id_hogar == 3879  & id_persona == 1 & id_viaje == 1 ~ 12+1/6,
                           id_hogar == 3936  & id_persona == 1 & id_viaje == 5 ~ 12,
                           id_hogar == 4115  & id_persona == 1 & id_viaje == 1 ~ 10+5/12,
                           id_hogar == 4128  & id_persona == 5 & id_viaje == 3 ~ 17.5,
                           id_hogar == 4145  & id_persona == 1 & id_viaje == 3 ~ 12,
                           id_hogar == 4255  & id_persona == 4 & id_viaje == 1 ~ 8,
                           id_hogar == 4563  & id_persona == 1 & id_viaje == 2 ~ 13,
                           id_hogar == 4814  & id_persona == 1 & id_viaje == 1 ~ 12+5/6,
                           id_hogar == 4815  & id_persona == 1 & id_viaje == 1 ~ 8,
                           id_hogar == 4843  & id_persona == 1 & id_viaje == 1 ~ 7,
                           id_hogar == 4893  & id_persona == 1 & id_viaje == 1 ~ 17.25,
                           id_hogar == 4895  & id_persona == 1 & id_viaje == 1 ~ 7,
                           id_hogar == 4897  & id_persona == 1 & id_viaje == 1 ~ 11+ 11/12,
                           id_hogar == 5131  & id_persona == 1 & id_viaje == 1 ~ 7+1/6,
                           id_hogar == 5245  & id_persona == 1 & id_viaje == 1 ~ 8.25,
                           id_hogar == 5373  & id_persona == 2 & id_viaje == 3 ~ 16.25,
                           id_hogar == 5463  & id_persona == 3 & id_viaje == 1 ~ 8+1/6,
                           id_hogar == 5469  & id_persona == 1 & id_viaje == 3 ~ 12.25,
                           id_hogar == 5520  & id_persona == 1 & id_viaje == 1 ~ 13,
                           id_hogar == 5591  & id_persona == 1 & id_viaje == 1 ~ 16+1/12,
                           id_hogar == 5612  & id_persona == 1 & id_viaje == 1 ~ 11+1/6,
                           id_hogar == 6073  & id_persona == 1 & id_viaje == 1 ~ 10.25,
                           id_hogar == 6172  & id_persona == 3 & id_viaje == 2 ~ 19,
                           id_hogar == 6308  & id_persona == 1 & id_viaje == 3 ~ 17+1/6,
                           id_hogar == 6320  & id_persona == 1 & id_viaje == 1 ~ 10,
                           id_hogar == 6321  & id_persona == 1 & id_viaje == 1 ~ 17.25,
                           id_hogar == 6349  & id_persona == 2 & id_viaje == 1 ~ 6.75,
                           id_hogar == 6424  & id_persona == 2 & id_viaje == 1 ~ 10+1/12,
                           id_hogar == 6471  & id_persona == 5 & id_viaje == 1 ~ 8+1/6,
                           id_hogar == 6508  & id_persona == 1 & id_viaje == 3 ~ 14+11/12,
                           id_hogar == 6820  & id_persona == 1 & id_viaje == 2 ~ 7+5/12,
                           id_hogar == 6868  & id_persona == 1 & id_viaje == 1 ~ 10+5/6,
                           id_hogar == 6907  & id_persona == 3 & id_viaje == 2 ~ 18.5,
                           id_hogar == 6919  & id_persona == 2 & id_viaje == 1 ~ 12+5/6,
                           id_hogar == 6975  & id_persona == 1 & id_viaje == 1 ~ 10+1/3,
                           id_hogar == 7000  & id_persona == 1 & id_viaje == 3 ~ 12+1/6,
                           id_hogar == 7170  & id_persona == 1 & id_viaje == 1 ~ 8.5,
                           id_hogar == 7170  & id_persona == 2 & id_viaje == 1 ~ 8.5,

                           T ~ FIN),
    )

  # 3. Agregar nuevas variables
  cli_ul(col_cyan("Modificación y creación de variables en bases de movilidad"))

  aux_RAD <- CARTO$RADIO |>
    st_drop_geometry() |>
    filter(!is.na(TH)) |>
    select(ID_RADIO, TH, ls)

  # 3.a Base PERSONA
  #   * Ponderación del Hogar (PONDERA)
  #   * Nivel máximo de instrucción (MNI)
  #   * Nivel máximo de instrucción (MNI_lab)

  PERSONA <- PERSONA |>
    mutate(MNI = case_when(EDAD < 15 ~ 1,
                           IV_12_nivel_aprobado %in% c(7, 22) ~ 1,
                           T ~ IV_12_nivel_aprobado),
           MNI_lab = factor(MNI, levels = 0:6,
                            labels = c("Sin estudios", "Sin estudios", "Primario", "Secundario",
                                       "Terciario", "Universitario", "Post Universitario"))
    )

  # 3.b Base HOGAR:
  #   * Máximo Nivel Educativo del Hogar (EDU)
  #   * Ponderación del Hogar (PONDERA_HOG)
  #   * Máximo Nivel Educativo del Hogar (EDU2)

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
    left_join(HOGAR_aux, by = "id_hogar") |>
    left_join(aux_RAD, by = "ID_RADIO") |>
    rename(TH_FROM = TH, ls_FROM = ls)

  rm(HOGAR_aux)

  # 3.c Base VIAJE:
  #   * Categorizar actividades de destino (MOT)
  #   * Formatear ORI y DES a 9 dígitos (ID_RADIO)

  VIAJE <- VIAJE |>
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
           DIST = DIST |> str_remove_all("-") |> as.numeric() ) |>
    left_join(aux_RAD, by = c("ORI" = "ID_RADIO")) |> rename(TH_ORI = TH, ls_ORI = ls) |>
    left_join(aux_RAD, by = c("DES" = "ID_RADIO")) |> rename(TH_DES = TH, ls_DES = ls) |>

# ╠ Crear cartografía a nivel de fracción -----------------

  cli_li(col_yellow("Creando cartografía a nivel de fracción (CARTO$FRAC)"))

  CARTO$FRAC <- CARTO$RADIO |>
    group_by(ID_FRAC = str_sub(ID_RADIO, end = 7)) |>
    summarise(across(HOG_PRI:HOG_SUP, ~sum(.x, na.rm = TRUE)),
              ls_res = sum(ls * p, na.rm = TRUE))

# ╠ Crear base de datos de movilidades individuales ---------------
# Datos de viaje a nivel PERSONA: Si la persona no tiene movilidades se asigna
#   un registro con id_viaje == 0, ORI = DES = FROM (= ID_RADIO del hogar),
#
# Variables de localización:
#  * FROM    Lugar de residencia (ID_RADIO reducido a escala)
#  * ORI     Lugar de inicio del viaje
#  * DES     Lugar de destino del viaje
  cli_li(col_yellow("Creando base de movilidades individuales"))

  # Definir escala geográfica (Departamento: 5, Fracción: 7, Radio: 9)
  escala <- c("Fracción" = 7)

  # Datos de viajes (VIAJE)

  db_mov <-  VIAJE |>
    arrange(id_hogar, id_persona, id_viaje) |>
    select(-PONDERA) # Sacamos el ponderador individual (se usa ponderador del hogar)

  # Datos de personas (PERSONA)
  # PONDERA, EDAD, SEXO, MNI

  db_mov <- PERSONA |>
    select(id_hogar, id_persona, PONDERA, EDAD, SEXO, MNI = MNI_lab) |>
    full_join(db_mov, by = c("id_hogar", "id_persona"))

  # Sumar datos de hogares (HOGAR)
  # RADIO (+FROM), EDU, PONDERA_HOG, TH_FROM

  db_mov <- HOGAR |>
    mutate(FROM = ID_RADIO) |>
    select(id_hogar, ID_RADIO, ID_RADIO, EDU, PONDERA_HOG, FROM, TH_FROM, ls_FROM) |>
    full_join(db_mov, by = "id_hogar") |>
    # Filtro hogares residentes en la zona de estudio
    filter(ID_RADIO %in% aux_RAD$ID_RADIO)

  # Modificar y filtrar base de viajes
  # ---------------------------------*
  # Base original: 67.929 viajes
  # Base filtrada: 67.838 viajes
  #    (a) Se remueven 18 viajes con datos duplicados
  #    (b) Se pierden 73 viajes (29 personas) por hora de viaje incompleta en
  #         algún viaje
  #
  # Se reordenaron los registros por hora (potencialmente se pueden haber
  #     perdido cronología real)

  # Quitar duplicados de inicio y motivo                                        # (a)
  db_mov <- db_mov[is.na(db_mov$INI) |
                     !base::duplicated(db_mov[c("id_hogar", "id_persona", "INI", "MOT")]), ]

  # Generar/modificar variables y cambiar escala
  EDU_lab <- c("Sin estudios", "Sin estudios",
               "Primario", "Secundario",
               "SNU", "Universitario",
               "Post-Universitario")

  db_mov <- db_mov |>
    mutate(ID = paste0(str_pad(id_hogar, width = 5, pad = "0"),
                       str_pad(id_persona, width = 2, pad = "0")),
           # Transformar ORI/DES/FROM a escala elegida (fracción)
           across(.cols = c(ORI, DES, FROM),
                  .fns = ~str_sub(.x, end = escala)  ),
           # Asignar id_viaje == 0 para casos sin viajes
           id_viaje = ifelse(is.na(id_viaje), 0, id_viaje),
           # Asignar ORI = DES = FROM para casos sin viajes
           across(.cols = c(DES, ORI),
                  .fns = ~ifelse(id_viaje == 0, FROM, .x) ),
           # Recategorizar MNI y EDU para coincidir con esquema de la tesis
           across(.cols = c(MNI, EDU),
                  .fns = ~factor(.x, levels = EDU_lab,
                                 labels = c(rep("Primario", 3),
                                            "Secundario",
                                            rep("Superior", 3) ) ),
                  .names = "{.col}_red"))

  # Construir etapas de actividades y duraciones
  db_mov <- db_mov |>
    arrange(ID, INI) |>
    group_by(ID) |>
    # Filtrar personas sin datos de inicio / fin en alguno de sus viajes
    filter(id_viaje == 0 | sum(is.na(INI) | is.na(FIN)) == 0) |>                # (b)
    # Generar nuevo id de viajes y marca de última actividad
    mutate(id_viaje2 = 1:n(),
           ULT = n() == id_viaje2) |>
    ungroup() |>
    # Calcular variables de actividad y viajes
    mutate(ID_SIG = lead(ID),
           INI_SIG = lead(INI),
           FIN = ifelse(INI > FIN, 24, FIN),                                        # Movilidades que terminan al día siguiente se truncan a las 24hs
           INI_SIG = ifelse(ULT, 24, INI_SIG),                                      # Última actividad se asigna que la siguiente empieza a las 24
           INI_SIG = ifelse(INI_SIG == FIN, INI_SIG + 1/24, INI_SIG),               # Actividades sin duración se les asigna 1 minuto # SACADO: (salvo que sean posterior a las 24hs)
           DUR_ACT = ifelse(ID == ID_SIG, (INI_SIG - FIN) * 60, (24 - FIN) * 60),
           DUR_VIA_calc = round((FIN-INI) * 60),
           PONDERA_time = round(PONDERA * DUR_ACT, 1))

  rm(EDU_lab, escala, aux_RAD)

# ╠ Grafo bipartito ---------------------------------

  cli_li(col_yellow("Creando grafo bipartito (PERSONA <-> LUGAR)"))

  pza_de_mayo <- st_sfc(st_point(x = c(-58.3769181, -34.6085972)), crs = 4326) |>
    st_transform(st_crs(CARTO$FRAC))

  # Base de viajes bipartita (PERSONA <-> LUGAR) -------------------------------------------
  # Se seleccionan:
  #   (a) viajes hacia actividades que no terminan en el hogar (DES_ACT != 1);  -> no tiene sentido ver estos destinos porque no son "lugares de uso"
  #   (b) personas con viajes el día de referencia (id_viaje != 0);             -> no tiene sentido considerar a personas sin movilidad
  #   (c) viajes dentro de RMBA (viaja_rmba != 2); y                            -> no tiene sentido considerar viajes fuera de AMBA (fuera de objeto)
  #   (d) sin datos faltantes en origen-destino (!is.na(DES) & !is.na(ORI))
  #   (e) personas de 16 años o más (EDAD >= 16)                                -> se supone que movilidad es de personas menores de 16 años no son autónomas (se considera en capítulo educativo)
  #   (f) los destinos son lugares clasificados según TH (!is.na(TH_DES))       -> se considera viajes a entornos urbanos dentro del área de estudio
  #   (g) viajes con ponderación de tiempo positiva (!is.na(PONDERA_time) &
  #         & PONDERA_time > 0)                                                 -> se considera viajes con tiempo de actividad
  #
  # Base original:  67.838 viajes
  # Base bipartita: 20.888 viajes

  cli_ul(col_cyan("Filtrar viajes") )

  db_bipartita <- db_mov |>
    filter(DES_ACT != 1 &                             # (a)
             id_viaje != 0 &                          # (b)
             viaja_rmba != 2 &                        # (c)
             (!is.na(DES) & !is.na(ORI)) &            # (d)
             EDAD >= 16 &                             # (e)
             !is.na(TH_DES) &                         # (f)
             !is.na(PONDERA_time) & PONDERA_time > 0  # (g)
           ) |>
    # Seleccionar variables
    select(ID, id_hogar, id_persona, id_viaje,    # ID
           PONDERA, PONDERA_HOG, PONDERA_time,    # w
           ID_RADIO, FROM, ORI, DES,              # Geo: ID_RADIO, FROM, ORI, DES,
           starts_with("TH_"),                    # Geo TH: TH_FROM, TH_ORI, TH_DES,
           starts_with("ls_"),                    # Geo ls: ls_FROM, ls_ORI, ls_DES,
           MOT, modo_des,                         # Motivos y modalidad
           INI, FIN, DUR_VIA, DUR_VIA_calc, FREC, # Temporalidad del viaje
           DUR_ACT,                               # Temporalidad de la actividad
           DIST,                                  # Distancia viajada
           EDAD, SEXO,                            # Características personales
           MNI, EDU, MNI_red, EDU_red)            # Características educativas

  # Propiedades de destinos (in) ---------------------------------------

  cli_ul(col_cyan("Calcular propiedades de destino (in)") )

  # Variables asociadas a las geografías de destino:
  # * Segregación residencial local
  # * Centralidad: Distancia a la centralidad (Plaza de Mayo, CABA)
  # * Policentralidad: Distancia a la estación de FFCC más cercana
  # * Zonificación: Ubicación en el aglomerado (Sur, Centro, Norte, Este, Oeste)

  aux_frac <- CARTO$FRAC |>
    rename(DES = ID_FRAC) |>
    # Filtrar nodos (áreas) no presentes en la red
    filter(DES %in% unique(db_bipartita$DES)) |>
    # Coordenadas para layout geográfico
    st_centroid() |>
    mutate(# Segregación
           lsRES_lab = cut(ls_res, include.lowest = T, ordered_result = T,
                           breaks = quantile(ls_res, na.rm = T, probs = seq(0, 1, by = 0.2)),
                           labels = paste0("inSEGRES_Q", 1:5) ),
           # Centalidad/policentralidad
           CENTRALIDAD = st_distance(x = geometry, y = pza_de_mayo)[,1],  # Distancia a Plaza de Mayo
           CENTRALIDAD = round(units::drop_units(CENTRALIDAD)/1000, 1),             # Unidad: km
           POLICENTRAL = apply(st_distance(x = geometry, y = CARTO$FFCC2), 1, min), # Distancia a estación FFCC más próxima
           POLICENTRAL = round(POLICENTRAL/1000, 1),                                # Unidad: km
           across(.cols = CENTRALIDAD:POLICENTRAL,
                  .fns = ~cut(.x, include.lowest = T, ordered_result = T,
                              breaks = quantile(.x, na.rm = T, probs = seq(0, 1, by = 0.2)),
                              labels = paste0("Q", 1:5) ) ),
           CENTRALIDAD = fct_relabel(CENTRALIDAD, ~paste0("inCentr_", .) ),
           POLICENTRAL = fct_relabel(POLICENTRAL, ~paste0("inPolicentr_", .) ),
           # Zona de ubicación en el aglomerado
           x = st_coordinates(geometry)[, 1],
           y = st_coordinates(geometry)[, 2],
           across(.cols = c(x:y),
                  .fns = ~cut(.x, include.lowest = T, breaks = 3),
                  .names = "{col}_"),
           x_ = factor(x_, levels(x_), labels = paste0("inLON_", c("Oeste", "Centro", "Este"))),
           y_ = factor(y_, levels(y_), labels = paste0("inLAT_", c("Sur", "Centro", "Norte")))) |>
    st_drop_geometry() |>
    rename(lon = x_, lat = y_)

  # Variables de tiempo:
  # * Tiempo de actividad: Tiempo promedio de actividad por motivo (inTRA,
  #         inREPR, inRECR, inEST), agrupado por quintiles.

  aux_time <- db_bipartita |>
    filter(!is.na(PONDERA_time)) |>
    group_by(DES, MOT) |>
    summarise(time_mot = sum(PONDERA_time)) |>
    group_by(MOT) |>
    mutate(MOT = str_to_upper(MOT),
           MOT = str_sub(MOT, 1, 3),
           MOT = paste0("time", MOT),
           time_mot = round(time_mot / 60 / 1000, 1) ) |> # Cambio de Unidad: miles de hs.
    ungroup() |>
    pivot_wider(id_cols = DES, names_from = MOT,
                values_from = time_mot) |>
    mutate(across(.cols = -DES,
                  .fns = ~cut(.x, include.lowest = T, ordered_result = T,
                              breaks = quantile(.x, na.rm = T, probs = seq(0, 1, by = 0.2)),
                              labels = paste0("Q", 1:5) ) ),
           across(.cols = -DES,
                  .fns = ~fct_na_value_to_level(.x, level = "NO") ),
           timeTRA = fct_relabel(timeTRA, ~paste0("inTrab_", .) ),
           timeREP = fct_relabel(timeTRA, ~paste0("inRepr_", .) ),
           timeREC = fct_relabel(timeTRA, ~paste0("inRecr_", .) ),
           timeEST = fct_relabel(timeTRA, ~paste0("inEst_", .) ))

  # Variables de tiempo en destino:
  # * Tiempo total: Cantidad de personas/tiempo en destino (n: muestra, N: población, time: tiempo)
  # * Tiempo total x grupo: ponderado en destino por grupo educativo (inPRI, inSEC, inSUP).

  aux_db <- db_bipartita |>
    group_by(DES) |>
    summarise(n = n(),
              N = sum(PONDERA, na.rm = T),
              time = sum(PONDERA_time, na.rm = T),
              PRI_t = sum(PONDERA_time * (EDU_red == "Primario"), na.rm = T),
              SEC_t = sum(PONDERA_time * (EDU_red == "Secundario"), na.rm = T),
              SUP_t = sum(PONDERA_time * (EDU_red == "Superior"), na.rm = T))

  # Variables de segregación de movilidad en destino
  aux_seg_in <- aux_db |>
    select(-time, -N, -n) |>
    pivot_longer(cols = PRI_t:SUP_t) |>
    mutual_local(group = "name", unit = "DES", weight = "value",
                 se = T, wide = T) |>
    mutate(ls = ifelse(ls < 0, 0, ls),
           ls = round(ls, 3),
           ls_lab = cut(ls, include.lowest = T,
                        labels = paste0("inSEG_Q", 1:5),
                        breaks = quantile(ls, na.rm = T,
                                          probs = seq(0, 1, by = 0.2)) ) ) |>
    select(DES, ls, ls_lab)

  prop_in <- aux_db |>
    filter(N > 0) |>
    mutate(across(.cols = ends_with("_t"),
                  .fns = ~ round(.x/ time * 100) ),                              # Porcentaje de minutos gastados en la unidad por grupo educativo
           # in_N
           N = round(N/1000, 1),                                                 # unidad: miles de personas
           N_lab = cut(N, include.lowest = T,
                       breaks = quantile(N, na.rm = T,
                                         probs = seq(0, 1, by = 0.2))),
           N_lab = factor(N_lab, levels(N_lab), labels = paste0("inN_", levels(N_lab))),
           # in_time
           time = round(time/60/1000, 2),                                        # unidad: miles de hs
           time_lab = cut(time, include.lowest = T,
                          breaks = quantile(time, na.rm = T,
                                            probs = seq(0, 1, by = 0.2)),
                          labels = paste0("inT_Q", 1:5)),
           # % EDU
           across(.cols = ends_with("_t"),
                  .fns = ~cut(.x, include.lowest = T,
                              breaks = unique(quantile(.x, na.rm = T, probs = seq(0, 1, by = 0.2)))),
                  .names = "{.col}_lab"),
           PRI_t_lab = factor(PRI_t_lab, levels(PRI_t_lab), labels = paste0("inPRI_%", levels(PRI_t_lab))),
           SEC_t_lab = factor(SEC_t_lab, levels(SEC_t_lab), labels = paste0("inSEC_%", levels(SEC_t_lab))),
           SUP_t_lab = factor(SUP_t_lab, levels(SUP_t_lab), labels = paste0("inSUP_%", levels(SUP_t_lab)))
    ) |>
    left_join(aux_time, by = "DES") |>
    left_join(aux_frac, by = "DES") |>
    left_join(aux_seg_in, by = "DES") |>
    rename_with(~paste0("in_", .x), .cols = c(-DES, -n))

  rm(aux_frac, aux_time, aux_db, aux_seg_in)

  # Propiedades de personas (p) ---------------------------------------

  cli_ul(col_cyan("Calcular propiedades de personas (p)") )

  # Variables asociadas a las geografías de destino:
  # * Segregación residencial local (RADIOS)
  # * Educación: Porcentaje de personas con educación primaria, secundaria y superior
  # * Centralidad: Distancia a la centralidad (Plaza de Mayo, CABA)
  # * Policentralidad: Distancia a la estación de FFCC más cercana
  # * Zonificación: Ubicación en el aglomerado (Sur, Centro, Norte, Este, Oeste)

  aux_radio <- CARTO$RADIO |>
    select(ID_RADIO, ls_res = ls, HOG, HOG_PRI:HOG_SUP) |>
    st_centroid() |>
    mutate(# Segregación (ls M)
           lsRES_lab = cut(ls_res, include.lowest = T, ordered_result = T,
                           breaks = quantile(ls_res, na.rm = T, probs = seq(0, 1, by = 0.2)),
                           labels = paste0("rSEGRES_Q", 1:5) ),
           ls_res = round(ls_res, 3),
           # % EDU
           across(HOG_PRI:HOG_SUP,
                  ~round(.x/HOG * 100)),
           # HOG = NULL,
           across(.cols = HOG_PRI:HOG_SUP,
                  .fns = ~cut(.x, include.lowest = T,
                              breaks = quantile(.x, na.rm = T, probs = seq(0, 1, by = 0.2))),
                  .names = "{.col}_lab"  ),
           HOG_PRI_lab = fct_relabel(HOG_PRI_lab, ~paste0("rPRI%", .) ),
           HOG_SEC_lab = fct_relabel(HOG_SEC_lab, ~paste0("rSEC%", .) ),
           HOG_SUP_lab = fct_relabel(HOG_SUP_lab, ~paste0("rSUP%", .) ),
           # Centalidad/policentralidad
           CENTRALIDAD = st_distance(x = geometry, y = pza_de_mayo)[,1],  # Distancia a Plaza de Mayo
           CENTRALIDAD = round(units::drop_units(CENTRALIDAD)/1000, 1),             # Unidad: km
           POLICENTRAL = apply(st_distance(x = geometry, y = CARTO$FFCC2), 1, min), # Distancia a estación FFCC más próxima
           POLICENTRAL = round(POLICENTRAL/1000, 1),                                # Unidad: km
           across(.cols = CENTRALIDAD:POLICENTRAL,
                  .fns = ~cut(.x, include.lowest = T, ordered_result = T,
                              breaks = quantile(.x, na.rm = T, probs = seq(0, 1, by = 0.2)),
                              labels = paste0("Q", 1:5) ) ),
           CENTRALIDAD = fct_relabel(CENTRALIDAD, ~paste0("rCentr_", .) ),
           POLICENTRAL = fct_relabel(POLICENTRAL, ~paste0("rPolicentr_", .) ),
           # Zonificación: Ubicación en el aglomerado
           x = st_coordinates(geometry)[, 1],
           y = st_coordinates(geometry)[, 2],
           across(.cols = c(x, y),
                  .fns = ~cut(.x, include.lowest = T, breaks = 3),
                  .names = "{col}_"),
           x_ = factor(x_, levels(x_), labels = paste0("rLON_", c("Oeste", "Centro", "Este"))),
           y_ = factor(y_, levels(y_), labels = paste0("rLAT_", c("Sur", "Centro", "Norte")))) |>
    st_drop_geometry() |>
    rename_with(~paste0("r", str_remove(.x, "^HOG_")),
                .cols = starts_with("HOG_")) |>
    rename(lon = x_, lat = y_)

  # Variables personales:
  # * Edad: Categoría de edad (pEDAD)
  # * Sexo: Sexo de la persona
  # * Educación del hogar

  prop_p <-  db_bipartita |>
    group_by(ID) |>
    filter(row_number() == 1) |>
    ungroup() |>
    select(ID, TH = TH_FROM, EDAD, SEXO, EDU = EDU_red, ID_RADIO) |>
    left_join(aux_radio, by = "ID_RADIO", relationship = "many-to-one") |>
    select(-ID_RADIO) |>
    mutate(# EDAD
           EDAD = cut(EDAD, breaks = c(0, 16, 30, 45, 60, 99), include.lowest = T),
           EDAD = fct_relabel(EDAD, ~paste0("pEDAD_" ,.x)),
           # SEXO
           SEXO = factor(SEXO, levels = 1:2, labels = c("pSEX_V", "pSEX_M")),
           # EDU
          EDU = fct_relabel(EDU,  ~paste0("pEDU_", .x) ) ) |>
    rename_with(~paste0("p_", .x), .cols = -ID)

  rm(aux_radio)

  # Propiedades de viajes (v)  --------------------------------------------------------------
  # w_time:    Fortaleza de la relación (cantidad de minutos que vinculan a la persona con la región)
  # v_TACT:    Tiempo promedio gastado en el destino por la persona
  # v_TVIA:    Tiempo promedio de viaje al destino

  cli_ul(col_cyan("Calcular propiedades de viajes (v)") )

  prop_v <- db_bipartita |>
    mutate(DUR_VIA = ifelse(is.na(DUR_VIA), abs(FIN-INI)*60, DUR_VIA)) |>
    select(ID, DES, v_MOT = MOT,
           w = PONDERA, w_time = PONDERA_time,
           in_TH = TH_DES,
           v_TACT = DUR_ACT, v_TVIA = DUR_VIA, v_DIST = DIST) |>
    mutate(v_TACT = round(v_TACT/60, 1),                                        # unidad: horas
           v_TACT_lab = cut(v_TACT, include.lowest = T, dig.lab = 10,
                            breaks = quantile(v_TACT, na.rm = T,
                                              probs = seq(0, 1, by = 0.2) ) ),
           v_TACT_lab = fct_relabel(v_TACT_lab, ~paste0("eTact_" ,.x) ),
           v_TVIA_lab = cut(v_TVIA, include.lowest = T,
                            breaks = quantile(v_TVIA, na.rm = T,
                                              probs = seq(0, 1, by = 0.2) ) ),
           v_TVIA_lab = fct_relabel(v_TVIA_lab, ~paste0("eTvia_", .x) ) )

  # Matriz de indicadores múltiples --------------------------------------
  # Multiple indicator matrix (Zhu2016)

  cli_ul(col_cyan("Creando matriz de indicadores múltiples") )

  db_mim <- prop_v |>
    left_join(prop_p, by = "ID" ) |>
    left_join(prop_in, by = "DES" ) |>
    select(ID, DES, n, w, w_time,
           starts_with("p_"), starts_with("in_"), starts_with("v_")) |>
    filter(!is.na(p_TH))

  rm(pza_de_mayo)

# ╚ Base de actividades ---------------------------------
  # TODO --
# Guardar resultado en lista ENMODO --------------

  cli_li(col_yellow("Armado de bases finales (movilidad)"))

  ENMODO <- list(HOG = HOGAR, PER = PERSONA,
                VIA = VIAJE, ETA = ETAPA,
                MOV_IND = db_mov, MIM = db_mim,
                prop = list(prop_in, prop_p, prop_v))

  rm(prop_in, prop_p, prop_v, db_bipartita)
  rm(HOGAR, PERSONA, VIAJE, ETAPA)
  rm(db_mov, db_mim)

  cli_alert_info(col_silver("Guradando datos finales {.path ENMODO.rda}."))
  save(ENMODO, file = fs::path("data-workflow", "final-data", "ENMODO", ext = "rda"))

  cli_alert_info(col_silver("Guradando datos finales {.path CARTO.rda}."))
  save(CARTO, file = fs::path("data-workflow", "final-data", "CARTO", ext = "rda"))

  rm(ENMODO, CARTO)
