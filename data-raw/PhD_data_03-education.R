# Cargar educación
# ---------------*

cli_h2(col_blue("Cargando datos educativos"))

if(!exists("CARTO", inherits = F) || is.null(CARTO$RADIO[["ls"]])){
  cli_alert_info(
    col_silver(
      paste0("Cargando cartografía",
             ": {.path data-raw/intermediate-data/PhD_03_carto-censo.RData}.")
    )
  )

  load(here::here("data-raw", "intermediate-data",
                  "PhD_03_carto-censo.RData"))
}

### ╠ Mapa Educativo Nacional -----------------------------
  # http://mapa.educacion.gob.ar/geoservicios
  # Listado de modalidades:
  # a <- CARTO$ESCUELAS
  # a$nen[!is.na(a$nen)] |> unique() |> paste(collapse = "") |> str_split("; ", simplify = T) |> sort() |> unique()
  # rm(a)

  cli_li(col_yellow("Descargando Mapa Educativo Nacional (ME)"))

  # options(timeout = 300)

  fileName <- tempfile()
  wfs <- paste0(base = "http://mapa.educacion.gob.ar/",
                service = "geoserver/ows",
                query = paste0("?service=wfs",
                               "&version=1.1.0",
                               "&request=GetCapabilities"))

  download.file(wfs, fileName, quiet = T)
  request <- GMLFile$new(fileName)
  client <- WFSCachingClient$new(request)

  CARTO$ESCUELAS <- client$getLayer("establecimiento_educativo", quiet = TRUE) |>
    mutate(CUE = str_sub(cue, start = 1L, end = -3L),
           ANEXO = str_sub(cue, start = -2L, end = -1L),
           PROV = str_sub(cue, start = 1L, end = 2L)) |>
    select(CUEANEXO = cue, CUE, ANEXO, PROV, name = fna,
           SECTOR = ges, mde, nen, geometry = the_geom) |>
    st_transform(st_crs(CARTO$AMGBA)) |>
    st_intersection(CARTO$AMGBA) |>
    filter(str_detect(mde, "Común")) |>
    mutate(ID1 = PhD_CUE_to_ID(CUEANEXO),
           NIV_PRI = str_detect(nen, "Común-Primaria 6años|Común-Primaria 7años"),
           NIV_SEC = str_detect(nen, "Común-Secundaria-Ambos Ciclos|Común-Secundaria-Ciclo Básico"),
           NIV_TEC = str_detect(nen, "Secundaria TécProf\\(INET\\)") ) |>
    filter(NIV_PRI | NIV_SEC | NIV_TEC) |>
    select(ID1, everything())

  unlink(fileName)
  rm(request, client, fileName, wfs)

### ╠ APRENDER --------------------------------------------
# Se usar promedio de años 2017/2018 para disminuir sesgos del operativo.

  cli_li(col_yellow("Cargando datos APRENDER (ME)"))

  #Secundario
  # APRENDER <- paste0("D:/Documentos/Fuentes de datos/ME Aprender/AP", c(2017,2019),
  #                    "/APRENDER", c(2017,2019),
  #                    "_BU_Estudiantes-secundaria-5-6-año_censo-L+M.csv") |>
  #   map(\(.x) read.csv2(.x, colClasses = c("ID1" = "character")) ) |>
  #   set_names(c("AP2017", "AP2019")) |>
  #   # Seleccionar escuelas en base (por ID1) y variables de trabajo
  #   map(\(x) filter(x, ID1 %in% CARTO$ESCUELAS$ID1)) |>
  #   map2(list(c("ap10", "ap11"),    # Variables EDU_MAD/EDU_PAD 2017
  #             c("ap16", "ap17")),    # Variables EDU_MAD/EDU_PAD 2019
  #        \(x, y) select(x, ID1:idalumno, ponder,
  #                       EDU_MAD = all_of(y[[1]]), EDU_PAD = all_of(y[[2]]))) |>

  ESC_APR <- APRENDER |>
    # Calcular totales de alumno por escuela
    # N = total ponderado; n = encuetados; n_sinNA = con respuestas en EDU
    group_by(ID1, APRENDER) |>
    mutate(N_op = round(sum(ponder, na.rm = T),1),
           n = n(),
           n_sinNA = sum(!is.na(EDU)),
           .groups = "drop") |>
    # Porcentaje educativo por escuela y operativo
    filter(!is.na(EDU)) |>
    count(ID1, APRENDER, N_op, n, n_sinNA, EDU, name = "nn", .drop = F) |>
    mutate(p_op = nn / n_sinNA) |>
    # Promedio de porcentaje educativo por escuela
    group_by(ID1, EDU, .drop = F ) |>
    summarise(p = weighted.mean(x = p_op, w = N_op, na.rm = T),
              N_AP = mean(N_op, na.rm = T),
              n_sinNA = sum(n_sinNA, na.rm = T)) |>
    pivot_wider(id_cols = c(ID1, N_AP, n_sinNA),
                names_from = EDU, values_from = p)

### ╚ Relevamiento Anual (RA) -------------------------------------------------

  cli_li(col_yellow("Cargando datos Relevamiento Anual (ME)"))

  # Secundario
  # ESC_RA <- paste0("D:/Documentos/Fuentes de datos/ME RA/v2/RA", c(2017,2019),
  #                  "/RA", c(2017,2019), "_BU2_Matricula.csv") |>
  #   map(\(.x) read.csv2(.x, colClasses = c("ID1" = "character"))) |>
  #   set_names(c("RA2017", "RA2018")) |>
  #   # Seleccionar escuelas en base (por ID1) y variables de trabajo
  #   map(\(x) filter(x, ID1 %in% CARTO$ESCUELAS$ID1)) |>
  #   map(\(x) select(x, ID1, all_of(paste0("X_", 8:12)), multi_sec)) |>

  # Primario
  ESC_RA <- paste0("D:/Documentos/Fuentes de datos/ME RA/v2/RA", 2017:2018,
                   "/RA", 2017:2018, "_BU2_Matricula.csv") |>
    map(\(.x) read.csv2(.x, colClasses = c("ID1" = "character"))) |>
    set_names(c("RA2017", "RA2018")) |>
    # Seleccionar escuelas en base (por ID1) y variables de trabajo
    map(\(x) filter(x, ID1 %in% CARTO$ESCUELAS$ID1)) |>
    map(\(x) select(x, ID1, all_of(paste0("X_", 1:6)), multi_pri)) |>
    bind_rows(.id = "RA") |>
    rowwise()  |>
    mutate(N_MATRIC = sum(across(c(starts_with("X_"), multi_pri)),
                          na.rm = T)) |>
    filter(N_MATRIC > 0) |>
    group_by(ID1, .drop = F ) |>
    summarise(N_MATRIC = mean(N_MATRIC, na.rm = T),
              N_MATRIC = round(N_MATRIC) )

# Bases finales ---------------------------------------------------

cli_li(col_yellow("Armado de bases finales (educación)"))

CARTO$ESCUELAS <- CARTO$ESCUELAS |>
  inner_join(ESC_RA, by = join_by(ID1)) |>
  inner_join(ESC_APR, by = join_by(ID1))

rm(ESC_RA, ESC_APR)

# cli_end(ulid)
# rm(ulid)

cli_alert_info("Guradando {.path PhD_04_escuela-con-datos.RData}.")
save.image("D:/Escritorio/PhD_data/PhD_04_escuela-con-datos.RData")
