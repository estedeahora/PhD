# Cargar educación
# ---------------*

cli_h2(col_blue("Cargando datos educativos (RA + APRENDER)"))

if(!exists("CARTO", inherits = F) || is.null(CARTO$RADIO[["ls"]])){
  cli_alert_info(
    col_silver(
      paste0("Cargando cartografía",
             ": {.path data-workflow/intermediate-data/PhD_04_carto-censo.RData}.")
    )
  )

  load(here::here("data-workflow", "intermediate-data",
                  "PhD_04_carto-censo.RData"))
}

# ╠ Mapa Educativo Nacional -----------------------------
  # http://mapa.educacion.gob.ar/geoservicios
  #
  # Listado de modalidades:
  # esc_aux <- CARTO$ESCUELAS
  # esc_aux$nen[!is.na(esc_aux$nen)] |>
  #     unique() |>
  #     paste(collapse = "") |>
  #     str_split("; ", simplify = T) |>
  #     sort() |> unique()
  # rm(esc_aux)

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

  AMBA_bf <- CARTO$AMGBA |> st_buffer(dist = 1000) |> st_concave_hull(ratio = 0.2)

  CARTO$ESCUELAS <- client$getLayer("establecimiento_educativo", quiet = TRUE) |>
    mutate(CUE = str_sub(cue, start = 1L, end = -3L),
           ANEXO = str_sub(cue, start = -2L, end = -1L),
           PROV = str_sub(cue, start = 1L, end = 2L)) |>
    select(CUEANEXO = cue, CUE, ANEXO, PROV, name = fna,
           SECTOR = ges, mde, nen, geometry = the_geom) |>
    st_transform(st_crs(AMBA_bf)) |>
    st_intersection(AMBA_bf) |>
    filter(str_detect(mde, "Común")) |>
    mutate(ID1 = PhD_CUE_to_ID(CUEANEXO),
           NIV_PRI = str_detect(nen, "Común-Primaria 6años|Común-Primaria 7años"),
           NIV_SEC = str_detect(nen, "Común-Secundaria-Ambos Ciclos|Común-Secundaria-Ciclo Básico"),
           NIV_TEC = str_detect(nen, "Secundaria TécProf\\(INET\\)") ) |>
    filter(NIV_PRI | NIV_SEC | NIV_TEC) |>
    select(ID1, everything())

  unlink(fileName)
  rm(AMBA_bf)
  rm(request, client, fileName, wfs)

# ╠ APRENDER --------------------------------------------
  # Respaldo de datos disponible en: https://doi.org/10.5281/zenodo.14888047
  # Se usar promedio de años 2017 y 2019 para disminuir sesgos del operativo.

  cli_li(col_yellow("Cargando datos APRENDER (ME)"))

  if(Sys.getenv("DATA_FOLDER") == "" || Sys.getenv("CENSO_FOLDER") == ""){

    # Descargar datos de Zenodo
    folder_temp <- tempdir()
    inborutils::download_zenodo(doi = "10.5281/zenodo.14888047",
                                folder_temp)
    zip_temp <- fs::path(folder_temp, "ME_data", ext = "zip")

    unzip(zipfile = zip_temp, exdir = folder_temp, unzip = "unzip")
    ME_files <- fs::path_ext_remove(zip_temp)
    AP_files <- fs::dir_ls(ME_files, regexp = "APRENDER.*\\.csv$")

    unlink(zip_temp)
    rm(folder_temp, zip_temp)

  }else{

    AP_files <- fs::path(Sys.getenv("DATA_FOLDER"), "ME_Aprender", paste0("AP", c(2017,2019)),
                         paste0("APRENDER", c(2017,2019),
                                "_BU_Estudiantes-secundaria-5-6-año_censo-L+M"),
                         ext = "csv")
  }

  APRENDER <- AP_files |>
    map(\(.x) read.csv2(.x, colClasses = c("ID1" = "character")) ) |>
    set_names(c("AP2017", "AP2019")) |>
    # Seleccionar escuelas en base (por ID1 y nivel) y variables de trabajo
    map(\(x) filter(x, ID1 %in% CARTO$ESCUELAS$ID1[CARTO$ESCUELAS$NIV_SEC])) |>
    map2(list(c("ap10", "ap11"),    # Variables EDU_MAD/EDU_PAD 2017
              c("ap16", "ap17")),    # Variables EDU_MAD/EDU_PAD 2019
         \(x, y) select(x, ID1:idalumno, ponder,
                        EDU_MAD = all_of(y[[1]]),
                        EDU_PAD = all_of(y[[2]]))) |>
    # Unificar bases
    bind_rows(.id = "APRENDER") |>
    # Generar variable EDU
    mutate(across(.cols = EDU_MAD:EDU_PAD,
                  .fns =
                    ~case_when(is.na(.x) | .x < 0 | .x == 8 ~ 0,
                               # Categorías AP2017
                               APRENDER == "AP2017" & .x %in% 1:4 ~ 1,
                               APRENDER == "AP2017" & .x %in% 5:6 ~ 2,
                               APRENDER == "AP2017" & .x %in% 7   ~ 3,
                               # Categorías AP2019
                               APRENDER == "AP2019" & .x %in% 1:3 ~ 1,
                               APRENDER == "AP2019" & .x %in% 4   ~ 2,
                               APRENDER == "AP2019" & .x %in% 5:7 ~ 3) ),
           EDU = ifelse(EDU_MAD >= EDU_PAD, EDU_MAD, EDU_PAD),
           EDU = factor(EDU, levels = 1:3, labels = c("PRI", "SEC", "SUP") ))

  ESC_APR <- APRENDER |>
    # Calcular totales de alumno por escuela
    # N_op = total ponderado; n = encuestados; n_sinNA = con respuestas en EDU
    group_by(ID1, APRENDER) |>
    mutate(N_op = round(sum(ponder, na.rm = T), 1),
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
              N_AP = sum(N_op, na.rm = T),
              n_sinNA = sum(n_sinNA, na.rm = T),
              .groups = "drop") |>
    pivot_wider(id_cols = c(ID1, N_AP, n_sinNA),
                names_from = EDU, values_from = p)

  rm(AP_files, APRENDER)

# ╚ Relevamiento Anual (RA) -------------------------------------------------
  # Respaldo de datos disponible en: https://doi.org/10.5281/zenodo.14888047

  cli_li(col_yellow("Cargando datos Relevamiento Anual (ME)"))

  if(Sys.getenv("DATA_FOLDER") == "" || Sys.getenv("CENSO_FOLDER") == ""){

    RA_files <- fs::dir_ls(ME_files, regexp = "RA.*\\.csv$")

    rm(ME_files)

  }else{
    RA_files <- fs::path(Sys.getenv("DATA_FOLDER"), "ME_RA", paste0("RA", 2017:2019),
                         paste0("RA", 2017:2019,
                                "_BU2_Matricula"),
                         ext = "csv")
  }

  ESC_RA <- RA_files |>
    map(\(.x) read.csv2(.x, colClasses = c("ID1" = "character"))) |>
    set_names(c("RA2017", "RA2018", "RA2019")) |>
    # Seleccionar escuelas en base (por ID1 y nivel) y variables de trabajo
    map(\(x) filter(x, ID1 %in% CARTO$ESCUELAS$ID1[CARTO$ESCUELAS$NIV_SEC])) |>
    map(\(x) select(x, ID1, all_of(paste0("X_", 8:12)), multi_sec)) |>
    bind_rows(.id = "RA") |>
    rowwise()  |>
    mutate(N_MATRIC = sum(across(c(starts_with("X_"), multi_sec)),
                          na.rm = T)) |>
    filter(N_MATRIC > 0) |>
    group_by(ID1, .drop = F ) |>
    summarise(N_MATRIC = mean(N_MATRIC, na.rm = T),
              N_MATRIC = round(N_MATRIC) )

  rm(RA_files)

# Bases finales ---------------------------------------------------

  cli_li(col_yellow("Sumar datos educativos a base `CARTO$ESCUELAS`"))

  CARTO$ESCUELAS <- CARTO$ESCUELAS |>
    left_join(ESC_RA, by = join_by(ID1)) |>
    left_join(ESC_APR, by = join_by(ID1))

  rm(ESC_RA, ESC_APR)

  cli_alert_info(col_silver("Guradando {.path data-workflow/intermediate-data/PhD_05_carto-educ.RData}."))
  save(CARTO,
       file = here::here("data-workflow", "intermediate-data",
                         "PhD_05_carto-educ.RData"))

