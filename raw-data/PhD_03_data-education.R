# Cargar educación
# ---------------*

# load("D:/Escritorio/PhD_data/PhD_03_CENSO.RData")

cli_h2(col_blue("Cargando datos educativos"))

# ulid <- cli_ul()

### ╠ Mapa Educativo Nacional -----------------------------
# http://mapa.educacion.gob.ar/
# http://mapa.educacion.gob.ar/geoservicios

cli_li(col_yellow("Descargando Mapa Educativo Nacional (ME)"))

# CUE_ME2() Enmascara CUE para coincidir con ID1 (ME 2019 en adelante)
CUE_ME2 <- function(x, verbose = F){

  op <- options(scipen = 999)
  on.exit(options(op), add = T)

  res <- x |>
    mutate(k = str_sub(CUEANEXO, start = 6, end = 7) |> as.numeric(),
           k1 = str_sub(CUEANEXO, 1, 5) |> as.numeric(),
           k2 = str_sub(CUEANEXO, 5, 9) |> as.numeric(),

           sel = as.numeric(CUE) <= 5000010 & k %in% 0:10,

           clave = ifelse(sel, k + 1, k + 4),

           x1 = k1 * clave,
           x2 = k2 * clave,

           ID1 = ifelse(sel, paste0(x1, x2), paste0(x2, x1)) |>
             # Repetir tres veces resultado
             strrep(times = 3) |>
             # Cortar ID en lenght = 15 elementos
             str_sub(end = 15) #|>
           # Transformar a numérico
           #as.numeric()
    ) |>

    select(CUE, ANEXO, CUEANEXO, ID1,
           k, k1, k2, x1, x2, clave, sel, everything())

  if(!verbose){
    res <- res |>
      select(-k, -k1, -k2, -x1, -x2, -sel, -clave )
  }
  return(res)
}

# options(timeout = 300)

fileName <- tempfile()
wfs <- "http://mapa.educacion.gob.ar/geoserver/ows?service=wfs&version=1.1.0&request=GetCapabilities"

download.file(wfs, fileName, quiet = T)
request <- GMLFile$new(fileName)
client <- WFSCachingClient$new(request)

CARTO$ESCUELAS <- client$getLayer("establecimiento_educativo", quiet = TRUE) |>
  mutate(CUE = str_sub(cue, start = 1L, end = -3L),
         ANEXO = str_sub(cue, start = -2L, end = -1L),
         PROV = str_sub(cue, start = 1L, end = 2L)) |>
  select(CUE, ANEXO, CUEANEXO = cue, PROV, name = fna,
         SECTOR = ges, mde, nen, geometry = the_geom) |>
  st_transform(st_crs(CARTO$AMGBA)) |>
  st_intersection(CARTO$AMGBA) |>
  filter(str_detect(mde, "Común")) |>
  mutate(NIV_PRI = str_detect(nen, "Común-Primaria 6años|Común-Primaria 7años"),
         NIV_SEC = str_detect(nen, "Común-Secundaria-Ambos Ciclos"),
         NIV_TEC = str_detect(nen, "Secundaria TécProf\\(INET\\)") ) |>
  filter(NIV_PRI | NIV_SEC | NIV_TEC) |>
  CUE_ME2()

# Listado de modalidades:
# a <- CARTO$ESCUELAS
# a$nen[!is.na(a$nen)] |> unique() |> paste(collapse = "") |> str_split("; ", simplify = T) |> sort() |> unique()
# rm(a)

unlink(fileName)
rm(request, client, fileName, wfs)
rm(CUE_ME2)

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

# Primario
APRENDER <- paste0("D:/Documentos/Fuentes de datos/ME Aprender/AP", 2017:2018,
                   "/APRENDER", 2017:2018, "_BU_Estudiantes-primaria-6-grado_censo",
                   c("-CS+CN", "-L+M"), ".csv") |>
  map(\(.x) read.csv2(.x, colClasses = c("ID1" = "character")) ) |>
  set_names(c("AP2017", "AP2018")) |>
  # Seleccionar escuelas en base (por ID1) y variables de trabajo
  map(\(x) filter(x, ID1 %in% CARTO$ESCUELAS$ID1)) |>
  map2(list(c("ap10", "ap11"),    # Variables EDU_MAD/EDU_PAD 2017
            c("ap9", "ap10")),    # Variables EDU_MAD/EDU_PAD 2018
       \(x, y) select(x, ID1:idalumno, ponder,
                      EDU_MAD = all_of(y[[1]]), EDU_PAD = all_of(y[[2]]))) |>
  # Unificar bases
  bind_rows(.id = "APRENDER") |>
  # Generar variable EDU
  mutate(across(.cols = EDU_MAD:EDU_PAD,
                .fns = ~case_when(.x < 0 | .x == 8 ~ 0,
                                  .x %in% 1:4 ~ 1,
                                  .x %in% 5:6 ~ 2,
                                  .x %in% 7 ~ 3) ),
         EDU = ifelse(EDU_MAD >= EDU_PAD, EDU_MAD, EDU_PAD),
         EDU = factor(EDU, levels = 1:3, labels = c("PRI", "SEC", "SUP") ))

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
