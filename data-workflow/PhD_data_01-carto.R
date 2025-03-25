# Cargar cartografía
# -----------------*

cli_h2(col_blue("Cargando cartografía."))

CARTO <- list()

# ╠ UNIDADES ESPACIALES (INDEC) ------------------------------------------------------
# AMGBA, Provincias, Departamentos y Radios

  cli_li(col_yellow("Descargando cartografía de INDEC."))

  fileName <- tempfile()
  wfs <- paste("https://geoservicios.indec.gob.ar",
                "geoserver", "ows",
                "?service=wfs&version=1.0.0&request=GetCapabilities",
                sep = "/")

  download.file(wfs, fileName, quiet = T)
  request <- GMLFile$new(fileName)
  client <- WFSCachingClient$new(request)

  # Envolvente AMBA
  CARTO$AMGBA <- client$getLayer(layer = "eph:aglomerados_eph_4326", quiet = TRUE) |>
    filter(eph_codagl %in% c("32", "33")) |>
    st_transform(crs = 22183) |>
    summarise() |>
    nngeo::st_remove_holes()

  cli_alert_success("Envolvente AMBA")

  # Radios PBA + CABA
  # * No se puede usar wfs por errores en la fuente ('eph:radios_eph_4326')
  # * No se puede usar fuente wfs original ('geocenso2010:radios_codigo')
  #     porque fue dada de baja del servicio wfs de INDEC
  # * Se descargan los polígonos manualmente desde portal INDEC
  #     (https://www.indec.gob.ar/indec/web/Institucional-Indec-Codgeo)
  #     y se genera un preprocesamiento para unir las capas de CABA y PBA.
  #     ver procesamiento en 'data-workflow/PhD_prepare-carto-radios2010.R'

  CARTO$RADIO <- read_sf(here::here("data-workflow", "raw-data",
                                    "radios2010_AMBA.geojson")) |>
    st_transform(crs = st_crs(CARTO$AMGBA))

  cli_alert_success("Radios AMBA")

  # Departamentos
  # * La fuente wfs original ('geocenso2010:departamentos_codigo') no está
  #     disponible actualmente. Se reemplaza por 'sig:v_departamentos'

  sel <- str_sub(CARTO$RADIO$ID_RADIO, 1, 5) |> unique()

  CARTO$DPTO <- client$getLayer(layer = "sig:v_departamentos", quiet = TRUE) |>
    filter(link %in% sel) |>
    select(ID_DPTO = link, nomdpto = nombre) |>
    st_transform(crs = st_crs(CARTO$AMGBA))

  CARTO$DPTO_AMBA <- CARTO$DPTO |>
    st_intersection(CARTO$AMGBA)

  cli_alert_success("Departamentos AMBA")

  # Jurisdicciones
  CARTO$PCIA <-  CARTO$DPTO_AMBA |>
    mutate(ID_PCIA = str_sub(ID_DPTO, 1, 2) ) |>
    group_by(ID_PCIA) |>
    summarise()

  cli_alert_success("Jurisdicciones")

  # Callejero INDEC (no usado)
  # * Se prefiere OSM por tener mejor calidad de dato y ser más estable.

  # CALLE <- client$getLayer(layer = "sig:vias") |>
  #   # sig:cuadras | desarrollo:cuadras | sig:vias | desarrollo:vias
  #   select(nomencla:tipo, codloc, codaglo) |>
  #   st_transform(crs = st_crs(CARTO$AMGBA)) |>
  #   st_intersection(CARTO$AMGBA)

  unlink(fileName)
  rm(sel)
  rm(client, request, wfs, fileName)

# Entornos urbanos (Mariana Marcos) ------------------------------------------
# * Se utilizan los datos descargados de: http://hdl.handle.net/11336/190015
# * Se corrige radio "067780303", no disponible en la cartografía de INDEC.
#     Se considera posible error de tipeo y se reemplaza por radio "067780301"
#     (no clasificado y disponible en cartografía).
# * Alternativamente podrían usarse los radios 'recortados' en formato geojson,
#     pero falta información sobre el modo de procesamiento de los datos.

  cli_li(col_yellow("Agregar Entornos Urbanos (Mariana Marcos) a radios censales."))

  TH <- read.csv2(here::here("data-workflow", "raw-data",
                             "entornos-urbanos.csv"),
                  colClasses = c(ID_RADIO = "character") ) |>
    mutate(ID_RADIO = str_pad(ID_RADIO, 9, pad = "0"),
           ID_RADIO = ifelse(ID_RADIO == "067780303", "067780301", ID_RADIO),
           TH = factor(TIPO_HAB, levels = 1:9,
                       labels = c("Ciudad colonial",
                                  "Expansión de la ciudad colonial",
                                  "Cascos conurbanizados",
                                  "Vivienda social",
                                  "Urbanización cerrada",
                                  "Urbanización de origen informal",
                                  paste("NSE", c("Alto", "Medio", "Bajo"))
                                  )
                       )
           ) |>
    select(-TIPO_HAB)

  CARTO$RADIO <- CARTO$RADIO |>
    left_join(TH, by = "ID_RADIO")

  rm(TH)

# ╠ FFCC (GCBA) ---------------------------------------------------------------

cli_li(col_yellow("Descargando datos de FFCC (GCBA)."))

# Vias
CARTO$FFCC1 <- read_sf("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte-y-obras-publicas/estaciones-ferrocarril/red-de-ferrocarril.geojson")|>
  st_transform(st_crs(CARTO$AMGBA) ) |>
  st_intersection(CARTO$AMGBA[])

# Estaciones
CARTO$FFCC2 <- read_sf("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte-y-obras-publicas/estaciones-ferrocarril/estaciones-de-ferrocarril.geojson") |>
  st_transform(st_crs(CARTO$AMGBA) ) |>
  st_intersection(CARTO$AMGBA[])

# ╚ CALLEJERO (OSM) ---------------------------------------------------------------------

  # Descargar información de OSM --------------------------------------------
  ### Tarda aproximadamente 20 minutos

  cli_li(col_yellow("Descargando datos callejero de OSM."))

  # Crear buffer de 1000m alrededor de AMGBA y generar envolvente cóncava
  AMBA_bf <- CARTO$AMGBA |> st_buffer(dist = 1000) |> st_concave_hull(ratio = 0.2)

  CALLE <- CARTO$AMGBA |>
    st_transform(4326) |>
    st_bbox() |>
    # Realizar consulta en OSM
    opq() |>
    add_osm_features(features = list("highway" = "construction",
                                     "highway" = "living_street",
                                     "highway" = "residential",
                                     "highway" = "motorway",
                                     "highway" = "motorway_link",
                                     "highway" = "primary",
                                     "highway" = "primary_link",
                                     "highway" = "secondary",
                                     "highway" = "secondary_link",
                                     "highway" = "tertiary",
                                     "highway" = "tertiary_link",
                                     "highway" = "trunk",
                                     "highway" = "trunk_link",
                                     "highway" = "unclassified",
                                     "highway" = "service")) |>
    osmdata_sf() |>
    # Extraer capas necesarias y unificar en única base
    magrittr::extract(c("osm_lines", "osm_polygons")) |>
    bind_rows() |>
    # Seleccionar variables necesarias
    select(osm_id, name, highway, lanes, maxspeed, surface) |>
    # Recortar datos a AMBA (buffer)
    st_transform(st_crs(AMBA_bf)) |>
    st_intersection(AMBA_bf) |>
    # Castear a LINESTRING
    st_cast("MULTILINESTRING") |>
    st_cast("LINESTRING")

  rm(AMBA_bf)

  cli_alert_info(col_silver("Guardando {.path PhD_01_OSM-download.RData}."))
  save(CALLE,
       file = here::here("data-workflow", "intermediate-data",
                         "PhD_01_OSM-download.RData"))

  # Armado red urbana -------------------------------------------
  # load(here::here("data-workflow", "intermediate-data",
  #                 "PhD_01_OSM-download.RData"))

  cli_li(col_yellow("Armando red urbana (callejero)."))

  # round the coordinates
  st_geometry(CALLE) <- st_geometry(CALLE)  |>
    lapply(function(x) round(x, 0)) |>
    st_sfc(crs = st_crs(CALLE))

  # convert to sfnetworks
  roads <- as_sfnetwork(CALLE, directed = FALSE)

  # Limpieza de la red urbana -------------------------------------------
  # Se modifica orden de las acciones de limpieza.
  # * Si la subdivisión de la red (2) va luego de eliminar bucles en (1) se
  #   pierden rotondas (que tienen nodos internos que pueden coincidir con
  #   otros componentes), aumentando artificialmente número de componentes
  # * Si la eliminación de pseudonodos (3) va antes de el armado de la
  #   simplificación de intersecciones (4) mediante clusters, se pueden perder,
  #   potenciales intersecciones perdidas.
  #
  # Orden:
  # Subdivide (2) -> Simplify (1) -> Simplify intersections (4) -> smooth pseudo nodes (3)

  cli_li(col_yellow("Limpieza de red urbana (callejero)."))

  cli_progress_bar("Limpieza de red:", total = 5, type = "tasks")

  cli_progress_update(status = "Subdivide and Simplify network")

  roads_clean <-  roads |>
    # 2. Subdivide
    convert(to_spatial_subdivision) |>
    # 1. Simplify
    activate("edges") |>
    arrange(edge_length()) |>
    filter(!edge_is_multiple()) |>
    filter(!edge_is_loop()) # ≃ convert(to_spatial_simple)

  cli_alert_success("Subdivide network")
  cli_alert_success("Simplify network")
  cli_progress_update(status = "Simplify intersections")

  # 4. simplify intersections

  # Retrieve the coordinates of the nodes.
  clusters <- roads_clean %>%
    activate("nodes") %>%
    st_coordinates() |>
    # Nodes within a distance of 2 from each other will be in the same cluster.
    # A node is assigned a cluster even if it is the only member of that cluster.
    dbscan(eps = 2, minPts = 1) |>
    pluck("cluster")

  roads_clean <- roads_clean |>
    activate("nodes") |>
    mutate(cls = clusters,               # Add the cluster information to the nodes of the network.
           cmp = group_components()) |>  # Verificar que los  nodos agrupados estén conectados en la red.
    # Contraer la red (utilizando centroide del clúster y eliminando nodos individuales)
    convert(to_spatial_contracted,
            cls, # cmp,
            simplify = TRUE)

  cli_alert_success("Simplify intersections")
  cli_progress_update(status = "Smooth pseudo nodes")

  # 3. smooth pseudo nodes
  roads_clean <- roads_clean |>
    convert(to_spatial_smooth)

  cli_alert_success("Smooth pseudo nodes")
  cli_progress_update(status = "Weigh edges and retain main component")

  # cat("Red original:", count_components(roads), "\n",
  #     "Red parcial:", count_components(roads_clean), "\n")

  # 5. Retenemos el componente principal de la red y ponderamos la red por la distancia del eje
  roads_clean <- roads_clean |>
    filter(group_components() == 1) |>    # Select 1st component
    activate("edges") |>
    mutate(weight = edge_length())        # Add weights to edges

  CALLE <- roads_clean
  rm(clusters, roads, roads_clean)

  cli_alert_success("Weigh edges and retain main component")

  cli_progress_done()

# Guardar bases -------------------------------------------

  cli_alert_info(col_silver("Guradando {.path data-workflow/intermediate-data/PhD_02_carto.RData}."))

  save(CARTO,
       file = here::here("data-workflow", "intermediate-data",
                         "PhD_02_carto.RData"))

  cli_alert_info(col_silver("Guradando datos finales {.path CALLE.rda}."))

  # usethis::use_data(CALLE, overwrite = TRUE, internal = FALSE)
  save(CALLE, file = here::here("data-workflow", "final-data", "CALLE.rda"))
  rm(CALLE)
