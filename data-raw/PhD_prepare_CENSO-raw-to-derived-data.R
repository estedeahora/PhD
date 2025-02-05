### ╠ CARTO INDEC ------------------------------------------------------
#
#     CARTO <- list()
#
#     wfs <- "https://geoservicios.indec.gob.ar/geoserver/ows?service=wfs&version=1.0.0&request=GetCapabilities"
#
#     fileName <- tempfile()
#
#     download.file(wfs, fileName)
#     request <- GMLFile$new(fileName)
#     client <- WFSCachingClient$new(request)
#
#     # Envolvente AMGBA
#     CARTO$AMGBA <- client$getLayer(layer = "eph:aglomerados_eph_4326") |>
#       filter(eph_codagl %in% c("32", "33", "02")) |>
#       st_transform(crs = 22183) |>
#       summarise() |>
#       st_remove_holes()
#
#     # Radios PBA + CABA
#     CARTO$RADIO <- client$getLayer(layer = "geocenso2010:radios_codigo") |>
#       filter(codpcia %in% c("02", "06"))|>
#       select(-gml_id, -color_psid, ID_RADIO = link) |>
#       st_transform(crs = 22183) |>
#       st_intersection(CARTO$AMGBA)

    # rm(client, request, fileName, wfs)



### ╠ Anillo + Sector -------------------------------------------------------

    # LOC_COD <- read.csv2("raw_data/COD_ANILLO_SECTOR.csv") |>
    #   select(Localidad = COD_LOCALIDAD, SECTOR, ANILLO) |>
    #   mutate(Localidad = str_pad(Localidad, 5, pad = 0))
    #
    # CARTO$LOC <- read_sf("raw_data/AMBA-Localidades/AMBA Localidades.shp") |>
    #   filter(!st_is_empty(geometry)) |>
    #   left_join(LOC_COD, by = "Localidad") |>
    #   st_transform(crs = 22183)
    #
    # rm(LOC_COD)
    #
    # CARTO$RADIO |>
    #   select(ID_RADIO) |>
    #   st_point_on_surface() |>
    #   st_intersection(CARTO$LOC[c("SECTOR", "ANILLO")]) |>
    #   st_drop_geometry() |>
    #   write.csv("derived_data/RADIO_ZONIFICACION_AMBA.csv",
    #             row.names = F, fileEncoding = "UTF8")

### ╚ Datos CENSO 2010 -------------------------------------------------------

    path_CENSO <- "D:/Documentos/Fuentes de datos/INDEC Censo/Censo 2010/C2010 Basico/"

    RADIO <- read_csv2(paste0(path_CENSO, "PROV.csv"))  |>
      left_join(read_csv2(paste0(path_CENSO, "DPTO.csv")),
                by = "PROV_REF_ID", multiple = "all")  |>
      left_join(read_csv2(paste0(path_CENSO, "FRAC.csv")),
                by = "DPTO_REF_ID", multiple = "all")  |>
      left_join(read_csv2(paste0(path_CENSO, "RADIO.csv")),
                by = "FRAC_REF_ID", multiple = "all")   |>
      mutate(ID_DTO = paste0(IDPROV, IDDPTO),
             ID_RADIO = paste0(ID_DTO, IDFRAC, IDRADIO)) |>
      select(ID_RADIO, IDPROV, NOMPROV, IDDPTO, NOMDPTO,
             IDFRAC, IDRADIO, # ZONA, ANILLO,
             RADIO_REF_ID)  |>
      filter(ID_RADIO %in% CARTO$RADIO$ID_RADIO)

    # Vivienda 2010
    VIVIENDA <- fread(paste0(path_CENSO, "VIVIENDA.csv"))
    CENSO <- RADIO %>%
      left_join(VIVIENDA, by = "RADIO_REF_ID", multiple = "all")

    # Hogar 2010
    HOGAR <- fread(paste0(path_CENSO, "HOGAR.csv"))
    CENSO <- CENSO |>
      left_join(HOGAR, by = "VIVIENDA_REF_ID", multiple = "all") |>
      filter(!is.na(HOGAR_REF_ID) ) |>
      select(ID_RADIO:VIVIENDA_REF_ID, HOGAR_REF_ID)

    rm(RADIO, VIVIENDA, HOGAR)

    # Persona 2010
    PERSONA <- fread(paste0(path_CENSO, "PERSONA.csv"))

    CENSO <- CENSO |>
      left_join(PERSONA, by = "HOGAR_REF_ID", multiple = "all")   |>
      select(-RADIO_REF_ID)

    rm(PERSONA, path_CENSO)

### ╠ Guardar datos finales ----------------------------------------------------

    save(CENSO, file = "derived_data/GBA.RData")
    write.csv(CENSO, "derived_data/GBA.csv",
              row.names = F, fileEncoding = "UTF8")

