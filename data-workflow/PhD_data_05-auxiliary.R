# Límite para mapa

load(file = here::here("data-workflow", "final-data", "CARTO.rda"))

# Zonas de interes ------------------------------------------------

  # Detalle 1: ZN
  # Zona norte (Vicente López y San Isidro) + CABA
  ZN1 <- CARTO$RADIO |>
    filter(PROV  == "02" | DPTO %in% c("861", "756"))

  # Detalle 2: EN
  # Expansión urbana norte. Tigre, Pilar y Escobar
  ZN2 <- CARTO$RADIO |>
    filter(DPTO %in% c("805", "638",  "252"))

  # Detalle 3: CF
  # Cascos urbanos ferroviarios. FFCC Sarmiento (Ramos-Haedo-Morón-Castelar-Ituzaingó)
  ZN3 <- CARTO$RADIO |>
    filter(DPTO %in% c("568",  "410", "427"))

  #  Detalle 4: ZM
  # Zonas marginales. Florencio Varela, Quilmes (Solano) y Almirante Brown (Calzada)
  ZN4 <- CARTO$RADIO |>
    filter(DPTO %in% c("274", "028", "658"))
  # ZN4 <- CARTO$RADIO |>
# Límites de zonas ------------------------------

  ZN1_lim <- ZN1 |> filter(!is.na(TH) & DPTO %in% c("756", "001")) |> st_bbox()
  ZN2_lim <- ZN2 |> filter(!is.na(TH)) |> st_bbox()
  ZN3_lim <- ZN3 |> filter(DPTO == "568" & !is.na(TH)) |> st_bbox()
  ZN4_lim <- ZN4 |> filter(DPTO == "274" & !is.na(TH)) |> st_bbox()

  ZN3_lim[2] = ZN3_lim[2] + (ZN3_lim[4] - ZN3_lim[2]) / 3
  ZN3_lim[4] = ZN3_lim[4] - (ZN3_lim[4] - ZN3_lim[2]) / 3.5

  ZN4_lim[1] = ZN4_lim[1] - (ZN4_lim[3] - ZN4_lim[1]) / 5
  ZN4_lim[3] = ZN4_lim[3] - (ZN4_lim[3] - ZN4_lim[1]) / 2
  ZN4_lim[2] = ZN4_lim[2] + (ZN4_lim[4] - ZN4_lim[2]) / 1.5
  ZN4_lim[4] = ZN4_lim[4] + (ZN4_lim[4] - ZN4_lim[2]) / 2.8

# Bases finales ---------------------------------------------------

  AUX <- list()

  AUX$lim_AMBA <- CARTO$RADIO |> filter(!is.na(TH)) |> st_bbox()

  AUX$lim_DETAILS <- list(ZN1 = ZN1_lim, ZN2 = ZN2_lim,
                          ZN3 = ZN3_lim, ZN4 = ZN4_lim)

  AUX$ZN <- list(ZN1 = ZN1, ZN2 = ZN2, ZN3 = ZN3, ZN4 = ZN4)

  rm(ZN1_lim, ZN2_lim, ZN3_lim, ZN4_lim)
  rm(ZN1, ZN2, ZN3, ZN4)

  # Save
  cli_alert_info(col_silver("Guradando datos auxiliares {.path AUX-data.rda}."))
  save(AUX, file = here::here("data-workflow", "final-data", "AUX-data.rda"))

  rm(CARTO)
  rm(AUX)
