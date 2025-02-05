library(tidyverse)
library(sf)

caba <- read_sf(here::here("_rawdata", "raw-data", "radios2010_CABA.geojson")) |>
  select(ID_RADIO = LINK, PROV, DPTO = DEPTO)

pba <- read_sf(here::here("_rawdata", "raw-data", "radios2010_PBA.geojson")) |>
  select(ID_RADIO = link) |>
  mutate(PROV = str_sub(ID_RADIO, start = 1, end = 2),
         DPTO = str_sub(ID_RADIO, start = 3, end = 5))

amba <- bind_rows(caba, pba) |>
  group_by(ID_RADIO, PROV, DPTO) |>
  summarise() |>
  st_intersection(CARTO$AMGBA)

write_sf(amba, here::here("_rawdata", "raw-data", "radios2010_AMBA.geojson"))

rm(caba, pba, amba)

amba$ID_RADIO |> duplicated() |> sum()
# amba$ID_RADIO[amba$ID_RADIO |> duplicated()]

# table(amba$ID_RADIO %in% TH$ID_RADIO)
# table(TH$ID_RADIO %in% amba$ID_RADIO)
# table(TH$ID_RADIO %in% amba_rec$ID_RADIO)

# TH$ID_RADIO[!TH$ID_RADIO %in% amba$ID_RADIO]
