# Load libraries ----------------------------

library(cli)
library(fs)
library(here)

library(usethis)
library(devtools)

library(tidyverse)
library(sf)

library(ggpubr)
library(ggtern)

library(knitr)
library(flextable)
library(officer)

library(FactoMineR)
library(segregation)

devtools::load_all()

load(file = here::here("data-workflow", "final-data", "AUX.rda"))
load(file = here::here("data-workflow", "final-data", "CARTO.rda"))

# Capítulo 4: Segregación en el dominio residencial

source(here::here("script", "PhD_01-residential-segregation.R"))

# Capítulo 5: Homogamia en el dominio residencial

load(here::here("data-workflow", "final-data", "HOG.Rda"))

source(here::here("script", "PhD_02-homogamy.R"))
