# Load libraries ----------------------------

library(cli)
library(fs)
library(here)

library(usethis)
library(devtools)

library(tidyverse)
library(data.table)
library(slam)

library(units)
library(sf)

library(knitr)
library(flextable)
library(officer)

library(FactoMineR)
library(segregation)
library(TraMineR)

library(lpSolve)
library(Rglpk)

library(igraph)
library(sfnetworks)

library(spatstat)
library(gstat)
library(variosig)

library(factoextra)
library(ggrepel)
library(ggpubr)
library(ggtern)
library(latex2exp)
library(ggExtra)
library(ggsci)

devtools::load_all()

set_flextable_defaults(big.mark = " ",
                       decimal.mark = ",",
                       font.size = 7,
                       table_align = "center",
                       font.family = "Alegreya Sans",
                       na_str = "")

# Capítulo 4: Segregación en el dominio residencial

source(here::here("script", "PhD_01-residential.R"))

# Capítulo 5: Homogamia en el dominio residencial

source(here::here("script", "PhD_02-homogamy.R"))

# Capítulo 6: Segregación en los espacios de actividad
source(here::here("script", "PhD_03-mobilities.R"))

# Capítulo 7: Segregación en el dominio escolar
# source(here::here("script", "PhD_04-education.R"))
