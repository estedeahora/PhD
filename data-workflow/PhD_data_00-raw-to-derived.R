# Preparación de bases de datos para PhD Cs. Soc.

# Command Line Interfaces
library(cli)
library(fs)
library(tictoc)

# Read data
library(readxl)
# install.packages("inborutils", repos = c(inbo = "https://inbo.r-universe.dev",
#                                          CRAN = "https://cloud.r-project.org"))
library(inborutils)   # Descarga de Zotero

# data wrangling
library(tidyverse)
library(data.table)
library(magrittr)
library(progressr)    # barra de progreso

# Parallel processing
library(future)
library(furrr)

# Spatial data handling
library(sf)
library(nngeo)        # remove holes

# Download data
library(osmdata)
library(rwfs)

# Networks
library(igraph)
library(tidygraph)
library(sfnetworks)

# Statistical analysis
library(dbscan)        # Clúster
library(segregation)   # segregation

library(devtools)
load_all()

# Option settings
options(cli.progress_show_after = 0)

# Intermediate data folder
fs::dir_create(here::here("data-workflow", "intermediate-data"))
fs::dir_create(here::here("data-workflow", "final-data"))

# BASES NECESARIAS:

# Nombre                    Año       Fuente
# APRENDER (Estudiantes) 2017/2019    ME      https://www.argentina.gob.ar/educacion/evaluacion-e-informacion-educativa/base-de-datos?tca=biGs15DqyLzvPLuIOhNm6_5G8xLVFWSaNRGR7mHJFtY
# RA BU2 (Matrícula)     2017/2019    ME      https://www.argentina.gob.ar/educacion/evaluacion-e-informacion-educativa/base-de-datos?tca=biGs15DqyLzvPLuIOhNm6_5G8xLVFWSaNRGR7mHJFtY
# CENSO                    2010       INDEC
# ENMODO                   2018       GCBA    https://data.buenosaires.gob.ar/dataset/encuesta-movilidad-domiciliaria

# Cartografía
# ----------*
# [x] Envolvente AMBA
# [x] Radios AMBA
# [x] Departamentos AMBA (completos y recortados a AMBA)
# [x] Jurisdicciones AMBA (provincias recortados a AMBA)
# [x] FFCC (estaciones y líneas)
# [x] Red vial urbana (OSM)
# -> CARTO = list(AMGBA, RADIO, DPTO, DPTO_AMBA, PCIA, FFCC1, FFCC2)
# -> CALLE [sfnetwork]

source(here::here("data-workflow", "PhD_data_01-carto.R"), encoding = "UTF-8")

# Población
# --------*
# [x] Base de Radios Censo AMBA con cantidad de personas por radio
# [x] Base de hogares con nivel educativo de los cónyuges
# [x] Base de hogares con cantidad de personas en edad escolar
# [-] Distribución de población facebook
# -> CENSO_CON: datos censales de cónyuges (P01 %in% 1:2)
# -> HOG: datos del hogar con información de educación de cónyuges y
#           educación y homogamia del hogar.
# -> Modifica CARTO["RADIO"]: Se agrega información de cantidad de hogares,
#           cantidad de hogares conyugales, cantidad de hogares con homogamia,
#           cantidad de hogares por nivel educativo (máximo cónyuge), cantidad
#           de niños en edad educativa primaria, probabilidad de homogamia
#           por nivel educativo y ls (segregación local).

source(here::here("data-workflow", "PhD_data_02-population.R"), encoding = "UTF-8")

# Educación
# --------*
# [x] Base de escuelas AMBA (comun primaria y secundaria) con composición
#       de MNI padres
# [x] Base de alumnos por escuelas con composición de MNI de padres
# [x] Base de matrícula de alumnos por escuelas (total de 1ero a 6to grado)
# -> Modifica CARTO, agregando "ESCUELAS" con datos sobre matrícula y composición
#          de MNI de padres por escuela.

source(here::here("data-workflow", "PhD_data_03-education.R"), encoding = "UTF-8")

# Movilidad
# --------*
# [x] ENMODO: Base de datos de movilidad domiciliaria de GCBA.
# -> ENMODO = list(HOG, PER, VIA, ETA)

source(here::here("data-workflow", "PhD_data_04-mobility.R"), encoding = "UTF-8")

# Elementos auxiliares
# -------------------*
# -> AUX = list(lim_AMBA,
#               lim_DETAILS = list(ZN1, ZN2, ZN3, ZN4),
#               ZN = list(ZN1, ZN2, ZN3, ZN4))

source(here::here("data-workflow", "PhD_data_05-auxiliary.R"), encoding = "UTF-8")
