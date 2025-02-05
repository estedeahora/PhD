# Preparación de bases de datos para PhD Cs. Soc.

# Command Line Interfaces
library(cli)
library(fs)

# Read data
library(readxl)

# data wrangling
library(tidyverse)
library(data.table)
library(magrittr)

# Spatial data handling
library(sf)
library(nngeo)

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
fs::dir_create(here::here("raw-data", "intermediate-data"))

# BASES NECESARIAS:

# Nombre                    Año       Fuente
# APRENDER (Estudiantes) 2017/2018    ME      https://www.argentina.gob.ar/educacion/evaluacion-e-informacion-educativa/base-de-datos?tca=biGs15DqyLzvPLuIOhNm6_5G8xLVFWSaNRGR7mHJFtY
# RA BU2 (Matrícula)     2017/2018    ME      https://www.argentina.gob.ar/educacion/evaluacion-e-informacion-educativa/base-de-datos?tca=biGs15DqyLzvPLuIOhNm6_5G8xLVFWSaNRGR7mHJFtY
# CENSO                    2010       INDEC
# ENMODO                   2018       GCBA    https://data.buenosaires.gob.ar/dataset/encuesta-movilidad-domiciliaria

# Cartografía
# ----------*
# [x] Envolvente AMBA
# [x] Radios AMBA
# [x] Departamentos AMBA
# [x] Jurisdicciones AMBA
# [x] FFCC (estaciones y líneas)
# [x] Red vial urbana (OSM)
# -> CARTO = list(AMGBA, RADIO, DPTO, PCIA, FFCC1, FFCC2)
# -> CALLE [sfnetwork]

source(here::here("raw-data", "PhD_data_01-carto.R"), encoding = "UTF-8")

# Población
# --------*
# [x] Base de Radios Censo AMBA con cantidad de personas por radio
# [x] Base de hogares con nivel educativo de los cónyuges
# [x] Base de hogares con cantidad de personas en edad escolar
# [-] Distribución de población facebook
# ->

source(here::here("raw-data", "PhD_data_02-population.R"), encoding = "UTF-8")

# Educación
# --------*
# [x] Base de escuelas AMBA (comun primaria y secundaria) con composición
#       de MNI padres
# [x] Base de alumnos por escuelas con composición de MNI de padres
# [x] Base de matrícula de alumnos por escuelas (total de 1ero a 6to grado)
# -> CARTO = c(CARTO, ESCUELAS)

source(here::here("raw-data", "PhD_data_03-education.R"), encoding = "UTF-8")

# Movilidad
# --------*
# [ ] ENMODO

source(here::here("raw-data", "PhD_data_04-mobility.R"), encoding = "UTF-8")
