
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, stringr, tidyverse, readxl, sf, velox)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
tbl <- read_excel('../_data/_tbl/02.conflicto_version-final-con-medio-bajo-2002-2013.xlsx')
adm <- st_read('../_data/_shp/_base/Municipios_SIGOT_geo.shp')

# Cleaning tables
tbl <- setNames(tbl, c('divipola', 'divipola_1', 'Departamento', 'Municipio', 'Indice_Num', 'Indice_Alp', 'Region'))
tbl <- tbl %>% 
  dplyr::select(divipola, Region, Departamento, Municipio, Indice_Num, Indice_Alp)
adm <- adm %>% 
  dplyr::select(NOMBRE_DPT, NOM_MUNICI, ID_ESPACIA)
adm <- adm %>% 
  mutate(ID_ESPACIA = as.numeric(as.character(ID_ESPACIA)))
tbl <- arrange(tbl, divipola)
adm <- arrange(adm, ID_ESPACIA)

# Inner join
pdt <- inner_join(adm, tbl, by = c('ID_ESPACIA' = 'divipola'))
njn <- anti_join(adm, tbl, by = c('ID_ESPACIA' = 'divipola')) 

# Write the final table
st_write(obj = pdt, dsn = '../_data/_shp/_pdt', layer = 'pdt', driver = 'ESRI Shapefile', update = TRUE)


