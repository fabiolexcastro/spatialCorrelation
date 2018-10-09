
# Load libraries
library(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, stringr, velox, sf, readxl)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use
myJoin <- function(yr){
  # yr <- 2007
  tb <- tbl %>% dplyr::filter(PERIODO %in% yr)
  sf <- inner_join(st_as_sf(adm), tb, by = c('ID_ESPACIA' =  'COD_MUNI'))
  st_write(obj = sf, dsn = '../_data/_shp/_production', layer = paste0('production_cocoa_', yr), driver = 'ESRI Shapefile', update = TRUE)
  print('Done')  
}

# Load data
tbl <- read_excel('../_data/_tbl/Datos produccion y rendimiento de cacao.xlsx')
colnames(tbl) <- colnames(tbl) %>% gsub(' ', '_', .)
adm <- shapefile('../_data/_shp/_base/Municipios_SIGOT_geo.shp')
adm@data$ID_ESPACIA <- as.numeric(adm@data$ID_ESPACIA)

# Tidying data
tbl <- tbl %>% 
  setNames(c('COD_DPTO', 'DPTO', 'COD_MUNI', 'MPIO', 'GRUPO_CULTIVO', 'SUBGRUPO_CULTIVO', 'CULTIVO', 'DESAGREGACION', 'COD_CULTIVO',
             'SCIENTIFIC_NAME', 'PERIODO', 'AREA_SEMBRADA_HA', 'AREA_COSECHADA_HA', 'PRODUCCION', 'RDTOS', 'ESTADO_FISICO_PRODUCCION'))
tbl <- tbl %>% dplyr::select(COD_DPTO, DPTO, MPIO, COD_MUNI, CULTIVO, PERIODO, AREA_SEMBRADA_HA, AREA_COSECHADA_HA, PRODUCCION, RDTOS)
yrs <- unique(tbl$PERIODO)
write.csv(tbl, '../_data/_tbl/production_tidy.csv', row.names = FALSE)

# Joining the shapefile and the table
lapply(yrs, myJoin)





















