
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, stringr, rgeos, velox, sf)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
fls <- list.files('../_data/Reporte_Bosque_Natural', recursive = T, pattern = '.tif$', full.names = TRUE)
shp <- shapefile('../_data/_shp/_base/Municipios_SIGOT_geo.shp')

shp@data$ID_ESPACIA <- as.numeric(shp@data$ID_ESPACIA)


myCalcForest <- function(fle, yr){
  fle <- fls[2]
  lyr <- raster(fle)
  lyr <- lyr * 1
  writeRaster(tst, paste0('../_data/Reporte_Bosque_Natural/2000/Productos TIFF/TS666TAOT_BEA_clean.tif'))
  
  pnt <- rasterToPoints(lyr)
  pn2 <- as_data_frame(pnt)
  pnt.shp <- SpatialPoints(coords = pnt[,1:2])
  
  adm.lyr <- rasterize(shp, lyr, field = 'ID_ESPACIA')
  vlx <- velox(lyr)
  tbl <- vlx$extract_points(sp = pnt[])
  
  
  class(vls)
  
  shp
  
  
  
  head(pnt)
  nrow(pnt)
  
}


