
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, gtools, stringr, velox, sf)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use
createCode <- function(code, lyr, shp, tbl){
  
  sink(code)
  
  cat('import arcpy', fill = T)
  cat('from arcpy import env', fill = T)
  cat('from arcpy.sa import *', fill = T)
  
  cat(paste0('inZoneData = ', '"', shp, '"'), fill = T)
  cat(paste0('zoneField = ', "'", 'NOM_MUNICI', "'"), fill = T)
  cat(paste0('inValueRaster = ', "'", lyr, "'"), fill = T)
  cat(paste0('outTable = ', "'", tbl, "'"), fill = T)
  
  cat('arcpy.CheckOutExtension("Spatial")', fill = T)
  cat('print "To process...!"', fill = T)
  
  cat('ZonalHistogram(inZoneData, zoneField, inValueRaster, outTable)', fill = T)
  cat('print "Done..."')#cat(paste0('outInt.save(', '"', out, unlist(strsplit(lyr, '.asc')), '.tif', '"', ')'), fill = T) 
  sink()
  
  shell(code)# system2(paste0('python ', code));# shell.exec(code)
  print('Done...')

}
myZonal <- function(yr, dpt){
  
  x <- grep(yr, lyrs, value = TRUE) %>% grep(dpt, ., value = TRUE)
  y <- grep(dpt, shps, value = TRUE)
  t <- paste0('//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_data/_dbf/', dpt, '_', yr, '_zonal.dbf')
  c <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_py/zonalHistogram_1.py'
  
  createCode(code = c,
             lyr = x,
             shp = y,
             tbl = t)
  
  print(paste0('Done... ', yr, ' ', dpt))
  
}

# Load data
shps <- list.files('//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_data/_shp/_base/_mpos', full.names = TRUE, pattern = '.shp$')
yers <- list.files('//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_data/_tif/_frst/_geo/_dptos/', full.names = FALSE)
lyrs <- list.files(paste0('//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_data/_tif/_frst/_geo/_dptos/', yers), full.names = TRUE, pattern = '.tif$')
dpts <- basename(shps) %>% gsub('lim_', '', .) %>% gsub('.shp', '', .)

# Applying the function
lapply(1:length(dpts), function(d){
  print(dpts[d])
  myZonal(yr = '2000', dpt = dpts[d])
})





