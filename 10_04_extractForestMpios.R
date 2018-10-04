
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use
createCode <- function(code, lyr, msk, out, nme){
  sink(code)
  cat('import arcpy', fill = T)
  cat('from arcpy import env', fill = T)
  cat('from arcpy.sa import *', fill = T)
  cat('arcpy.CheckOutExtension("Spatial")', fill = T)
  cat(paste0('raster = ', '"', lyr, '"'), fill = T)
  cat('arcpy.env.snapRaster = raster', fill = T)
  cat(paste0('mask = ', "'", msk, "'"), fill = T)
  cat(paste0('corte = ', "'", out, "'", " + '/' + ", "'", nme, "'"), fill = T)
  cat(paste0('final = ExtractByMask(raster, mask)'), fill = T)
  cat('final.save(corte)', fill = T)
  cat('print "Done..."')#cat(paste0('outInt.save(', '"', out, unlist(strsplit(lyr, '.asc')), '.tif', '"', ')'), fill = T) 
  sink()
  
  shell(code)# system2(paste0('python ', code));# shell.exec(code)
  print('Done...')
}

# Paths
fles <- list.files('../_data/_tif/_frst/_geo/_col', full.names = TRUE, pattern = '.tif$')
code <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_py/myExtract2.py'
dirBase <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_data/_tif/_frst/_geo/_col'
mask <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_data/_shp/_base/Municipios_SIGOT_geo.shp'
out <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_data/_tif/_frst/_geo/_dptos'

# Loading in R
nmes <- basename(fles)
lyrs <- paste(dirBase, nmes, sep = '/')
mask.shp <- shapefile(mask)

# Extracting the dptos into different shapefiles
mask.shp@data$NOMBRE_DPT <- iconv(mask.shp@data$NOMBRE_DPT, 'UTF-8', 'latin1')
dptos <- mask.shp@data$NOMBRE_DPT %>% unique() %>% sort()

lapply(1:length(dptos), function(k){
  x <- mask.shp[mask.shp@data$NOMBRE_DPT %in% dptos[k],]
  x <- aggregate(x, 'COD_DEPTO')
  writeOGR(obj = x, dsn = '../_data/_shp/_base/_dptos', layer = paste0('lim_', gsub(' ', '_', dptos[k])), driver = 'ESRI Shapefile')
  print(paste0('Done... ', dptos[k]))  
})

# Years
yrs <- parse_number(nmes)
nmes <- c('frst_2000.tif', 'frst_2016.tif', 'frst_2017')
yrs <- c(2000, 2005, 2015)
shps <- list.files('//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_data/_shp/_base/_dptos', full.names = TRUE, pattern = '.shp$')
out <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_data/_tif/_frst/_geo/_dptos'

# Creating the directories for each year
lapply(1:length(yrs), function(k) dir.create(path = paste0(out, '/', gsub(' ', '_', yrs[k])))) 

# Applying the function for extract by mask since Python
lapply(1:length(yrs), function(y){
  print(yrs[y])
  lapply(1:length(dptos), function(k){
    print(dptos[k])
    createCode(code = code,
               lyr = lyrs[y],
               msk = shps[k],
               out = paste(out, yrs[y], sep = '/'),
               nme = paste0(gsub('.tif', '', nmes[y]), '_', dptos[k], '.tif'))
  }) 
  print('Done..!')
})


createCode(code = code,
           lyr = '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_data/Reporte_Bosque_Natural/2005/Productos_TIFF/TS666TGG9_BEA_geo.tif',
           msk = '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_data/_shp/_base/valle.shp',
           out = '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_tmp',
           nme = 'frst_2005_vlle.tif')


# End