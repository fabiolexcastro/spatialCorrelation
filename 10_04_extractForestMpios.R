
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
noMatch <- function(pos){
  # pos <- 1
  x <- grep(yrs[pos], fls, value = TRUE) %>% str_sub(., 11, nchar(.)) %>% gsub('.tif', '', .)  
  y <- setdiff(dptos, x)
  print('Done!')
  return(y)
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
nmes <- basename(fles)
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

# Reviewing the results
fls <- list.files('../_data/_tif/_frst/_geo/_dptos/', recursive = TRUE, pattern = '.tif$')
fls <- basename(fls)
parse_number(fls) %>% unique() # Years Ok...!

miss <- lapply(1:length(yrs), noMatch) %>% unique() %>% unlist()
miss <- iconv(miss, to = 'ASCII//TRANSLIT')

# Missing raster (this happened through the accented characters)
shps2 <- list.files('//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_data/_shp/_base/_dptos', full.names = TRUE, pattern = '.shp$')
shps2 <- grep(paste0(miss, collapse = '|'), shps2, value = TRUE)
dptos2 <- basename(shps2) %>% gsub('lim_', '', .) %>% gsub('.shp', '', .)

lapply(1:length(yrs), function(y){
  print(yrs[y])
  lapply(1:length(dptos2), function(k){
    print(dptos2[k])
    createCode(code = code,
               lyr = lyrs[y],
               msk = shps2[k],
               out = paste(out, yrs[y], sep = '/'),
               nme = paste0(gsub('.tif', '', nmes[y]), '_', dptos2[k], '.tif'))
  }) 
  print('Done..!')
})


