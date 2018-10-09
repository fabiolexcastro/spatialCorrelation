
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, velox, sf, tidyverse, stringr, gtools, foreign)
library(textclean)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
setwd('//mnt/workspace_cluster_9/Coffee_Cocoa2/_cocoaCol/_r')

# Functions to use
myCount <- function(yr, dpto){
  # yr <- yrs[1]; dpto <- 'AMAZONAS'
  sh  <- shp[shp@data$NOMBRE_DPT %in% dpto,]
  fls <- list.files(paste0('../_data/_tif/_frst/_geo/_dptos/', yr), full.names = TRUE, pattern = '.tif$')
  fle <- grep(dpto, fls, value = TRUE)
  lyr <- raster(fle)
  mps <- sh@data$NOM_MUNIC

  print('To make the lapply')

  lyrs <- lapply(1:length(mps), function(k){
  	print(mps[k])
  	mp <- sh[sh@data$NOM_MUNIC %in% mps[k],]
  	ct <- raster::crop(lyr, mp) %>% raster::mask(mp)
  	nm <- mp@data$NOM_MUNICI
  	pn <- rasterToPoints(ct) %>% as_data_frame()
  	p2 <- pn %>% 
  		setNames(c('x', 'y', 'value')) %>%
  		group_by(value) %>%
  		summarize(count = n()) %>% 
  		ungroup() %>% 
  		mutate(dpto = dpto,
  			   mpio = nm)
  	print('Done lapply into the function!')
  	return(p2)
  })

  tbls <- bind_rows(lyrs)
  tbls <- inner_join(tbls, lbl, by = c('value' = 'value'))
  tbls <- tbls %>% dplyr::select(-value) %>% spread(category, count)
  tbls <- tbls %>% setNames(c('Departamento', 'Municipio', 'Bosque', 'No_Bosque', 'Sin_Informacion'))
  tbls <- tbls %>% mutate(total_pix = Bosque + No_Bosque + Sin_Informacion,
  	Bosque_prc = Bosque / total_pix * 100,
  	No_Bosque_prc  = No_Bosque / total_pix * 100,
  	Sin_Informacion_prc = Sin_Informacion / total_pix * 100)
  write.csv(tbls, paste0('../_data/_tbl/_bosque/', yr, '/count_', gsub(' ', '', dpto), '.csv'), row.names = FALSE)
  print('Done..!')
}

# Load data
yrs <- list.files('../_data/_tif/_frst/_geo/_dptos')
shp <- shapefile('../_data/_shp/_base/Municipios_SIGOT_geo.shp')
dpt <- shp@data$NOMBRE_DPT %>% unique()
lbl <- data.frame(value = 1:3, category = c('Bosque', 'No bosque', 'Sin informacion'))

y <- 1
yr <- yrs[y]
d <- 1
dpto <- dpt[d]

# Applying the function
fnl <- lapply(1:length(yrs), function(y){
 print(yrs[y])
 lapply(1:length(dpt), function(d){
 	print(dpt[d])
 	myCount(yr = yrs[y], dpto = dpt[d])
 	print('Done global function DONE!...!')
 })
})



