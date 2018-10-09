
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse)

# Initial setup 
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function to use
myCount <- function(lyr){
  lyr <- lyr
  yrs <- names(lyr) %>% parse_number()
  
  tbls <- lapply(1:length(dpts), function(k){
    
    print(dpts[k])
    
    adm <- shp[shp@data$NOMBRE_DPT %in% dpts[k],]  
    lyr.cut <- raster::crop(lyr, adm) %>% raster::mask(adm)
    pnt <- rasterToPoints(lyr.cut) %>% as_data_frame()
    pnt.adm <- raster::extract(adm, pnt[,1:2])  
    pnt.adm <- as_data_frame(pnt.adm) %>% dplyr::select(NOMBRE_DPT, NOM_MUNICI)
    pnt.adm <- pnt.adm %>% mutate(value = pnt %>% pull(3))
    
    dpto <- unique(pnt.adm$NOMBRE_DPT)
    mpio <- unique(pnt.adm$NOM_MUNICI)  
    
    pnt2 <- pnt.adm %>%
      setNames(c('dpto', 'mpio', 'value')) %>%
      group_by(dpto, mpio, value) %>%
      summarize(count = n()) %>% 
      ungroup() %>% 
      mutate(dpto = dpto,
             mpio = mpio,
             year = yrs)
    
  })
  
  tbl <- bind_rows(tbls)
  write.csv(tbl, paste0('../_data/_tbl/_bosque/', yrs, '/count_forest_', yrs, '.csv'), row.names = FALSE)
  print('Done...!')
  
}

# Load data
fls <- list.files('../_data/_tif/_frst/_geo/_col/_rsmple', full.names = TRUE, pattern = '.tif$')
lyr <- raster(fls[6])
shp <- shapefile('../_data/_shp/_base/Municipios_SIGOT_geo.shp')
dpts <- shp@data$NOMBRE_DPT %>% unique() %>% sort()

# Applying the function
lapply(1:length(fls), function(k) myCount(lyr = raster(fls[k])))

# End




