
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, velox, sf, reshape)
library(tiling)
library(envirem)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
lbl <- data.frame(value = c(0, 1, 2, 3, 8), category = c('No_idoneo', 'Baja', 'Media', 'Alta', 'Exclusion_Legal'))

# Load data
lyr <- raster('../_data/Reporte_Bosque_Natural/2005/Productos TIFF/TS666TGG9_BEA_geo.tif')
adm <- shapefile('../_data/_shp/_base/Municipios_SIGOT_geo.shp')
adm@data$gid <- as.numeric(adm@data$gid)
adm@data$myID <- 1:nrow(adm@data)


# Example Valle del Cauca
vll <- adm[adm@data$NOMBRE_DPT %in% 'VALLE DEL CAUCA',]
vll <- aggregate(vll, 'COD_DEPTO')

tst <- raster::crop(lyr, vll) %>% raster::mask(vll)
ts2 <- tst * 1 #vll.lyr <- rasterize(vll, ts2, field = 'gid')# vll.znl <- raster::zonal(ts2, vll.lyr, fun = 'mean', na.rm = TRUE)

# Extract points
vll.pnt <- rasterToPoints(ts2)
vll.vlx <- velox(ts2)
vll.spn <- SpatialPointsDataFrame(coords = vll.pnt[,1:2], data = data.frame(value = vll.pnt[,3]))

tmp <- vll.vlx$extract(sp = adm)

vll.adm <- raster::extract(vll, vll.spn) %>% as_data_frame()

vll.ext <- vll.vlx$extract_points(sp = vll.spn)
vll.spn@data$value <- vll.ext[,1]

vll.spn




# Processing
res <- res(lyr)[1] * 111.11
pnt <- lyr %>% rasterToPoints() %>% as_data_frame()
cnt <- raster::extract(adm, pnt[,1:2]) %>% as_data_frame()
cnt <- cnt %>% dplyr::select(ID_ESPACIA, myID, NOMBRE_DPT, NOM_MUNICI) %>% cbind(., pnt) %>% as_data_frame()
cnt <- cnt[complete.cases(cnt),]
cnt <- cnt %>% mutate(NOMBRE_DPT = iconv(NOMBRE_DPT, 'UTF-8', 'latin1'), NOM_MUNICI = iconv(NOM_MUNICI, 'UTF-8', 'latin1'))
cnt2 <- cnt

sum <- cnt %>%
  group_by(ID_ESPACIA, NOMBRE_DPT, NOM_MUNICI, suitCocoa_geo) %>%
  dplyr::summarize(count = n()) %>%
  ungroup() %>% 
  inner_join(., lbl, by = c('suitCocoa_geo' = 'value'))

sum <- sum %>% dplyr::select(-suitCocoa_geo) %>% spread(category, count)
sum[is.na(sum)] <- 0

# Adding the area (has), percentage
smm <- sum %>% 
  mutate(total_count = Alta + Baja + Exclusion_Legal + Media + No_idoneo,
         Alta_prc = Alta/total_count * 100,
         Baja_prc = Baja/total_count * 100,
         Exclusion_Legal_prc = Exclusion_Legal/total_count * 100,
         Media_prc = Media/total_count * 100,
         No_idoneo_prc = No_idoneo/total_count * 100) # tst %>% dplyr::select(Alta:total_count) %>% mutate_all(funs(. / total_count * 100))

# Cleaning administrative shapefile
adm <- st_as_sf(adm)
adm <- adm %>% dplyr::select(OBJECTID, ID_ESPACIA, gid, NOMBRE_DPT, NOM_MUNICI) %>% mutate(NOM_MUNICI = iconv(NOM_MUNICI, 'UTF-8', 'latin1'), NOMBRE_DPT = iconv(NOMBRE_DPT, 'UTF-8', 'latin1'))

fnl <- inner_join(adm, smm, by = c('ID_ESPACIA' = 'ID_ESPACIA'))
colnames(fnl) <- c('OBJECTID', 'ID_ESPACIA', 'gid', 'NOMBRE_DPT', 'NOM_MUNICI', 'NOMBRE_DPT2', 'NOM_MUNICI2', 
                   'High', 'Low', 'Leg', 'Med', 'NoSuit', 'Tot', 'High_prc', 'Low_prc', 'Leg_prc', 'Med_prc', 'NoSuit_prc', 'geometry')

st_write(obj = fnl, dsn = '../_data/_shp/_suitability', layer = 'cocoa_count', driver = 'ESRI Shapefile', update = TRUE)


