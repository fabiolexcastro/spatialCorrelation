

# Load libraries
library(tabulizer)
library(tidyverse)
library(foreign)
library(sf)

# Initial setup
g <- gc(reset = true)
rm(list = ls())
options(scipen = 999)

# Load data
fle <- '../_data/_pdf/MUNICIPIOS_ZOMAC_344_DEFINITIVO.pdf'
adm <- '../_data/_shp/_pdt/pdt.dbf' %>% read.dbf() %>% as_data_frame() %>% dplyr::select(NOMBRE_, NOM_MUN, ID_ESPA)
shp <- '../_data/_shp/_base/Municipios_SIGOT_geo.shp' %>% st_read()

# Extractin tables and tidying data
tbl <- extract_tables(fle)
tbl[[1]] <- tbl[[1]][,1:3]
tbl <- do.call(rbind, tbl)
tbl <- as_data_frame(tbl)
tbl <- tbl[-c(1,2,3),]
tbl <- tbl %>% setNames(c('codigo', 'dpto', 'mpio'))
tbl <- tbl %>% mutate(codigo = as.numeric(codigo))

# Join
t <- inner_join(tbl, adm, by = c('codigo' = 'ID_ESPA')) %>% dplyr::select(codigo, NOMBRE_, NOM_MUN) %>% mutate(zomac = 'Si')
x <- full_join(t, adm, by = c('codigo' = 'ID_ESPA'))
y <- x[is.na(x$NOMBRE_.x),]
y <- y %>% dplyr::select(codigo, NOMBRE_.y, NOM_MUN.y) %>% mutate(zomac = 'No')
y <- y %>% setNames(c('codigo', 'NOMBRE_', 'NOM_MUN', 'zomac'))
z <- rbind(t, y)
z %>% filter(NOM_MUN == 'CIÉNAGA DE ORO')

shp <- shp %>% mutate(ID_ESPACIA = as.numeric(ID_ESPACIA))
shp <- inner_join(shp, z, by = c('gid' = 'codigo'))

st_write(obj = shp, dsn = '../_data/_shp/_zomac', layer = 'mpios_zomac', driver = 'ESRI Shapefile', update = TRUE)







