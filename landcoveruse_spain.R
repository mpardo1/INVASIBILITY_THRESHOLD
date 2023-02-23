rm(list = ls())
library("mapSpain")
library(tidyverse)
library(sf)
library(pollen)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gganimate)
library(rmapshaper)
library("ggpubr")
library(viridis)
library(stringr)
library(gdata)
##### Landcover use by municipality
Path <- "~/INVASIBILITY_THRESHOLD/data/CLC2018_ES.gpkg"
landcover <- st_read(Path)
class(landcover)
summary(landcover)
landcover <- landcover[which(landcover$CODE_18 == "111" | landcover$CODE_18 == "112"),]
muni_geo <- esp_get_munic_siane(moveCAN = FALSE)
landcover = landcover %>% st_transform(st_crs(muni_geo))
landcover <- landcover[-c(5155,8035,8095,8097,8106,8112),]
intersect <- st_intersection(landcover, muni_geo) 
area <- intersect %>% 
  mutate(area = st_area(.) %>% as.numeric())

Path <- "~/INVASIBILITY_THRESHOLD/output/landover.Rds"
saveRDS(area,Path)

# ###### Human population by municipality
# census <- mapSpain::pobmun19
