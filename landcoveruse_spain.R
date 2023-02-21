rm(list = ls())
library(sf)
library(ggplot2)
library(mapSpain)
##### Landcover use by municipality
Path <- "~/INVASIBILITY_THRESHOLD/data/CLC2018_ES.gpkg"
landcover <- st_read(Path)
class(landcover)
summary(landcover)
landcover <- landcover[which(landcover$CODE_18 == "111" | landcover$CODE_18 == "112"),]
muni_geo <- esp_get_munic_siane(moveCAN = FALSE)
st_crs(landcover) = 4258
st_crs(muni_geo) = 4258

intersect = st_intersection(muni_geo,landcover) 

###### Human population by municipality
census <- mapSpain::pobmun19
