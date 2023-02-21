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

ggplot(landcover) + 
  geom_sf()

st_area(landcover)
st_write(landcover,"land.kml")


muni_geo <- st_read("~/Documentos/PHD/2023/INVASIBILITY/data/Municipios_IGN.shp")

inter <- st_intersects(landcover, muni_geo)
vor = station_points %>% st_geometry() %>% st_union() %>%
  st_voronoi(envelope = st_geometry(spain_perimeter)) %>%
  st_collection_extract(type = "POLYGON") %>% st_as_sf() %>%
  st_intersection(spain_perimeter) %>% st_join(station_points)

###### Human population by municipality
census <- mapSpain::pobmun19