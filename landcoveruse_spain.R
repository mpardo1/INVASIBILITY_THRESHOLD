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
muni_geo = muni_geo %>% st_transform(st_crs(landcover))

# plot(muni_geo$geometry)
# cell_res <- 1000
# cell_mask <- 0.025
# SPAIN_CRS = 25830
# ua <- read_rds("data/proc/spain_ua_multipolygon.Rds")
# st_crs(ua) <- 25830
# spain_perimeter = st_read("~/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_peninbal_etrs89.shp") %>%
#   bind_rows(st_read("~/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_canarias_wgs84.shp")) %>%
#   st_transform(SPAIN_CRS) %>% summarize()
# 
# landcover <- landcover[1:3,]
# muni_test <- muni_geo[1,]
# this_perimeter_25830 <- spain_perimeter %>% st_transform(st_crs(ua)) %>% st_union()
# 
# these_points = st_make_grid(st_bbox(this_perimeter_25830)+100000*c(-1,-1,1,1),
#                             cellsize = c(cell_res,cell_res), what = "polygons",
#                             square = TRUE) %>% st_sf %>%
#   st_join(muni_geo %>%
#             dplyr::select(name, ine.ccaa.name, ine.prov.name),
#           join = st_intersects, left=FALSE) %>%
#   st_cast("POINT") %>% st_as_sf() %>%
#   st_join(landcover %>% dplyr::select(INDICATIVO) %>%
#             rename(indicativo = INDICATIVO), join = st_intersects, left=FALSE)

inter <- st_join(muni_geo,landcover, join = st_within)

library(tidyverse)

# example data from raster package
soil <- muni_test
# field polygons
field <- landcover %>% 
  st_transform(st_crs(soil))

# intersect - note that sf is intelligent with attribute data!
pi <- st_intersection(soil, field)
plot(soil$geometry, axes = TRUE)
plot(field$geoms, add = TRUE)
plot(pi$geometry, add = TRUE, col = 'red')

# add in areas in m2
attArea <- pi %>% 
  mutate(area = st_area(.) %>% as.numeric())

# for each field, get area per soil type
attArea %>% 
  as_tibble() %>% 
  group_by(field, soil) %>% 
  summarize(area = sum(area))
###### Human population by municipality
census <- mapSpain::pobmun19
