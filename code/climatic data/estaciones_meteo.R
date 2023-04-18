library("sf")
library("mapSpain")
library(tidyverse)

# Read data meteo stations:
esta_meteo <- st_read(
  "/home/marta/Documentos/PHD/2022/INVASIBILY/Data/Estaciones_Meteo/Estaciones_Completas.shp")
st_geometry_type(esta_meteo)
st_bbox(esta_meteo)

can_stations =
esp_can <- esp_get_munic_siane(moveCAN = FALSE)

ggplot(esp_can) +
  geom_sf( size = 0.1) +
  geom_sf(data = esta_meteo, size = 0.5, color = "red") + theme_bw()
