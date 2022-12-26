library("mapSpain")
library(tidyverse)
library(sf)

# Read temperature John File.
Path <- "/home/marta/Documentos/PHD/2022/INVASIBILY/OUTPUT/aemet_weather_daily_deep_history_sf.Rds"
df <- readRDS(Path)

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$R0 <- runif(length(esp_can$codauto),0,5)
can_box <- esp_get_can_box()

ggplot(esp_can) +
  geom_sf(aes(fill = R0), size = 0.1) +
  geom_sf(data = can_box) + theme_bw()

# Path <- "/home/marta/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_peninbal_etrs89.shp"
# df_muni <- read_sf(Path)
Path <- "/home/marta/Documentos/PHD/2022/INVASIBILY/Municipios_Data/Municipios_IGN.shp"
df_muni <- read_sf(Path)
