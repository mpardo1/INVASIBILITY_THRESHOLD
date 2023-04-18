rm(list = ls())
library("ggplot2")
library("latex2exp")
library("tidyverse")
library("parallel")
library("deSolve")
library("sf")
library("mapSpain")

theme_set(ggplot2::theme_bw() +
            ggplot2::theme(axis.title = element_text(size  = 15, color = 'black'),
                           axis.text = element_text(size = 15, color = 'black'),
                           legend.text = element_text(size = 15, color = 'black'),
                           legend.title = element_text(size = 15, color = 'black')))


# Weather by meteo stations:
Path <- "~/Documentos/PHD/2022/INVASIBILY/OUTPUT/aemet_weather_daily_deep_history.Rds"
df_out <- readRDS(Path)

# Stations relation with municipalities:
Path <- "~/Documentos/PHD/2022/INVASIBILY/OUTPUT/prediction_points_lc_sf_df_all_spain_1000.Rds"
df_out1 <- readRDS(Path)

# Read data meteo stations:
esta_meteo <- st_read(
  "/home/marta/Documentos/PHD/2022/INVASIBILY/Data/Estaciones_Meteo/Estaciones_Completas.shp")
st_geometry_type(esta_meteo)
st_bbox(esta_meteo)

esp_can <- esp_get_munic_siane(moveCAN = FALSE)

ggplot(esp_can) +
  geom_sf( size = 0.1) +
  geom_sf(data = esta_meteo, size = 0.5, color = "red") + theme_bw()


setdiff(sort(unique(df_out$indicativo)),sort(unique(esta_meteo$INDICATIVO)))
