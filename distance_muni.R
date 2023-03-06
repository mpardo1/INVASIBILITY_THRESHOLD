rm(list=ls())
library("mapSpain")
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(gganimate)
library(viridis)
library("data.table")

esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
# Check how to form NATCODE:
esp_can_filt <- esp_can[,c(1,2,3,4)]
esp_can_filt$geometry <- NULL
unique(esp_can_filt)
nr <- nrow(esp_can)
esp_can$dist <- 0
for(i in c(1:nr)){
  esp_can$dist[i] <- st_distance(esp_can[which(esp_can$name == "Sant Cugat del Vallès"),],esp_can[i,])
}

ggplot(esp_can) +
  geom_sf(aes(fill = dist), size = 0.1) +
  scale_fill_viridis(name = "distance") +
  geom_sf(data = can_box) + theme_bw()
