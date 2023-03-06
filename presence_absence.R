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

Path = "~/INVASIBILITY_THRESHOLD/data/PresenceAbsence_MA_BG2.Rds"
df_pa <- readRDS(Path)
# Path = "~/INVASIBILITY_THRESHOLD/data/PresenceAbsence_MA_BG2.rds"
# saveRDS(df_pa, Path)

esp_can <- esp_get_munic_siane(moveCAN = TRUE)
# Check how to form NATCODE:
esp_can_filt <- esp_can[,c(1,2,3,4)]
esp_can_filt$geometry <- NULL
unique(esp_can_filt)

can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE))
# esp_can <- esp_can[-(which(is.na(esp_can$name))),]
df_pa$NATCODE <- as.numeric(df_pa$NATCODE)
df_join <- esp_can %>% left_join(df_pa)
df_join$pa <- ifelse((df_join$A_PRIM_DET_OFICIAL == 0 & df_join$A_PRIM_DET_CITSCI == 0),0,1)
df_join$pa[which(is.na(df_join$pa))] <- 0
# muni_spain = st_read("~/INVASIBILITY_THRESHOLD/data/Municipios_IGN.shp")
# muni_spain$NATCODE <- as.numeric(muni_spain$NATCODE)
# df_join <- muni_spain %>% left_join(df_pa)
# df_join$pa <- ifelse((df_join$A_PRIM_DET_OFICIAL == 0 & df_join$A_PRIM_DET_CITSCI == 0),0,1)
# df_join <- df_join[-(which(is.na(df_join$pa))),]

ggplot(df_join) +
  geom_sf(aes(fill = factor(pa)), size = 0.1) +
  geom_sf(data = can_box) + theme_bw()

