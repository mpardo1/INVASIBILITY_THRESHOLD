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

Path = "~/INVASIBILITY_THRESHOLD/data/PresenceAbsence_MA_BG2.csv"
df_pa <- read.table(file = Path, 
                    sep = ",", header=TRUE, fill = TRUE)
Path = "~/INVASIBILITY_THRESHOLD/data/PresenceAbsence_MA_BG2.rds"
saveRDS(df_pa, Path)

esp_can <- esp_get_munic_siane(moveCAN = TRUE)
# Check how to form NATCODE:
esp_can_filt <- esp_can[,c(1,2,3,4)]
esp_can_filt$geometry <- NULL
unique(esp_can_filt)

can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE))
df_pa$NATCODE <- as.numeric(df_pa$NATCODE)
df_join <- esp_can %>% left_join(df_pa)
df_join$pa <- ifelse((df_join$A_PRIM_DET_OFICIAL == 0 & df_join$A_PRIM_DET_CITSCI == 0),0,1)

ggplot(df_join) +
  geom_sf(aes(group = pa), size = 0.1) +
  geom_sf(data = can_box) + theme_bw()
