rm(list=ls())
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
library("data.table")
library("zoo")

Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/rainfall/monthly_rainf_aemet_weather_year_2_21.Rds"
R0_rain <- readRDS(Path)
can_box <- esp_get_can_box()
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
R0_rain$geometry <- NULL
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE))
R0_rain <- esp_can %>% left_join(R0_rain)
# Create plots:
ggplot(R0_rain) +
  geom_sf(aes(fill = R0_tmed), size = 0.01) + 
  scale_fill_viridis(name = "R0(T)", limits = c(0,max(R0_rain$R0_tmed))) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) + 
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(month)

R0_rain <- setDT(R0_rain)
data_sum <- R0_rain[ , .(R0_med = mean(R0_tmed),
                                        R0_min = min(R0_tmin),
                                        R0_max = max(R0_tmax)),
                                    by = list(month,name)]     # Aggregate data

data_sum$bool <- ifelse(data_sum$R0_med >= 1, 1,0)
plot_df_grouped <- data_sum[ , .(sum_bool = sum(bool)), 
                             by = list(name)]     # Aggregate data

plot_df <-  esp_can %>%  left_join(plot_df_grouped)

plot_sum_albo <- ggplot(plot_df) +
  geom_sf(aes(fill = sum_bool), lwd = 0) + 
  scale_fill_viridis(name = "Nº of months with R0>1",
                     limits = c(0, 12), option="turbo") +
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("Aedes Albopictus 2021") + 
  theme_bw() 
plot_sum_albo
