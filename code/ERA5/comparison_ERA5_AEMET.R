# Comparison from AEMET to ERA5
rm(list=ls())
library("mapSpain")
library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library("ggpubr")
library(viridis)
library(stringr)
library(gdata)
library("data.table")

# Spain map
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$R0_test <- runif(length(esp_can$codauto),0,5)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE))

# Read data AEMET
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/monthly_rainf_aemet_weather_year_2_18.Rds"
aemet_18 <- readRDS(Path)
aemet_18 <- st_as_sf(aemet_18[which(aemet_18$month == 5),])

aemet_plot_temp <- ggplot(aemet_18) +
  geom_sf(aes(fill = tmed), linewidth = 0.01) +
  scale_fill_viridis(name = "Temp") +
  geom_sf(data = can_box) + theme_bw()

aemet_plot_rain <- ggplot(aemet_18) +
  geom_sf(aes(fill = precmed), linewidth = 0.01) +
  scale_fill_viridis(name = "Rain") +
  geom_sf(data = can_box) + theme_bw()

# Read data ERA5
Path <- "~/INVASIBILITY_THRESHOLD/output/ERA5/temp/temp_out_daily_2018_05.Rds"
era5_18 <- readRDS(Path)
era5_18 <- as.data.frame(do.call(rbind,era5_18))
era5_18$month <- lubridate::month(era5_18$date)
era5_18 <- era5_18 %>% group_by(NATCODE) %>%
  summarize(tmean = mean(tmean))
era5_18 <- esp_can  %>% left_join(era5_18)

era5_plot_temp <- ggplot(era5_18) +
  geom_sf(aes(fill = tmean), linewidth = 0.01) +
  scale_fill_viridis(name = "Temp") +
  geom_sf(data = can_box) + theme_bw()

ggarrange(era5_plot + ggtitle("era5"),
          aemet_plot + ggtitle("aemet"))

Path <- "~/INVASIBILITY_THRESHOLD/output/ERA5/rainfall/rainfall_2018_05.Rds"
era5_18 <- readRDS(Path)
era5_18$precmed <- rowSums(era5_18[,c(1:(ncol(era5_18)-1))])
era5_18 <- era5_18[,c("NATCODE","precmed")]
era5_18 <- esp_can  %>% left_join(era5_18)

era5_plot_rain <- ggplot(era5_18) +
  geom_sf(aes(fill = precmed), linewidth = 0.01) +
  scale_fill_viridis(name = "Rain") +
  geom_sf(data = can_box) + theme_bw()

ggarrange(era5_plot_rain + ggtitle("era5"),
          aemet_plot_rain + ggtitle("aemet"),
          era5_plot_temp ,
          aemet_plot_temp )
