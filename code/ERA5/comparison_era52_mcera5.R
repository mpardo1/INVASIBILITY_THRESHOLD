# Code to extract climatic data from Copernicus:
rm(list=ls())
library(mapSpain)
library(terra)
library(sf)
library(raster)
library(tidyverse)
library(data.table)
library(parallel)
# library(geodata)

month_s  = "April"
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/2022/temp_", month_s, "_2022.Rds")
df_aux <- readRDS(Path)

esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_day <- df_aux[which(df_aux$date == as.Date("2022-04-06")),]
df_day <- esp_can %>% left_join(df_day)
plot_era5 <- ggplot(df_day) + 
  geom_sf(aes(fill = tmean ), color = NA) +
  scale_fill_viridis_c(option = "magma", limits = c(-5,25)) + 
  theme_bw()

# Read weather mcera5 -----------------------------------------
year = 2022
Path <- paste0("/home/marta/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",
               year,".Rds")
df_group <- setDT(readRDS(Path))
df_day <- df_group[which(df_group$date == as.Date("2022-04-06")),]
df_day <- esp_can %>% left_join(df_day)
plot_mcera5 <- ggplot(df_day) + 
  geom_sf(aes(fill = tmean ), color = NA) +
  scale_fill_viridis_c(option = "magma", limits = c(-5,25)) + 
  theme_bw()

# Copernicus data base 

# Read data temp -------------------------------------------------
df_20 <- data.frame()
for(i in c(1:12)){
  mon <- ifelse(i<10, paste0("0",i),as.character(i))
  Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/temp_out_daily_2020_",mon,".Rds")
  df_aux <- readRDS(Path)
  df_aux <- setDT(do.call(rbind,df_aux))
  df_20 <- rbind(df_20, df_aux)
}

# Test weather data -------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_day <- df_20[which(df_20$date == as.Date("2020-04-05")),]
df_day <- esp_can %>% left_join(df_day)
df_day <- ggplot(df_day) + 
  geom_sf(aes(fill = tmean ), color = NA) +
  scale_fill_viridis_c(option = "magma",limits = c(-5,25)) + 
  theme_bw()

# Join all the plots
library(ggpubr)
ggarrange(plot_era5 + ggtitle("Tmean ERA5 2022"), 
          plot_mcera5 + ggtitle("Tmean mcera5 package R"),
          df_day + ggtitle("Tmean ERA5 2020"), common.legend = TRUE)
