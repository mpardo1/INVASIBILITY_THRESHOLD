rm(list=ls())
library("mapSpain")
library(tidyverse)
library(sf)
library(dplyr)
library(lubridate)
library(stringr)
library(gdata)

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$R0_test <- runif(length(esp_can$codauto),0,5)
can_box <- esp_get_can_box()
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/"

group_monthly <- function(file_name){
  weather <- readRDS(paste0(Path,file_name))
  weather_df <- as.data.frame(do.call(rbind, weather))
  colnames(weather_df) <- c("muni_name", "ccaa_name", "prov_name",
                            "date", "tmin", "tmed", "tmax", "precmed", "n")
  
  colnames(esp_can) <- c(colnames(esp_can)[1:5], "muni_name",
                         colnames(esp_can)[7:length(colnames(esp_can))])
  # Merge the municipalities shapefile with the weather data:
  weather_municip_R01 <-  esp_can %>%  left_join(weather_df)
  weather_municip_R01$month <- lubridate::month(weather_municip_R01$date)
  weather_municip_R01$year <- lubridate::year(weather_municip_R01$date)
  
  weather_municip_R01_monthly <- weather_municip_R01 %>% 
    group_by(month,muni_name,ccaa_name, prov_name, year) %>% 
    summarise(t_med = mean(tmed), t_min = min(tmin),
              t_max = mean(tmax), precmed = mean(precmed))
  
    return(weather_municip_R01_monthly)
}

# Read the weather data for a specific month and year for all municipalities
list_files <- list.files(Path)
df_list <- lapply(list_file_filt, group_monthly)

write_rds(df_list, paste0("~/INVASIBILITY_THRESHOLD/output/weather/monthly/aemet_weather_year_monthly_yearlist.Rds"))
