rm(list=ls())
library(mapSpain)
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library("data.table")
library(tidyr)
library(parallel)

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()

Path <- "/home/marta/INVASIBILITY_THRESHOLD/output/R0/R0/R0_ERA5_hourly_to_daily_2020.Rds"
df_hourly <- readRDS(Path)
