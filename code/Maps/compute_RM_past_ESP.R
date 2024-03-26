# Code to compute R_M for aggregated raster for 2003-2020
# Load pkgs and remove data -----------------------------------------------------
rm(list = ls())
library("data.table")
library("ggplot2")
library("terra")
library("sf")
library("mapSpain")
library("tidyverse")
# library("geodata")
library(parallel)

sft_path <- "~" # Ubuntu or Mac
sft_path <- ".."# Windows
source("G:/mpardo/INVASIBILITY_THRESHOLD/code/funcR0.R")

# Load aggregated
Path <-  "U:/Colonization/data/CERRA/temp/tmean_monthly_2003-2020.tif"
temp_df <- rast(Path)
Path <-  "U:/Colonization/data/CERRA/rain/rain_monthly_2003-2020.tif"
rain_df <- rast(Path)

# plot
plot(rain_df[[1]])
plot(temp_df[[8]])

# Load Spanish municipality maps
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

# Project and crop
temp_df <- terra::project(temp_df,esp_can)
temp_df <- terra::crop(temp_df, esp_can) %>%
  terra::mask(., esp_can)
rain_df <- terra::project(rain_df,esp_can)
rain_df <- terra::crop(rain_df, esp_can) %>%
  terra::mask(., esp_can)

# Plot
plot(rain_df[[6]])

# Intersect sf with raster
temp_ext <- terra::extract(temp_df,esp_can,ID=TRUE)
esp_can_id <- 