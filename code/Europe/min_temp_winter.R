rm(list=ls())
# Package for tables
library("RColorBrewer")
library(tidyverse)
library(mapSpain)
library(data.table)
library(terra)
library(sf)
library("leaflet")
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")

# Load raster winter Europe
Path <- "~/INVASIBILITY_THRESHOLD/data/ERA5/Winter/"
list_files <- list.files(Path)

# Join all rasters
rast_win <- rast(paste0(Path, list_files[1]))
terra::plot(rast_win[[1]])
for(i in c(2:length(list_files))){
  rast_aux <- rast(paste0(Path, list_files[i]))
  rast_win <- c(rast_win, rast_aux)
}

# Extract time in the raster
unique(month(time(rast_win)))

# Compute the minimum of the year
rast_win <- tapp(rast_win, index = "years", fun = "min")

# Change to celsius degrees
values(rast_win) <- values(rast_win) - 273.5
terra::plot(rast_win)

# Plot with 1 where the temp is less than -10
rast_threshold <- rast_win
rast_threshold <- terra::ifel(rast_threshold < (-10),1,0)
terra::plot(rast_threshold)
