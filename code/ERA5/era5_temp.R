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

# Download data from web ---------------------------------------------------
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form
# "45/-30/25/5" # North, West, South, East

rast_temp <- function(month_s){
  
  # Load data ----------------------------------------------------------------
  Path <- paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/2022/",month_s,"_temp_2022.grib")
  nc_raster <- rast(Path) 
  nc_raster
  coord_ref <- st_crs(nc_raster)
  # plot(nc_raster[[26]])
  
  # Load Spain map municipalities --------------------------------------------
  esp_can <- esp_get_munic_siane(moveCAN = FALSE)
  esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                       esp_can$cpro,
                                       esp_can$LAU_CODE))
  crs(nc_raster) <- crs(esp_can)
  
  # Shape Spain --------------------------------------------------------------
  # esp0 <- geodata::gadm(country = 'ESP', level = 0,
  #                       path = 'tmpr')
  Path <- paste0("~/INVASIBILITY_THRESHOLD/output/esp.rds")
  esp0 <- readRDS(Path)
  # plot(esp0)
  
  # Crop the raster for Spain ------------------------------------------------
  nc_raster <- terra::crop(nc_raster, esp0) %>%
    terra::mask(., esp0)
  
  return(nc_raster)
}

# Extract different layer --------------------------------------------------
# There is first layer temp day 1 second layer rainfall day and repeat
extract_hourly <- function(ind){ 
  start_time <- Sys.time()
  # plot(nc_raster[[1]])
  temp_1 <- subset(nc_raster,ind)
  # time(temp_1)
  
  # Load Spain map municipalities --------------------------------------------
  esp_can <- esp_get_munic_siane(moveCAN = FALSE)
  esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                       esp_can$cpro,
                                       esp_can$LAU_CODE))
  
  # Extract weather for municipalities ---------------------------------------
  temp_muni <- terra::extract(temp_1, esp_can)
  colnames(temp_muni) <- c("ID", "K")
  
  # Transfor to Celsius and grop by municipality ----------------------------
  temp_muni$C_temp <- temp_muni$K - 273.15
  temp_muni <- temp_muni %>% group_by(ID) %>%
    summarize(tmean = mean(C_temp))
  
  # Join temp df with shapefile esp -----------------------------------------
  esp_can$ID <- seq(1, nrow(esp_can),1)
  temp_esp <- esp_can %>% left_join(temp_muni)
  temp_esp$geometry <- NULL
  # ggplot(temp_esp) +
  #   geom_sf(aes(fill=tmean), colour = NA) +
  #   scale_fill_viridis_c(option = "magma") +
  #   theme_bw()
  end_time <- Sys.time()
  print(end_time - start_time)
  
  return(temp_esp)
}

# Aggregate hourly to daily -------------------------------------
agg_daily <- function(i){
  temp1 <- extract_hourly(i)
  temp2 <- extract_hourly(i+1)
  temp3 <- extract_hourly(i+2)
  temp4 <- extract_hourly(i+3)
  
  temp <- rbind(temp1, temp2, temp3, temp4)
  temp$NATCODE <- as.numeric(paste0("34",temp$codauto,
                                    temp$cpro,
                                    temp$LAU_CODE))
  temp <- temp %>% group_by(NATCODE) %>%
    summarise(tmean = mean(tmean),
              tmin = min(tmean),
              tmax = max(tmean))
  return(temp)
}

# Select month for extraction climate --------------------------------------
month_s <- "January"
nc_raster <- rast_temp(substr(month_s,1,3))
plot(nc_raster[[2]])

time_info <- time(nc_raster)

# Paralelize code --------------------------------------------------
num_cores = 12
climat_each_muni <- mclapply(seq(1,4*30,4), 
                             agg_daily, 
                             mc.cores = num_cores)
climat_each_muni <- setDT(do.call(cbind, climat_each_muni))

# Save the resultant file ------------------------------------------
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/2022/temp_", month_s, "_2022.Rds")
saveRDS(climat_each_muni,Path)