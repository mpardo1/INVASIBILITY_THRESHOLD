# Code to extract climatic data from Copernicus:
# it looks very weird so i am not gonna use it.
rm(list=ls())
library(mapSpain)
library(terra)
library(sf)
library(raster)
library(tidyverse)
library(data.table)
library(parallel)
# library(geodata)

# Read grib file ---------------------------------------------------
Path <- "~/INVASIBILITY_THRESHOLD/data/ERA5/CERRA/2020_jan_jun_rain.nc"
Path <- "~/INVASIBILITY_THRESHOLD/data/ERA5/CERRA/2020_jun_dec_rain.nc"
nc_raster <- rast(Path)

# Save coords raster 
coord_ref <- st_crs(nc_raster)
plot(nc_raster[[1]])

# Sf with spain municipalities -------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

# Crop Spain data --------------------------------------------------
esp <- st_transform(esp_can,coord_ref)
rast_spain <- nc_raster  %>%
  terra::crop(esp)

plot(rast_spain[[5]])

# Extract values for each muni -------------------------------------
temp_muni <- terra::extract(rast_spain, esp)
colnames(temp_muni) <- c("ID",as.character(time(nc_raster)))
esp_sf <- st_as_sf(esp) %>%
  mutate(ID := seq_len(nrow(.))) %>%
  left_join(., temp_muni, by = "ID")

# Add natcode to df ------------------------------------------------
esp_sf$NATCODE <- as.numeric(paste0("34",
                                    esp_sf$codauto,
                                    esp_sf$cpro,
                                    esp_sf$LAU_CODE))

# Select columns with usefull info ---------------------------------
esp_s <- esp_sf[,c(8,10:ncol(esp_sf))]
esp_s$geometry <- NULL
esp_s <- setDT(reshape2::melt(esp_s,id.vars = "NATCODE"))
colnames(esp_s) <- c("NATCODE","date","prec")

# Aggregate if a NATCODE has more than one raster cell -------------
esp_s <- esp_s[,.(prec=sum(prec)), by = list(NATCODE,date)]

# esp_s1 is runnin with the file from jun to dec -------------------
esp_rain_2020 <- rbind(esp_s1,esp_s)

# Save file -------------------------------------------------------
Path <- "~/INVASIBILITY_THRESHOLD/data/ERA5/CERRA/2020_rain.Rds"
saveRDS(esp_rain_2020,Path)

# Plot sum prec to check ------------------------------------------
esp_R <- esp_s[,.(prec=sum(prec)), by = list(NATCODE)]
esp_R <- esp_R %>%
  left_join(esp_can)

ggplot(st_as_sf(esp_R)) +
  geom_sf(aes(fill = prec), color = NA) +
  scale_fill_viridis_c()

# Create df for compare in plot -----------------------------------
esp_s <- st_as_sf(esp_s[which(as.character(esp_s$date) ==
                                as.character(time(rast_spain[[2]]))),] %>%
                    left_join(esp_can))
plot_sf <- ggplot(esp_s) +
  geom_sf(aes(fill=temp), colour = NA) +
  scale_fill_viridis_c()

plot(rast_spain[[2]])
plot_sf

