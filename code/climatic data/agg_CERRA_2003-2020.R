# Code to read raster data CERRA era5
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

# Load temp files to aggregate monthly
Path_dir <- "U:/Colonization/data/CERRA/temp/"
list_files <- list.files(Path_dir)
print(list_files)

# Load raster
for(i in c(1:3)){#length(list_files))){
  clim_df <- rast(paste0(Path_dir,list_files[[i]])) # Load raster
  print(names(clim_df)[1])
  clim_df_m <- tapp(clim_df,"months", mean) # Aggregate raster by months
  writeRaster(clim_df_m, paste0("U:/Colonization/data/CERRA/temp/tmean_monthly_",unique(year(time(clim_df))),".tif"))
}

# Load rain files to process
Path_dir <- "U:/Colonization/data/CERRA/rain/"
list_files <- list.files(Path_dir)
print(list_files)

# Load raster
for(i in c(1:1)){#length(list_files))){
  clim_df <- rast(paste0(Path_dir,list_files[[i]])) # Load raster
  print(names(clim_df)[1])
  clim_df_m <- tapp(clim_df,"months", mean) # Aggregate raster by months
  writeRaster(clim_df_m, paste0("U:/Colonization/data/CERRA/rain/rain_monthly_",unique(year(time(clim_df)))[1],".tif"))
}

# Transform rainfall into daily rainfall with x  kg/m2s = x*86400 mm day
# But since i am downloading the 24 accumulated I think it is ok as it is.
# Checked

# Join data sets of temperature of all years and average monthly
Path <- "U:/Colonization/data/CERRA/temp/tmean_monthly_2003.tif"
clim_df_m <- rast(Path)
for(i in c(2004:2020)){
  print(paste0("i:",i))
  # Load raster
  Path <- paste0("U:/Colonization/data/CERRA/temp/tmean_monthly_",i,".tif")
  clim_df_m2 <- rast(Path)
  # Join two rasters and aggregate again
  add(clim_df_m) <- clim_df_m2
  clim_df_m <- tapp(clim_df_m,"months", mean)
}

# Save raster
writeRaster(clim_df_m, "U:/Colonization/data/CERRA/temp/tmean_monthly_2003-2020.tif")

# Join data sets of rainfall of all years and average monthly
Path <- "U:/Colonization/data/CERRA/rain/rain_monthly_2003.tif"
clim_df_m <- rast(Path)
for(i in c(2004:2020)){
  print(paste0("i:",i))
  # Load raster
  Path <- paste0("U:/Colonization/data/CERRA/rain/rain_monthly_",i,".tif")
  clim_df_m2 <- rast(Path)
  # Join two rasters and aggregate again
  add(clim_df_m) <- clim_df_m2
  clim_df_m <- tapp(clim_df_m,"months", mean)
}

# Save raster
writeRaster(clim_df_m, "U:/Colonization/data/CERRA/rain/rain_monthly_2003-2020.tif")

