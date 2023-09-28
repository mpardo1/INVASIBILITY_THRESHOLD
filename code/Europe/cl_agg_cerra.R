# aggregate the raster hourly data to monthly data for Europe.
rm(list = ls())
library(terra)
library(parallel)
library(tidyverse)
library(tictoc)
# load the data 
path <- "~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/cerra_2020_TEMP.grib"
temp_eu <- rast(path)
temp_eu_mon <- tapp(temp_eu,"months",mean)

# Extract the timestamps from the raster 
timestamps <- time(temp_eu)
# compute the number of layer per month
df_time <- data.frame(timestamps)
df_time$month <- lubridate::month(df_time$timestamps)
df_time <- df_time %>% group_by(month) %>% summarize(n=n())

# Extract the month and year from the timestamps
months <- lubridate::month(timestamps)

# Create an empty list to store monthly mean rasters
monthly_mean_rasters <- list()

# Loop through each unique month and calculate the monthly mean
# unique_months <- unique(months)
# agg_func <- function(month) {
#   print(paste0("month:", month))
#   if(month == 1){
#     subset_raster <- subset(temp_eu,1:df_time$n[month])
#   }else{
#     subset_raster <- subset(temp_eu,
#                             (df_time$n[month-1]+1):(df_time$n[month]+df_time$n[month-1]))
#   }
# 
#   monthly_mean_raster <- aggregate(subset_raster, fun = mean)
#   monthly_mean_rasters[[month]] <- monthly_mean_raster
#   return(monthly_mean_raster)
# }
# 
# 
# # parallelize
# cores = 12
# monthly_mean_rasters <- mclapply(1:12, mc.cores = cores,
#                          mc.preschedule = F,agg_func)
# 
# # Create a single raster stack from the monthly mean rasters
# monthly_mean_rast <- rast(monthly_mean_rasters)
# writeRaster(monthly_mean_rast,
#             filename = "~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/monthly_aggregated_raster.tif",
#             overwrite = TRUE)
# # 
# try with a loop ---------------------------------------
# for(month in c(1:12)){
#   tic()
#   print(paste0("month:", month))
#   if(month == 1){
#     subset_raster <- subset(temp_eu,1:df_time$n[month])
#   }else{
#     subset_raster <- subset(temp_eu,
#                             (df_time$n[month-1]+1):(df_time$n[month]+df_time$n[month-1]))
#   }
# 
#   monthly_mean_raster <- aggregate(subset_raster,fact = 3, fun = mean)
#   monthly_mean_raster <- tapp(subset_raster,"months",mean)
#   
#   toc()
#   monthly_mean_rasters[[month]] <- monthly_mean_raster
# }

# monthly_mean_rasters <- rast(monthly_mean_rasters)
# Save the raster stack to a new file
terra::writeRaster(temp_eu_mon,
                   "~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/3monthly_aggregated_raster.tif", overwrite = TRUE)
