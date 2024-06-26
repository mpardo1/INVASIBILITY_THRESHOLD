## Proces the nc files from mcera5
## It has to be run in the cluster with 10 cores

rm(list=ls())
library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)

# Function to process the weather from the nc files
extract_weather <- function(ind){
  print(paste0("ind:",ind))
  # Specify desired single point (within the bounds of your .nc file) 
  x <- esp_can$centroid[ind][[1]][1]
  y <- esp_can$centroid[ind][[1]][2]
  
  # Gather all hourly variables, with spatial and temporal dimensions
  # matching the extent, or a subset, of data in one downloaded file 
  st_time <- lubridate::ymd(paste0(year,":01:01"))
  en_time <- lubridate::ymd(paste0(year,":12:31"))
  point_out <- extract_clim(nc = my_nc, long = x,
                            lat = y, start_time = st_time,
                            end_time = en_time) 
  
  point_out <- point_out[,c("obs_time", "temperature")]
  point_out$NATCODE <- esp_can$NATCODE[ind]
  point_out$pop <- esp_can$pob19[ind]
  point_out$area <- esp_can$area[ind]
  # You can then inspect the data frame
  # Gather daily precipitation 
  point_out_precip <- extract_precip(nc =  my_nc, long = x, 
                                     lat = y, start_time = st_time, 
                                     end_time = en_time, convert_daily = FALSE)
  
  point_out$prec1 <- point_out_precip
  point_out$prec <- point_out$prec1/point_out$area 
  
  return(point_out)
}

setwd("~/INVASIBILITY_THRESHOLD/data/mcera5/")
# Designate your desired bounding coordinates (in WGS84 / EPSG:4326)
xmn <- -19
xmx <- 4.5
ymn <- 27
ymx <- 44

# Spain map 
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
esp_can$centroid <- st_centroid(esp_can$geometry)
esp_can$area <- st_area(esp_can$geometry)
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
esp_can$geometry <- NULL
census <- mapSpain::pobmun19
esp_can <- esp_can %>% left_join(census,
                                 by = c("cmun" = "cmun","cpro" = "cpro"))
esp_can <- esp_can[,c("NATCODE", "centroid", "pob19", "area")]
# List the path of an .nc file that was downloaded via
# request_era5()
year = 2010
my_nc <- paste0(getwd(),"/era5_Spain_",year,".nc")
# Number of cores used in the parallelization
num_cores = 10
# Parallelize function in order to obtain value R0 for each municipality
climat_each_muni <- mclapply(c(1:nrow(esp_can)), 
                             extract_weather, 
                             mc.cores = num_cores)
# Save the resultant file:
saveRDS(climat_each_muni,
        paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/ERA5_daily_mcera_",year,".Rds"))

year = 2011
my_nc <- paste0(getwd(),"/era5_Spain_",year,".nc")
# Number of cores used in the parallelization
num_cores = 10
# Parallelize function in order to obtain value R0 for each municipality
climat_each_muni <- mclapply(c(1:nrow(esp_can)), 
                             extract_weather, 
                             mc.cores = num_cores)
# Save the resultant file:
saveRDS(climat_each_muni,
        paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/ERA5_daily_mcera_",year,".Rds"))


year = 2012
my_nc <- paste0(getwd(),"/era5_Spain_",year,".nc")
# Number of cores used in the parallelization
num_cores = 10
# Parallelize function in order to obtain value R0 for each municipality
climat_each_muni <- mclapply(c(1:nrow(esp_can)), 
                             extract_weather, 
                             mc.cores = num_cores)
# Save the resultant file:
saveRDS(climat_each_muni,
        paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/ERA5_daily_mcera_",year,".Rds"))


