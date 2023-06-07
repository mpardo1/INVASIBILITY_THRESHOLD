rm(list=ls())
library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)

extract_weather <- function(ind){
  print(paste0("ind:",ind))
  # Specify desired single point (within the bounds of your .nc file) 
  x <- esp_can$centroid[ind][[1]][1]
  y <- esp_can$centroid[ind][[1]][2]
  
  # Gather all hourly variables, with spatial and temporal dimensions
  # matching the extent, or a subset, of data in one downloaded file 
  st_time <- lubridate::ymd("2020:01:01")
  en_time <- lubridate::ymd("2020:12:31")
  point_out <- extract_clim(nc = my_nc, long = x,
                            lat = y, start_time = st_time,
                            end_time = en_time) 
  
  point_out <- point_out[,c("obs_time", "temperature")]
  point_out$NATCODE <- esp_can$NATCODE[ind]
  point_out$pop <- esp_can$pob19[ind]
  # You can then inspect the data frame
  # Gather daily precipitation 
  point_out_precip <- extract_precip(nc = my_nc, long = x, 
                                     lat = y, start_time = st_time, 
                                     end_time = en_time, convert_daily = FALSE)
  
  point_out$prec <- point_out_precip
  
  return(point_out)
}

## Test package mcera5
uid <- "187470"
cds_api_key <- "fbef7343-1aef-44c1-a7c3-573285248e5d"

ecmwfr::wf_set_key(user = uid,
                   key = cds_api_key,
                   service = "cds")
# Designate your desired bounding coordinates (in WGS84 / EPSG:4326)
xmn <- -19
xmx <- 4.5
ymn <- 27
ymx <- 44

# Temporal grid
st_time <- lubridate::ymd("2022:01:01")
en_time <- lubridate::ymd("2022:12:31")

file_prefix <- "era5_Spain_2022"
file_path <- getwd()

req <- build_era5_request(xmin = xmn, xmax = xmx,
                          ymin = ymn, ymax = ymx,
                          start_time = st_time,
                          end_time = en_time,
                          outfile_name = file_prefix)

request_era5(request = req, uid = uid, out_path = file_path)

# List the path of an .nc file that was downloaded via
# request_era5()
my_nc <- paste0(getwd(), "/era5_Spain_2020.nc")
# my_nc = "/home/marta/era5_Spain_2020.nc"

# Compute centroid for each municipality
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
esp_can$centroid <- st_centroid(esp_can$geometry)
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
esp_can$geometry <- NULL
census <- mapSpain::pobmun19
esp_can <- esp_can %>% left_join(census,
                                 by = c("cmun" = "cmun","cpro" = "cpro"))
esp_can <- esp_can[,c("NATCODE", "centroid", "pob19")]
# Number of cores used in the parallelization
num_cores = 1
# Parallelize function in order to obtain value R0 for each municipality
R0_each_muni <- mclapply(c(1:nrow(esp_can)), 
                         extract_weather, 
                         mc.cores = num_cores)

