library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)

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
st_time <- lubridate::ymd("2020:01:01")
en_time <- lubridate::ymd("2020:12:31")

file_prefix <- "era5_Spain"
file_path <- getwd()

req <- build_era5_request(xmin = xmn, xmax = xmx,
                          ymin = ymn, ymax = ymx,
                          start_time = st_time,
                          end_time = en_time,
                          outfile_name = file_prefix)

request_era5(request = req, uid = uid, out_path = file_path)

# List the path of an .nc file that was downloaded via
# `request_era5()` 
my_nc <- paste0(getwd(), "/era5_Spain_2020.nc")
my_nc = "/home/marta/era5_Spain_2020.nc"

# Compute centroid for each municipality
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
esp_can$centroid <- st_centroid(esp_can$geometry)
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE))
census <- mapSpain::pobmun19
esp_can <- esp_can %>% left_join(census, by = c("cmun" = "cmun","cpro" = "cpro"))

ind = 551
# Specify desired single point (within the bounds of your .nc file) 
x <- esp_can$centroid[ind][[1]][1]
y <- esp_can$centroid[ind][[1]][2]

# Gather all hourly variables, with spatial and temporal dimensions
# matching the extent, or a subset, of data in one downloaded file 
point_out <- extract_clim(nc = my_nc, long = x,
                          lat = y, start_time = st_time,
                          end_time = en_time) 

point_out <- point_out[,c("obs_time", "temperature")]
ggplot(point_out) +
  geom_line(aes(obs_time,temperature), size = 0.1) +
  theme_bw()

point_out$NATCODE <- esp_can$NATCODE[ind]
point_out$pop <- esp_can$pob19[ind]
# You can then inspect the data frame
# head(point_out)

# Gather daily precipitation 
point_out_precip <- extract_precip(nc = my_nc, long = x, 
                                   lat = y, start_time = st_time, 
                                   end_time = en_time, convert_daily = FALSE)

point_out$prec <- point_out_precip
ggplot(point_out) +
  geom_line(aes(obs_time,prec), size = 0.1) +
  theme_bw()
