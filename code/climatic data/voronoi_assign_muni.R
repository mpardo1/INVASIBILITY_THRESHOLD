####Title####
# Grabbing weather from all Spain weather stations
## Written using R 4.0.3
rm(list=ls())

####Dependencies####
library(tidyverse)
library(sf)
library(rgdal)
library(readxl)
library(janitor)
library(brms)
library(cmdstanr)
library(matrixStats)
library(curl)
library(jsonlite)
library(RSocrata)
library(sf)
library(tmap)
library(ggplot2)
library(lubridate)
library(readxl)
library(janitor)
library(RcppRoll)
library(parallel)
library(data.table)
# library(groupdata2)

# SETTING DATES ####
yesterday =today() - 1
last_prediction_date = yesterday

# SETTING CORES ####
ncores = 8

SPAIN_CRS = 25830

# In order to work this github repository you need to create a directory called data in 
# INVASIBILITY_THRESHOLD with the files below (dowload from john cluster or aemet):
# Files from John:
# home/usuaris/j.palmer/research/data/cartography/SIGLIM_Publico_INSPIRE/SHP_ETRS89/recintos_autonomicas_inspire_peninbal_etrs89/recintos_autonomicas_inspire_peninbal_etrs89.shp
# home/usuaris/j.palmer/research/ddata/cartography/1da1315b/Estaciones_Completas.shp
# home/usuaris/j.palmer/research/ddata/cartography/b29c8d56/Estaciones_Termometricas.shp
# home/usuaris/j.palmer/research/ddata/cartography/2aa58725/Estaciones_Automaticas.shp
# home/usuaris/j.palmer/research/ddata/cartography/8892d9c9/Estaciones_Pluviometricas.shp
# website aemet download data (Estaciones_completas etc): https://opendata.aemet.es/centrodedescargas/productosAEMET
# Website aemet con querys: https://opendata.aemet.es/dist/index.html?#/maestro/getMunicipioUsingGET
# Comand to copy a whole directory from john cluster to your when you are in your directory in the cluster:
# cp -r /home/usuaris/j.palmer/research/SpainTiger/data/cartography/SIGLIM_Publico_INSPIRE/SHP_WGS84/recintos_autonomicas_inspire_canarias_wgs84 .
#-----------------------Export data from aemet-----------------------#
spain_perimeter = st_read("~/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_peninbal_etrs89.shp") %>% 
  bind_rows(st_read("~/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_canarias_wgs84.shp")) %>% 
  st_transform(SPAIN_CRS) %>% summarize() 

station_points = st_read("~/INVASIBILITY_THRESHOLD/data/Estaciones_Completas.shp") %>%
  bind_rows(st_read("~/INVASIBILITY_THRESHOLD/data/Estaciones_Termometricas.shp")) %>%
  bind_rows(st_read("~/INVASIBILITY_THRESHOLD/data/Estaciones_Automaticas.shp")) %>% 
  bind_rows(st_read("~/INVASIBILITY_THRESHOLD/data/Estaciones_Pluviometricas.shp")) %>% 
  st_transform(SPAIN_CRS)

# File with weather data:
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/aemet_weather_daily_deep_history_sf_2023-01-30.Rds"
weather_daily <- readRDS(Path)

# Average weather data by month, smaller database:
weather_daily$fecha <- as.Date(weather_daily$fecha)
weather_daily_filt <- as.data.frame(weather_daily[which(weather_daily$fecha >  as.Date("2010-01-01")),])
weather_daily_filt$month <- format(weather_daily_filt$fecha, "%m") 
weather_daily_filt$year <- format(weather_daily_filt$fecha, "%y") 
weather_daily_filt[which(is.na(weather_daily_filt$prec)),8] <- 0
weather_daily_filt_mean <- weather_daily_filt %>%  group_by(INDICATIVO, month, year) %>% 
  summarise(tmin = min(tmin), tmax = max(tmax), tmed = mean(tmed),
            precmed = mean(prec), num_weather = n())

indi_station_geo <- unique(weather_daily_filt[, c(1,2)])

weather_daily_filt_mean <- weather_daily_filt_mean[order(weather_daily_filt_mean$year,
                                                         weather_daily_filt_mean$month,
                                                         weather_daily_filt_mean$INDICATIVO),]

# # List of list with meteo station that are depending on the month.
# part_df <- split(weather_daily_filt_mean, list(weather_daily_filt_mean$month, weather_daily_filt_mean$year))
# list_meteo_station <- lapply(part_df, "[", 1)
# list_INDI <- unique(list_meteo_station)

# Delete non use data frame to free space.
rm(weather_daily, weather_daily_filt)
len <- length(unique(with(weather_daily_filt_mean, paste0(year, month))))
#---------------------------------------------------------------------------#

#----------------------ASSOCIATION METEO STATION MUNICIPALITIES-----------------------------#
setwd("~/SpainTiger")
source("scripts/functions.r")
source("scripts/parameters.r")

cell_res <- 1000
cell_mask <- 0.025
SPAIN_CRS = 25830
ua <- read_rds("data/proc/spain_ua_multipolygon.Rds")
st_crs(ua) <- 25830
spain_perimeter <- read_rds("data/proc/spain_perimeter.Rds")
st_crs(spain_perimeter) <- 4258

len <- length(unique(with(weather_daily_filt_mean, paste0(year, month))))
# Since there are not data for all stations every month, we should compute the voronoi for each month
# Highly cost computationally.

# Function that input the weather_file data with the station and the variables and output the same weather file
# with the municipalities name
rel_meteostat_muni <- function(weather_daily_f){
  init_time <- Sys.time()
  station_points <- station_points[which(station_points$INDICATIVO %in% unique(weather_daily_f$INDICATIVO)),]
  spain_perimeter = st_read("~/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_peninbal_etrs89.shp") %>%
    bind_rows(st_read("~/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_canarias_wgs84.shp")) %>%
    st_transform(SPAIN_CRS) %>% summarize()
  
  
  # Voronoi cells for weather stations ####
  vor = station_points %>% st_geometry() %>% st_union() %>%
    st_voronoi(envelope = st_geometry(spain_perimeter)) %>%
    st_collection_extract(type = "POLYGON") %>% st_as_sf() %>%
    st_intersection(spain_perimeter) %>% st_join(station_points)
  
  # ggplot(vor) +
  #   geom_sf()
  
  # John Codes: ####
  spain_muni_map = st_read("~/INVASIBILITY_THRESHOLD/muni_data/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89.shp") %>%
    bind_rows(st_read("~/INVASIBILITY_THRESHOLD/data/recintos_municipales_inspire_canarias_wgs84/recintos_municipales_inspire_canarias_wgs84.shp"))
  
  # Cambia el sistema de coordenadas.
  st_crs(spain_muni_map) = 4258
  spain_muni_map = spain_muni_map %>% st_transform(st_crs(ua))
  this_perimeter_25830 <- spain_perimeter %>% st_transform(st_crs(ua)) %>% st_union()
  these_points = st_make_grid(st_bbox(this_perimeter_25830)+100000*c(-1,-1,1,1),
                              cellsize = c(cell_res,cell_res), what = "polygons",
                              square = TRUE) %>% st_sf %>%
    st_join(spain_muni_map %>%
              dplyr::select(NAMEUNIT, NATCODE, CODNUT1, CODNUT2, CODNUT3),
            join = st_intersects, left=FALSE) %>%
    st_cast("POINT") %>% st_as_sf() %>%
    st_join(vor %>% dplyr::select(INDICATIVO) %>%
              rename(indicativo = INDICATIVO), join = st_intersects, left=FALSE)
  
  these_points <- unique(these_points[,c(1,6,7)])
  these_points$geometry <- NULL
  these_points <- unique(these_points)
  
  weather_municip <- merge(x=weather_daily_f, y=these_points, 
                           by.x="INDICATIVO", by.y="indicativo", all.x=TRUE, all.y = TRUE)
  
  weather_municip <-  weather_municip %>%  group_by(NAMEUNIT, month, year) %>% 
    summarise(tmin = min(tmin), tmax = max(tmax), tmed = mean(tmed),
              precmed = mean(precmed), rep = sum(num_weather), n = n())
  
  rm(these_points,spain_muni_map, vor,spain_perimeter)
  print(paste0("Time expended in the function:", Sys.time() -  init_time )) 
  return(weather_municip)
}

### Loop over all months and years:
min_year <- as.numeric(min(weather_daily_filt_mean$year))
max_year <- as.numeric(max(weather_daily_filt_mean$year))
while(min_year < (max_year + 1)){
  for(i in c(1:12)){
    weather_daily_f <- weather_daily_filt_mean[which(as.numeric(weather_daily_filt_mean$month) == i &
                                                       as.numeric(weather_daily_filt_mean$year) == min_year),]
    df_weather <- rel_meteostat_muni(weather_daily_f)
    write_rds(df_weather, paste0("~/INVASIBILITY_THRESHOLD/output/weather/aemet_weather_monthly_",i,"_",min_year,".Rds"))
    rm(weather_daily_f,df_weather)  
  }
  min_year = min_year + 1
}

