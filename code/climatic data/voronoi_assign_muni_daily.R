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
library("mapSpain")
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
weather_daily$month <- format(weather_daily$fecha, "%m") 
weather_daily$year <- format(weather_daily$fecha, "%y") 
weather_daily$day <- format(weather_daily$fecha, "%d") 
weather_daily[which(is.na(weather_daily$prec)),8] <- 0

indi_station_geo <- unique(weather_daily[, c(1,2)])

weather_daily_filt_mean <- weather_daily[order(weather_daily$year,
                                               weather_daily$month,
                                               weather_daily$INDICATIVO),]

# # List of list with meteo station that are depending on the month.
# part_df <- split(weather_daily_filt_mean, list(weather_daily_filt_mean$month, weather_daily_filt_mean$year))
# list_meteo_station <- lapply(part_df, "[", 1)
# list_INDI <- unique(list_meteo_station)

# Delete non use data frame to free space.
rm(weather_daily)
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
  print("Antes de st_read")
  spain_perimeter = st_read("~/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_peninbal_etrs89.shp") %>%
    bind_rows(st_read("~/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_canarias_wgs84.shp")) %>%
    st_transform(SPAIN_CRS) %>% summarize()
  print("Antes de drop_na")
  station_points <- station_points %>% drop_na()
  
  print("Antes de voronoi")
  # Voronoi cells for weather stations ####
  vor = station_points %>% st_geometry() %>% st_union() %>%
    st_voronoi(envelope = st_geometry(spain_perimeter)) %>%
    st_collection_extract(type = "POLYGON") %>% st_as_sf() %>%
    st_intersection(spain_perimeter) %>% st_join(station_points)
  
  # ggplot(vor) +
  #   geom_sf()
  
  # John Codes: ####
  # spain_muni_map = st_read("~/INVASIBILITY_THRESHOLD/muni_data/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89.shp") %>%
  #   bind_rows(st_read("~/INVASIBILITY_THRESHOLD/data/recintos_municipales_inspire_canarias_wgs84/recintos_municipales_inspire_canarias_wgs84.shp"))
  
  print("Después del voronoi")
  # Cambia el sistema de coordenadas.
  # esp_can <- esp_get_munic_siane(moveCAN = FALSE)
  esp_can <- readRDS("~/INVASIBILITY_THRESHOLD/data/esp_can.Rds")
  st_crs(esp_can) = 4258
  
  print("Antes de usar esp_can")
  spain_muni_map = esp_can %>% st_transform(st_crs(ua))
  print("despues del st_transform ")
  this_perimeter_25830 <- spain_perimeter %>% st_transform(st_crs(ua)) %>% st_union()
  
  print("Antes de these points ")
  these_points = st_make_grid(st_bbox(this_perimeter_25830)+100000*c(-1,-1,1,1),
                              cellsize = c(cell_res,cell_res), what = "polygons",
                              square = TRUE) %>% st_sf %>%
    st_join(spain_muni_map %>%
              dplyr::select(name, ine.ccaa.name, ine.prov.name),
            join = st_intersects, left=FALSE) %>%
    st_cast("POINT") %>% st_as_sf() %>%
    st_join(vor %>% dplyr::select(INDICATIVO) %>%
              rename(indicativo = INDICATIVO), join = st_intersects, left=FALSE)
  
  print("Despues de usar esp_can")
  rm(ua, this_perimeter_25830)
  
  these_points$geometry <- NULL
  these_points <- unique(these_points)
  
  print("*************Antes del merge")
  weather_municip <- merge(x=weather_daily_f, y=these_points, 
                           by.x="INDICATIVO", by.y="indicativo", all.x=TRUE, all.y = TRUE)
  
  print("Antes del groupby")
  weather_municip <-  weather_municip %>%  group_by(name, ine.ccaa.name, ine.prov.name, fecha) %>% 
    summarise(tmin = ifelse(is.na(tmin) | is.infinite(tmin),0,min(tmin)),
              tmax = ifelse(is.na(tmax) | is.infinite(tmax),0,max(tmax)),
              tmed = ifelse(is.na(tmed) | is.infinite(tmed),0,mean(tmed)),
              precmed = ifelse(is.na(prec) | is.infinite(prec),0,mean(prec)), n = n())
  print("Despues del groupby")
  
  rm(these_points,spain_muni_map, vor,spain_perimeter)
  print(paste0("Time expended in the function:", Sys.time() -  init_time )) 
  return(weather_municip)
}

### Loop over all months and years:
Cores <- 10 #parallel::detectCores()#Numero de cores a utilizar.
min_year <- as.numeric(min(weather_daily_filt_mean$year))
max_year <- as.numeric(max(weather_daily_filt_mean$year))
print("***********************Antes del while")
while(min_year <= max_year ){
  print(paste0("Min year:", min_year))
    weather_daily_f <- weather_daily_filt_mean[which(as.numeric(weather_daily_filt_mean$year) == min_year),]
    weather_daily_f <- weather_daily_f %>% drop_na()
    print("Después del dropna")
    weather_daily_f$day_month <- as.numeric(yday(weather_daily_f$fecha))
    min_day = min( weather_daily_f$day_month)
    max_day = max( weather_daily_f$day_month)
    print("Antes del malapply")
    weather_df_y <- mclapply(min_day:max_day, mc.cores = Cores, mc.preschedule = F,function(k){ 
      print("Despues del malapply")
      weather_daily_f <- weather_daily_f[which(as.numeric(weather_daily_f$day_month) == k),]
      print("Dentro del mclapply")
       df_weather <- rel_meteostat_muni(weather_daily_f)
      
      print("Dentro del mclapply")
      if(exists('weather_year') && is.data.frame(get('weather_year'))){
        weather_year <- rbind(df_weather,weather_year)
      }else{
        weather_year <- df_weather
      }
      
      rm(weather_daily_f,df_weather) 
      weather_year
    })
  outweather <- weather_df_y
  write_rds(outweather, paste0("~/INVASIBILITY_THRESHOLD/output/weather/Daily/aemet_weather_year_whole_",min_year,".Rds"))
  min_year = min_year + 1
}

