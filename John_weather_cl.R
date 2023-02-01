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

# h <- new_handle()
# handle_setheaders(h, 'api_key' = 'eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJqb2huLnBhbG1lckB1cGYuZWR1IiwianRpIjoiYWRjYTliNGItNmZkMC00MTlkLWI1MzMtNjRlNzQwMGY2MDAxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2MTA0NTk4MTUsInVzZXJJZCI6ImFkY2E5YjRiLTZmZDAtNDE5ZC1iNTMzLTY0ZTc0MDBmNjAwMSIsInJvbGUiOiIifQ.JtTlq8QIaAEdte8Mn3JrgzGvkwrtboEpswfEK6Lb1Hc')
# 
# all_dates = seq.Date(from = as_date("2004-01-01"), to=as_date("2022-07-23"), by = "day")
# 
# ncores = 1
# 
# i = length(all_dates)-7
# weather_daily = bind_rows(lapply(1:length(all_dates), function(i){
# 
#   start_date = all_dates[i]
#   print(start_date)
#   flush.console()
#   if(i %% 10 == 0) Sys.sleep(30)
# 
#   req = curl_fetch_memory(paste0('https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/', start_date, 'T00%3A00%3A00UTC/fechafin/', start_date, 'T23%3A59%3A59UTC/todasestaciones'), handle=h)
# 
#   wurl = fromJSON(rawToChar(req$content))$datos
# 
#   req = curl_fetch_memory(wurl)
# 
#   wdia  = fromJSON(rawToChar(req$content)) %>% as_tibble() %>% select(fecha, indicativo, velmedia, tmed, tmin, tmax, prec) %>% mutate(
#     velmedia = as.numeric(str_replace(velmedia, ",", ".")),
#     tmed = as.numeric(str_replace(tmed, ",", ".")),
#     tmin = as.numeric(str_replace(tmin, ",", ".")),
#     tmax = as.numeric(str_replace(tmax, ",", ".")),
#     prec = as.numeric(str_replace(prec, ",", ".")),
#     FW = as.integer(velmedia <= (6*3.6)*1000/(60*60)),
#     FT = case_when(tmed<=15~0, tmed>30~0, (tmed>15 & tmed <=20)~ (.2*tmed)-3, (tmed>20 & tmed<=25)~1, (tmed>25 & tmed <= 30)~ (-.2*tmed)+6),
#     mwi = FW*FT
#   ) %>% select(fecha, indicativo, mwi, tmed, tmin, tmax, prec) %>% filter(!is.na(mwi))
# 
#   return(wdia)
# }))
# 
# distinct_station_points = station_points %>% group_by(INDICATIVO) %>% summarize()
# 
# weather_daily_sf = distinct_station_points %>% left_join(weather_daily, by=c("INDICATIVO"="indicativo")) %>% filter(!is.na(mwi) & !is.na(fecha))
# 
# write_rds(weather_daily, paste0("~/INVASIBILITY_THRESHOLD/output/weather/aemet_weather_daily_deep_history_",Sys.Date(),".Rds"))
# 
# write_rds(weather_daily_sf, paste0("~/INVASIBILITY_THRESHOLD/output/weather/aemet_weather_daily_deep_history_sf_",Sys.Date(),".Rds"))

Path <- "~/INVASIBILITY_THRESHOLD/output/weather/aemet_weather_daily_deep_history_sf_2023-01-30.Rds"
weather_daily <- readRDS(Path)


# Filter weather data for the left join otherwise too heavy.
weather_daily$fecha <- as.Date(weather_daily$fecha)
weather_daily_filt <- as.data.frame(weather_daily[which(weather_daily$fecha >  as.Date("2021-01-01")),])
weather_daily_filt$month <- format(weather_daily_filt$fecha, "%m") 
weather_daily_filt$year <- format(weather_daily_filt$fecha, "%y") 
weather_daily_filt[which(is.na(weather_daily_filt$prec)),8] <- 0
weather_daily_filt_mean <- weather_daily_filt %>%  group_by(INDICATIVO, month, year) %>% 
  summarise(geometry = geometry, 
            tmin = min(tmin), tmax = max(tmax), tmed = mean(tmed),
            precmed = mean(prec), num_weather = n())

weather_daily_filt_mean <- weather_daily_filt_mean[order(weather_daily_filt_mean$INDICATIVO,
                                                         weather_daily_filt_mean$year,
                                                         weather_daily_filt_mean$month),]

month_e <- "06"
year_e <- "21"
weather_daily_filt_mean <- weather_daily_filt_mean[which(weather_daily_filt_mean$month == month_e & weather_daily_filt_mean$year == year_e),]
station_meteo <- unique(weather_daily_filt_mean %>%  group_by(INDICATIVO) %>% 
  summarise(geometry = geometry))

rm(weather_daily)
#---------------------------------------------------------------------------#
#----------------------ASSOCIATION METEO STATION MUNICIPALITIES-----------------------------#
#### Associate municipalities to meteo stations
### All files in John Palmer cluster directory 
# setwd("~/SpainTiger")
# source("scripts/functions.r")
# source("scripts/parameters.r")
# 
# cell_res <- 1000
# cell_mask <- 0.025
# SPAIN_CRS = 25830
# ua <- read_rds("data/proc/spain_ua_multipolygon.Rds")
# st_crs(ua) <- 25830
# spain_map <- read_rds("data/proc/spain_map.Rds")
# spain_perimeter <- read_rds("data/proc/spain_perimeter.Rds")
# st_crs(spain_perimeter) <- 4258
# 
# # Voronoi cells for weather stations ####
# station_points <- station_points[which(station_points$INDICATIVO %in% station_meteo$INDICATIVO),]
# spain_perimeter = st_read("~/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_peninbal_etrs89.shp") %>% 
#   bind_rows(st_read("~/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_canarias_wgs84.shp")) %>% 
#   st_transform(SPAIN_CRS) %>% summarize() 
# 
# vor = station_points %>% st_geometry() %>% st_union() %>%
#   st_voronoi(envelope = st_geometry(spain_perimeter)) %>%
#   st_collection_extract(type = "POLYGON") %>% st_as_sf() %>%
#   st_intersection(spain_perimeter) %>% st_join(station_points)
# 
# ggplot(vor) + 
#   geom_sf()

# Loading Models ####
# spain_muni_map = st_read("data/cartography/SIGLIM_Publico_INSPIRE/SHP_ETRS89/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89.shp") %>%
#   bind_rows(st_read("~/INVASIBILITY_THRESHOLD/data/recintos_municipales_inspire_canarias_wgs84/recintos_municipales_inspire_canarias_wgs84.shp")) 
# st_crs(spain_muni_map) = 4258
# spain_muni_map = spain_muni_map %>% st_transform(st_crs(ua))
# 
# # muni_points = spain_muni_map %>% select(geometry) %>% st_centroid() # creating muni centroids so that the munis end up with at least 1 sampled point
# this_perimeter_25830 <- spain_perimeter %>% st_transform(st_crs(ua)) %>% st_union()
# these_points = st_make_grid(st_bbox(this_perimeter_25830)+100000*c(-1,-1,1,1),
#                             cellsize = c(cell_res,cell_res), what = "polygons",
#                             square = TRUE) %>% st_sf %>%
#   st_join(spain_muni_map %>% 
#             dplyr::select(NAMEUNIT, NATCODE, CODNUT1, CODNUT2, CODNUT3),
#           join = st_intersects, left=FALSE) %>% 
#   st_cast("POINT") %>% st_as_sf() %>% 
#   st_join(vor %>% dplyr::select(INDICATIVO) %>% 
#             rename(indicativo = INDICATIVO), join = st_intersects, left=FALSE) 
# 
# write_rds(these_points, paste0("~/INVASIBILITY_THRESHOLD/data/station_municipalities.Rds"))

Path <- "~/INVASIBILITY_THRESHOLD/data/station_municipalities.Rds"
these_points <- readRDS(Path)

these_points <- unique(these_points[,c(1,6)])
these_points_filt <- unique(as.data.frame(these_points)[,c(1,2)])
# ggplot(these_points) + geom_sf() + geom_sf(data=this_perimeter_25830, color="red", fill=NA)
# length(unique(these_points$NATCODE))
# these_points_muni_xy25830_df = unlist(st_geometry(these_points)) %>%
#   matrix(ncol=2,byrow=TRUE) %>% as_tibble(.name_repair="universal") %>%
#   setNames(c("crs25830_x","crs25830_y")) %>% as_tibble()
# 
# these_points_muni_lonlat = these_points %>% st_transform(crs=4326)
# 
# these_points_muni_lonlat_df = unlist(st_geometry(these_points_muni_lonlat)) %>%
#   matrix(ncol=2,byrow=TRUE) %>% as_tibble(.name_repair="universal") %>% 
#   setNames(c("lon","lat")) %>% as_tibble()
# these_points_muni = these_points %>% 
#   mutate(lon = these_points_muni_lonlat_df$lon, lat = these_points_muni_lonlat_df$lat, 
#          crs25830_x = these_points_muni_xy25830_df$crs25830_x, 
#          crs25830_y = these_points_muni_xy25830_df$crs25830_y)
# 
# write_rds(these_points_muni_lc, paste0("data/prediction_points_spain/prediction_points_lc_sf_df_all_spain_", cell_res, ".Rds"))
# 
# # Filter weather stations by municipality:
# Path <- "~/SpainTiger/data/prediction_points_spain/prediction_points_lc_sf_df_all_spain_1000.Rds"
# these_points_muni_lc <- readRDS(Path)
# these_points_muni_lc <- these_points_muni_lc[, c(2,7)]
# these_points_muni_lc$geometry <- NULL
# these_points_muni_lc <- unique(these_points_muni_lc)


# Join data frame municipalities names (these_points_muni_lc) with weather data from 
# meteo stations (weather_daily_filt_mean)
these_points_filt
weather_municip <- merge(x=weather_daily_filt_mean, y=these_points_filt, 
                   by.x="INDICATIVO", by.y="indicativo", all.x=TRUE, all.y = TRUE)

# If there is more than one meteo station related with one municipality, average the temperatures
# for the mean temp, and get the minimum (maximum) for the stations for the mintemp (maxtemp)
weather_municip <-  weather_municip %>%  group_by(NAMEUNIT, month, year) %>% 
  summarise(tmin = min(tmin), tmax = max(tmax), tmed = mean(tmed),
            precmed = mean(precmed), rep = sum(num_weather), n = n())

# Sort the data frame in order to take a look easily
weather_municip <- weather_municip[order(weather_municip$NAMEUNIT,
                                         weather_municip$year,
                                         weather_municip$month),]
write_rds(weather_municip, paste0("~/INVASIBILITY_THRESHOLD/output/weather/aemet_weather_daily_deep_history_sf_",Sys.Date(),".Rds"))

