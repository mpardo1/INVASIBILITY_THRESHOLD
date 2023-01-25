####Title####
# Grabbing weather from all Spain weather stations
## Written using R 4.0.3
rm(list=ls())

####Dependencies####
library(tidyverse)
library(sf)
library(rgdal)
library(ggplot2)
library(lubridate)
library(readxl)
library(janitor)
library(RcppRoll)
library(parallel)
library(data.table)
library(brms)
library(cmdstanr)
library(matrixStats)
library(curl)
library(jsonlite)
library(RSocrata)

# SETTING DATES ####
yesterday =today() - 1
last_prediction_date = yesterday

# SETTING CORES ####
ncores = 8

SPAIN_CRS = 25830

# Files from John:
# home/usuaris/j.palmer/research/data/cartography/SIGLIM_Publico_INSPIRE/SHP_ETRS89/recintos_autonomicas_inspire_peninbal_etrs89/recintos_autonomicas_inspire_peninbal_etrs89.shp
# home/usuaris/j.palmer/research/ddata/cartography/1da1315b/Estaciones_Completas.shp
# home/usuaris/j.palmer/research/ddata/cartography/b29c8d56/Estaciones_Termometricas.shp
# home/usuaris/j.palmer/research/ddata/cartography/2aa58725/Estaciones_Automaticas.shp
# home/usuaris/j.palmer/research/ddata/cartography/8892d9c9/Estaciones_Pluviometricas.shp

spain_perimeter = st_read("/home/usuaris/m.pardo/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_peninbal_etrs89.shp") %>% st_transform(SPAIN_CRS) %>% summarize() 

station_points = st_read("/home/usuaris/m.pardo/INVASIBILITY_THRESHOLD/data/Estaciones_Completas.shp") %>% bind_rows(st_read("/home/usuaris/m.pardo/INVASIBILITY_THRESHOLD/data/Estaciones_Termometricas.shp")) %>% bind_rows(st_read("/home/usuaris/m.pardo/INVASIBILITY_THRESHOLD/data/Estaciones_Automaticas.shp")) %>% bind_rows(st_read("/home/usuaris/m.pardo/INVASIBILITY_THRESHOLD/data/Estaciones_Pluviometricas.shp")) %>% st_transform(SPAIN_CRS)


h <- new_handle()
handle_setheaders(h, 'api_key' = 'eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJqb2huLnBhbG1lckB1cGYuZWR1IiwianRpIjoiYWRjYTliNGItNmZkMC00MTlkLWI1MzMtNjRlNzQwMGY2MDAxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2MTA0NTk4MTUsInVzZXJJZCI6ImFkY2E5YjRiLTZmZDAtNDE5ZC1iNTMzLTY0ZTc0MDBmNjAwMSIsInJvbGUiOiIifQ.JtTlq8QIaAEdte8Mn3JrgzGvkwrtboEpswfEK6Lb1Hc')

all_dates = seq.Date(from = as_date("2004-01-01"), to=as_date("2022-07-23"), by = "day")

ncores = 1

i = length(all_dates)-7
weather_daily = bind_rows(lapply(1:length(all_dates), function(i){

  start_date = all_dates[i]
  print(start_date)
  flush.console()
  if(i %% 10 == 0) Sys.sleep(30)

  req = curl_fetch_memory(paste0('https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/', start_date, 'T00%3A00%3A00UTC/fechafin/', start_date, 'T23%3A59%3A59UTC/todasestaciones'), handle=h)

  wurl = fromJSON(rawToChar(req$content))$datos

  req = curl_fetch_memory(wurl)

  wdia  = fromJSON(rawToChar(req$content)) %>% as_tibble() %>% select(fecha, indicativo, velmedia, tmed, tmin, tmax, prec) %>% mutate(
    velmedia = as.numeric(str_replace(velmedia, ",", ".")),
    tmed = as.numeric(str_replace(tmed, ",", ".")),
    tmin = as.numeric(str_replace(tmin, ",", ".")),
    tmax = as.numeric(str_replace(tmax, ",", ".")),
    prec = as.numeric(str_replace(prec, ",", ".")),
    FW = as.integer(velmedia <= (6*3.6)*1000/(60*60)),
    FT = case_when(tmed<=15~0, tmed>30~0, (tmed>15 & tmed <=20)~ (.2*tmed)-3, (tmed>20 & tmed<=25)~1, (tmed>25 & tmed <= 30)~ (-.2*tmed)+6),
    mwi = FW*FT
  ) %>% select(fecha, indicativo, mwi) %>% filter(!is.na(mwi))

  return(wdia)
}))

distinct_station_points = station_points %>% group_by(INDICATIVO) %>% summarize()

weather_daily_sf = distinct_station_points %>% left_join(weather_daily, by=c("INDICATIVO"="indicativo")) %>% filter(!is.na(mwi) & !is.na(fecha))

write_rds(weather_daily, paste0("home/usuaris/m.pardo/INVASIBILITY_THRESHOLD/data/aemet_weather_daily_deep_history_",Sys.Date(),".Rds"))

write_rds(weather_daily_sf, paste0("home/usuaris/m.pardo/INVASIBILITY_THRESHOLD/data/aemet_weather_daily_deep_history_sf_",Sys.Date(),".Rds"))

# Path <- "/home/usuaris/m.pardo/INVASIBILITY_THRESHOLD/OUTPUT/aemet_weather_daily_deep_history.Rds"
# weather_daily <- readRDS(Path)

# # for making voronoi cells for each day as a way of dealing with missing station data.
# weather_daily_sf1 = bind_rows(mclapply(all_dates, function(this_date){
#   print(this_date)
#   flush.console()
#   tmed = tmed %>% filter(fecha == this_date)
#   wdia = weather_daily %>% filter(fecha == this_date)
#   these_stations = wdia %>% pull(indicativo) %>% unique()
#   these_station_points = station_points %>% filter(INDICATIVO %in% these_stations)
#   vor = these_station_points %>% st_geometry() %>% st_union() %>% st_voronoi(envelope = st_geometry(spain_perimeter)) %>% st_collection_extract(type = "POLYGON") %>% st_as_sf() %>% st_intersection(spain_perimeter) %>% st_join(these_station_points)
#   result = vor %>% left_join(wdia, by=c("INDICATIVO"="indicativo")) %>% select(fecha, mwi, geometry)
# # ggplot(result) + geom_sf(aes(fill=mwi)) + scale_fill_distiller(palette="Spectral")
#   return(result)
# }, mc.cores=ncores))
# # 
# write_rds(weather_daily_sf1, paste0("/home/usuaris/m.pardo/INVASIBILITY_THRESHOLD/data/aemet_weather_daily_deep_history_1",Sys.Date(),".Rds"))
