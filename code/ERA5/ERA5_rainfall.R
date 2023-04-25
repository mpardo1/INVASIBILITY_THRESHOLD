rm(list=ls())
library(ncdf4)
library(tidyverse)
library(sf) 
library(raster)
library(terra)
library("mapSpain")
library(reticulate)
library(ncdf4)

# Link ERA5 data:
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-cerra-single-levels?tab=overview
# You need to create a file with the url and API key in (you need to log in first):https://cds.climate.copernicus.eu/api-how-to
#install the CDS API
conda_install("r-reticulate","cdsapi", pip=TRUE)#import python CDS-API

cdsapi <- import("cdsapi")
#for this step there must exist the file .cdsapirc
server = cdsapi$Client() #start the connection
# ------------------------------------------------------------#
# d = "01"
# m = "03"
# y = "2021"
# # ERA5 Land
# query <- r_to_py(list(
#   variable= "total_precipitation",
#   year= y,
#   month= m,
#   day= d,
#   time="00:00",
#   format= "grib"
# ))
# 
# 
# #query to get the ncdf
# Path = paste0("era5land_temp_",y,"_",m,"_", d,".grib")
# server$retrieve("reanalysis-era5-single-levels",
#                 query,
#                 Path)
# 
# nc_raster <- rast(Path) 
# plot(nc_raster,2)
#---------------------------------------------------------------#
era5_rain <- function(day_n,month_n,year_n){
    query <- r_to_py(list(
      variable= "total_precipitation",
      level_type= "surface_or_atmosphere",
      data_type= "reanalysis",
      product_type=" forecast",
      year= year_n,
      month= month_n,
      day= day_n, 
      time="00:00",
      format= "grib",
      leadtime_hour = "24"
    ))
    
    
    #query to get the ncdf
    Path = paste0("era5_temp_",year_n,month_n, day_n,".grib")
    server$retrieve("reanalysis-cerra-single-levels",
                    query,
                    Path)
    
    ## Read the file from Copernicus data.
    nc_raster <- rast(Path) 
    return(nc_raster)
}

#we create the query
d = "10"
m = "03"
y = "2021"
date <- as.Date(paste0(y,"-",m,"-",d), "%Y-%m-%d")
num_days <- lubridate::days_in_month(date)
nc_raster <- era5_rain(d,m,y)
# nc_raster
coord_ref <- st_crs(nc_raster)
# plot(nc_raster,1)
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
esp <- st_transform(esp_can,coord_ref)
# plot(rast_spain,2)
#--- extract values from the raster for each county ---#
temp_muni <- terra::extract(nc_raster, esp)
colnames(temp_muni) <- c("ID",as.character(time(nc_raster)))
esp_prec <- esp_can %>% left_join(temp_muni)
for(i in c(2:num_days)){
    d <- ifelse(i<10, paste0("0",i), as.character(i))
    nc_raster <- era5_rain(d,m,y)
    # nc_raster
    coord_ref <- st_crs(nc_raster)
    # plot(nc_raster,1)
    esp_can <- esp_get_munic_siane(moveCAN = FALSE)
    esp <- st_transform(esp_can,coord_ref)
    
    # plot(rast_spain,2)
    #--- extract values from the raster for each county ---#
    temp <- terra::extract(nc_raster, esp)
    colnames(temp) <- c("ID",as.character(time(nc_raster)))
    temp_muni <- cbind(temp,temp_muni)
}

# temp_muni_filt <- temp_muni[,c(seq(2,ncol(temp_muni),2))]
# unique(c(temp_muni_filt))
# temp_muni$precmean <- rowMeans(temp_muni[,2:ncol(temp_muni)])
#--- get mean tmax ---#
temp_muni <- temp_muni %>%
  group_by(ID) %>%
  summarize(prec = mean(`2021-03-11`))


esp_sf <- st_as_sf(esp) %>%
  mutate(ID := seq_len(nrow(.))) %>%
  left_join(., temp_muni, by = "ID")

esp_sf$prec1 <- esp_sf$prec + 280
ggplot(esp_sf) +
  geom_sf(aes(fill = prec1), size = 0.1) +
  scale_fill_distiller(palette="Spectral")
# df_temp <- as.data.frame(esp_sf[,c("name", "tmean")])
# df_temp$geometry <- NULL
# esp_can <- esp_get_munic_siane(moveCAN = TRUE)
# esp_can <-  esp_can  %>% 
#   left_join(df_temp)
# esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE))
# esp_can$geometry <- NULL
# df_temp <- esp_can[,c("NATCODE","tmean")]
# saveRDS(df_temp,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",year_n,".Rds"))

rm(list=ls())
library(ncdf4)
library(tidyverse)
library(sf) 
library(raster)
library(terra)
library("mapSpain")
library(reticulate)
library(ncdf4)

# Link ERA5 data:
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-cerra-single-levels?tab=overview
# You need to create a file with the url and API key in (you need to log in first):https://cds.climate.copernicus.eu/api-how-to
#install the CDS API
conda_install("r-reticulate","cdsapi", pip=TRUE)#import python CDS-API

cdsapi <- import("cdsapi")
#for this step there must exist the file .cdsapirc
server = cdsapi$Client() #start the connection

year_n = "2020"
month_n = "03"
day_n = "01"
query <- r_to_py(list(
  variable= "total_precipitation",
  level_type= "surface_or_atmosphere",
  data_type= "reanalysis",
  product_type=" forecast",
  year= year_n,
  month= month_n,
  day= day_n, 
  time="00:00",
  format= "netcdf",
  leadtime_hour = "24"
))


#query to get the ncdf
Path = paste0("1newera5_temp_",year_n,month_n, day_n,".nc")
server$retrieve("reanalysis-cerra-single-levels",
                query,
                Path)

nc_raster <- rast(Path)
plot(nc_raster,1)
# lambert <- "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# crs(nc_raster) <- lambert
coord_ref <- st_crs(nc_raster)
# plot(nc_raster,1)
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
esp <- st_transform(esp_can,coord_ref)
# plot(rast_spain,2)
#--- extract values from the raster for each county ---#
temp_muni <- terra::extract(nc_raster, esp)
colnames(temp_muni) <- c("ID","prec")
temp_muni <- temp_muni %>%
  group_by(ID) %>%
  summarize(prec = mean(prec))


esp_sf <- st_as_sf(esp) %>%
  mutate(ID := seq_len(nrow(.))) %>%
  left_join(., temp_muni, by = "ID")

ggplot(esp_sf) +
  geom_sf(aes(fill = prec), size = 0.1) +
  scale_fill_distiller(palette="Spectral")
