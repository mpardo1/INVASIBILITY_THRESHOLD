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
#-----------------------CLUSTER--------------------------#
conda_create("r-reticulate")
conda_install("r-reticulate","cdsapi", pip=TRUE)#import python CDS-API
# indicate that we want to use a specific condaenv
use_condaenv("r-reticulate")
cdsapi <- import("cdsapi")
#-----------------------------------------------------#
#-------------------LOCAL----------------------------#
#install the CDS API
conda_install("r-reticulate","cdsapi", pip=TRUE)#import python CDS-API

cdsapi <- import("cdsapi")
#-----------------------------------------------------#
#for this step there must exist the file .cdsapirc
server = cdsapi$Client() #start the connection

era5_rain <- function(day_n,month_n,year_n, time_n){
    query <- r_to_py(list(
      variable= "total_precipitation",
      product_type= "reanalysis",
      year= year_n,
      month= month_n, #formato: "01","01", etc.
      day= day_n,   
      time= time_n,
      format= "netcdf",
      area =  "45/-30/25/5" # North, West, South, East
    ))
    Path = paste0("era5_temp_",year_n,month_n, day_n,".grib")
    server$retrieve("reanalysis-era5-single-levels",
                    query,
                    Path)
    
    ## Read the file from Copernicus data.
    nc_raster <- rast(Path) 
    return(nc_raster)
}

daily_prec <- function(mon, ye){
    d = "01"
    m = mon
    y = ye
    date <- as.Date(paste0(y,"-",m,"-",d), "%Y-%m-%d")
    num_days <- lubridate::days_in_month(date)
    vec_times = str_c(0:23,"00",sep=":")%>%str_pad(5,"left","0")
    t = vec_times[1]
    nc_raster <- era5_rain(d,m,y,t)
    # nc_raster
    coord_ref <- st_crs(nc_raster)
    # plot(nc_raster,1)
    esp_can <- esp_get_munic_siane(moveCAN = FALSE)
    esp <- st_transform(esp_can,coord_ref)
    #--- extract values from the raster for each county ---#
    temp_muni <- terra::extract(nc_raster, esp)
    colnames(temp_muni) <- c("ID",paste0(as.character(time(nc_raster)),"/",t))
    temp_muni_daily <- data.frame(ID = seq(1,14876), pctest = 1)
    for(i in c(2:num_days)){
      d = ifelse(i<10, paste0("0", as.character(i)),as.character(i))
        # esp_prec <- esp_can %>% left_join(temp_muni)
        for(i in c(2:length(vec_times))){
            t = vec_times[i]
            nc_raster <- era5_rain(d,m,y,t)
            # nc_raster
            coord_ref <- st_crs(nc_raster)
            # plot(nc_raster,1)
            esp_can <- esp_get_munic_siane(moveCAN = FALSE)
            esp <- st_transform(esp_can,coord_ref)
            
            # plot(rast_spain,2)
            #--- extract values from the raster for each county ---#
            temp <- terra::extract(nc_raster, esp)
            colnames(temp) <-  c("ID",paste0(as.character(time(nc_raster)),"/",t))
            temp_muni <- cbind(temp,temp_muni)
        }
        
        temp_muni <- temp_muni[,c(1,seq(2,ncol(temp_muni),2))]
        # unique(c(temp_muni_filt))
        temp_muni$precmean <- rowMeans(temp_muni[,2:ncol(temp_muni)])
        temp_muni_t <- temp_muni[,c(1,ncol(temp_muni))]
        colnames(temp_muni_t) <- c("ID",paste0(y,"-",m,"-",d))
        temp_muni_daily <- cbind(temp_muni_daily, temp_muni_t)
    }
  return(temp_muni_daily)
}

y = "2019"
for (i in c(1:12)) {
  m <- ifelse(i<10, paste0("0", as.character(i)),as.character(i))
  df_Jan_2020 <- daily_prec(m,y)
  df_Jan_2020_t <- df_Jan_2020[,c(1,4:ncol(df_Jan_2020))]
  df_Jan_2020_t <- df_Jan_2020_t[,c(1,seq(2,ncol(df_Jan_2020_t),2))]
  esp_sf <- st_as_sf(esp) %>%
    mutate(ID := seq_len(nrow(.))) %>%
    left_join(., df_Jan_2020_t, by = "ID")
  
  esp_sf$NATCODE <- as.numeric(paste0("34",esp_sf$codauto,esp_sf$cpro,esp_sf$LAU_CODE))
  esp_sf <- esp_sf[,c(9:ncol(esp_sf))]
  esp_sf$geometry <- NULL
  saveRDS(esp_sf, paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/rainfall/rainfall_",y,"_",m,".Rds"))
  rm(esp_sf,df_Jan_2020_t, df_Jan_2020)
}

#--- get mean tmax ---#
temp_muni <- temp_muni %>%
  group_by(ID) %>%
  summarize(prec = mean(`2018-07-02` ))

esp_sf <- st_as_sf(esp) %>%
  mutate(ID := seq_len(nrow(.))) %>%
  left_join(., temp_muni, by = "ID")

esp_sf$prec1 <- esp_sf$prec + 280
ggplot(esp_sf) +
  geom_sf(aes(fill = prec), size = 0.1) +
  scale_fill_distiller(palette="Spectral")

