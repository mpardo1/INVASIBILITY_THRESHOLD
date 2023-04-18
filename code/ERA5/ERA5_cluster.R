rm(list=ls())
library(ncdf4)
library(tidyverse)
library(sf) 
library(raster)
library(terra)
library("mapSpain")
library(reticulate)
library(ncdf4)
library(parallel)

# Tutorial:
# https://dominicroye.github.io/en/2018/access-to-climate-reanalysis-data-from-r/
# Link ERA5 data:
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-cerra-single-levels?tab=overview
# You need to create a file with the url and API key in (you need to log in first):https://cds.climate.copernicus.eu/api-how-to
# Install the CDS API

# create a new environment 
conda_create("r-reticulate")
conda_install("r-reticulate","cdsapi", pip=TRUE)#import python CDS-API
# indicate that we want to use a specific condaenv
use_condaenv("r-reticulate")
cdsapi <- import("cdsapi")
# For this step there must exist the file .cdsapirc
server = cdsapi$Client() #start the connection
Cores = 10
func_weather <- function(mon, ye){
  date <- as.Date(paste0(ye,"-",mon ,"-23"), "%Y-%m-%d")
  max_day = lubridate::days_in_month(date)
  min_day = 1
    # Parallelize
    weather_df_y <- mclapply(min_day:max_day, mc.cores = Cores, mc.preschedule = F,function(k){ #we create the query
      query <- r_to_py(list(
        variable= "2m_temperature",
        level_type= "surface_or_atmosphere",
        data_type= "reanalysis",
        product_type=" analysis",
        year= ye,
        month= mon,
        day= k,
        time=c("00:00", "03:00", "06:00",
               "09:00", "12:00", "15:00",
               "18:00", "21:00"),
        format= "grib"
      ))
      
      #query to get the ncdf
      Path = paste0("era5_temp_",ye,mon, k,".grib")
      server$retrieve("reanalysis-cerra-single-levels",
                      query,
                      Path)
      
      ## Read the file from Copernicus data.
      nc_raster <- rast(Path) 
      # nc_raster
      coord_ref <- st_crs(nc_raster)
      # plot(nc_raster,1)
      esp_can <- esp_get_munic_siane(moveCAN = FALSE)
      esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE))
      esp <- st_transform(esp_can,coord_ref)
      
      # Crop Spain data
      rast_spain <- nc_raster  %>%
        terra::crop(esp)
      
      # plot(rast_spain,2)
      #--- extract values from the raster for each county ---#
      temp_muni <- terra::extract(rast_spain, esp)
      colnames(temp_muni) <- c("ID",as.character(time(nc_raster)))
      temp_muni$tmean <- rowMeans(temp_muni[,2:ncol(temp_muni)])
      #--- get mean tmax ---#
      temp_muni <- temp_muni %>%
        group_by(ID) %>%
        summarize(tmean = mean(tmean))
      
      esp_sf <- st_as_sf(esp) %>%
        mutate(ID := seq_len(nrow(.))) %>%
        left_join(., temp_muni, by = "ID")
      
      df_temp <- as.data.frame(esp_sf[,c("NATCODE", "tmean")])
      df_temp$geometry <- NULL
      df_temp$date <- as.Date(paste0(ye,"-",mon ,"-",k), "%Y-%m-%d")
      
      return(df_temp)
    })
  df_temp <- weather_df_y
  # Save output file
  return(df_temp)
}

# Select the Year that you want the daily temperatures:
year_n = "2011"
month_n = "01"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
               year_n,"_",month_n,".Rds"))

month_n = "02"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "03"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "04"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "05"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "06"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "07"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "08"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "09"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "10"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "11"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "12"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

# Select the Year that you want the daily temperatures:
year_n = "2012"
month_n = "01"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "02"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "03"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "04"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "05"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "06"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "07"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "08"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "09"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "10"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "11"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "12"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

# Select the Year that you want the daily temperatures:
year_n = "2013"
month_n = "01"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "02"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "03"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "04"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "05"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "06"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "07"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "08"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "09"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "10"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "11"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "12"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

# Select the Year that you want the daily temperatures:
year_n = "2014"
month_n = "01"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "02"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "03"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "04"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "05"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "06"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "07"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "08"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "09"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "10"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "11"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "12"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

# Select the Year that you want the daily temperatures:
year_n = "2015"
month_n = "01"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "02"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "03"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "04"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "05"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "06"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "07"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "08"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "09"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "10"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "11"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "12"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

# Select the Year that you want the daily temperatures:
year_n = "2017"
month_n = "01"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "02"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "03"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "04"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "05"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "06"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "07"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "08"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "09"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "10"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "11"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "12"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

# Select the Year that you want the daily temperatures:
year_n = "2018"
month_n = "01"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "02"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "03"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "04"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "05"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "06"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "07"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "08"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "09"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "10"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "11"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "12"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

# Select the Year that you want the daily temperatures:
year_n = "2019"
month_n = "01"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "02"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "03"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "04"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "05"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "06"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "07"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "08"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "09"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "10"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "11"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "12"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

# Select the Year that you want the daily temperatures:
year_n = "2020"
month_n = "01"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "02"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "03"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "04"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "05"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "06"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "07"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "08"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "09"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "10"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "11"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))

month_n = "12"
print("Antes de func")
df_out <- func_weather(month_n, year_n)
print("Despues de func")
saveRDS(df_out,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",
                      year_n,"_",month_n,".Rds"))
# aux_df <- df_out[[1]]
# esp_can
# esp_can <-  esp_can  %>% 
#   left_join(df_out[[1]] )
# ggplot(esp_can) + 
#   geom_sf(aes(fill = tmean), size = 0.1) + 
#   scale_fill_distiller(palette="Spectral")
