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
cdsapi <- import('cdsapi')
#for this step there must exist the file .cdsapirc
server = cdsapi$Client() #start the connection
#we create the query
day_n = "01"
month_n = "01"
year_n = "2021"
query <- r_to_py(list(
  variable= "2m_temperature",
  level_type= "surface_or_atmosphere",
  data_type= "reanalysis",
  product_type=" analysis",
  year= year_n,
  month= month_n,
  day= day_n,
  time=c("00:00", "03:00", "06:00",
    "09:00", "12:00", "15:00",
    "18:00", "21:00"),
  format= "grib"
))


#query to get the ncdf
Path = paste0("era5_temp_",year_n,month_n, day_n,".grib")
server$retrieve("reanalysis-cerra-single-levels",
                query,
                Path)

## Read the file from Copernicus data.
nc_raster <- rast(Path) 
# nc_raster
coord_ref <- st_crs(nc_raster)
# plot(nc_raster,1)
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
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

df_temp <- as.data.frame(esp_sf[,c("name", "tmean")])
df_temp$geometry <- NULL
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can <-  esp_can  %>% 
  left_join(df_temp)
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE))
esp_can$geometry <- NULL
df_temp <- esp_can[,c("NATCODE","tmean")]
saveRDS(df_temp,paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_",year_n,".Rds"))
# ggplot(esp_can) + 
#   geom_sf(aes(fill = tmean), size = 0.1) + 
#   scale_fill_distiller(palette="Spectral")
