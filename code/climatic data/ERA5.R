rm(list=ls())
library(ncdf4)
library(tidyverse)
library(sf) 
library(RColorBrewer)
library(raster)
library(terra)
library(geodata)
library(exactextractr)
library("mapSpain")
library(reticulate)
library(ncdf4)
# Link ERA5 data:
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-cerra-single-levels?tab=overview
#install the CDS API
conda_install("r-reticulate","cdsapi", pip=TRUE)#import python CDS-API
cdsapi <- import('cdsapi')
#for this step there must exist the file .cdsapirc
server = cdsapi$Client() #start the connection
#we create the query
query <- r_to_py(list(
  variable= "2m_temperature",
  level_type= "surface_or_atmosphere",
  data_type= "reanalysis",
  product_type= "analysis",
  year= '2020',
  month= '01', 
  day= '01',
   time= c('00:00', '03:00', '06:00',
            '09:00', '12:00', '15:00',
            '18:00', '21:00'),
   
   format= 'grib'
))

#query to get the ncdf
server$retrieve("reanalysis-era5-single-levels",
                query,
                "era5_ta_2018.grib")

## Read the file from Copernicus data.
Path = "era5_ta_2018.grib"
nc_raster <- rast(Path) 
coord_ref <- st_crs(nc_raster)
plot(nc_raster,1)
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
esp <- st_transform(esp_can,coord_ref)

# Crop Spain data
rast_spain <- nc_raster  %>%
  terra::crop(esp)

plot(rast_spain,1)
#--- extract values from the raster for each county ---#
temp_muni <- terra::extract(rast_spain, esp)
colnames(temp_muni) <- c("ID",as.Date(time(nc_raster)))

#--- get mean tmax ---#
temp_muni <-
  temp_muni %>%
  group_by(ID) %>%
  summarize(tmean = mean(temp))


esp_sf <- st_as_sf(esp) %>%
   mutate(ID := seq_len(nrow(.))) %>%
   left_join(., temp_muni, by = "ID")

df_temp <- as.data.frame(esp_sf[,c("name", "tmean")])
df_temp$geometry <- NULL
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can <-  esp_can  %>% 
  left_join(df_temp)

ggplot(esp_can) + 
  geom_sf(aes(fill = tmean), size = 0.1) + 
  scale_fill_distiller(palette="Spectral")
