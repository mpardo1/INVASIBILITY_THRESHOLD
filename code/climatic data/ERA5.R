rm(list=ls())
library(ncdf4)
library(tidyverse)
library(KrigR)
library(sf) 
library(RColorBrewer)
# Link ERA5 data:
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-cerra-single-levels?tab=overview
# Nice tutorial to extract data from a nc file
# https://towardsdatascience.com/how-to-crack-open-netcdf-files-in-r-and-extract-data-as-time-series-24107b70dcd
# Download data:
# Spain coordinates
lon_min <- -9.28
lon_max <- 3.34
lat_min <- 36.00
lat_max <- 43.82

## Read the file from Copernicus data.
Path = "~/INVASIBILITY_THRESHOLD/data/ERA5/adaptor.mars.external-1680271509.6859422-29720-9-a7d5dfd1-4c9f-4d22-be1b-389929d0079c.nc"
nc <- nc_open(Path)
# Metadata file print, read carefully
print(era5_temp)
era5_temp[[1]]
# Data variables and dimensions
attributes(nc)
attributes(nc$var)
attributes(nc$dim)

x <- nc$dim$x$vals
y <- nc$dim$y$vals

library(mapproj)
latlon <- mapproject(x, y, projection = "+proj=lcc +lon_0=0 +lat_1=60 +lat_2=40 +lat_0=45")
lat <- latlon$y
lon <- latlon$x

# Extract the latitude and longitude data
lat <- ncvar_get(era5_temp, "latitude")
nlat <- length(lat)
lon <- ncvar_get(era5_temp, "longitude")
nlon <- length(lon)

lat_ind <- which(lat>=27 & lat <=43.5)
lon_ind <- which(lon>=-10 & lon <=5.5)

temp <- ncvar_get(era5_temp, "t2m", start = c(1, lat_ind[1], lon_ind[1]),
                  count = c(-1, length(lat_ind), length(lon_ind)))

# Getting the longitude indexes which longitude values are between 140.9 and 149.8 (Victoria State)
lonIdx <- which( era5_temp$dim$lon$vals > lon_min & era5_temp$dim$lon$vals < lon_max) ##
# Reading longitude values within longitute indexes range (lonIdx)
lon <- ncvar_get(era5_temp, "longitude", start=c(min(lonIdx)), count=c((max(lonIdx)-min(lonIdx)+1)))
nlon <- dim(lon)
head(lon)

# Getting the latitude indexes which latitude values are between -39.10 and -34 (Victoria State)
latIdx  <- which( era5_temp$dim$lat$vals > lat_min & era5_temp$dim$lat$vals < lat_max)
# Reading latitude values within this index
lat <- ncvar_get(era5_temp, "latitude", verbose = F, start=c(min(latIdx)), count=c((max(latIdx)-min(latIdx)+1)))
nlat <- dim(lat)
head(lat)
# Extract the time, it is written as second since reference data, 
#look for it in print(era5_temp) in my file: seconds since 1970-01-01.
time <- ncvar_get(era5_temp, "time")
tunits <- ncatt_get(era5_temp, "time", "units")  # check units tiem
nt <- dim(time) 

lswt_array <- ncvar_get(era5_temp, "t2m") 
fillvalue <- ncatt_get(era5_temp, "t2m", "_FillValue")
dim(lswt_array)



# Convert the data to Celsius
temp_mean <- temp_mean - 273.15
temp_max <- temp_max - 273.15
temp_min <- temp_min - 273.15