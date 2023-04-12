rm(list=ls())
#install the RNCEP, lubridate and tidyverse packages
if(!require("RNCEP")) install.packages("RNCEP")
if(!require("lubridate")) install.packages("lubridate")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")

#load the packages
library(RNCEP)
library(lubridate) #date and time manipulation
library(tidyverse) #data manipulation and visualization
library(RColorBrewer) #color schemes
library(sf) #to import a spatial object and to work with geom_sf in ggplot2

# Spain coordinates
lon_min <- -9.28
lon_max <- 3.34
lat_min <- 36.00
lat_max <- 43.82

#define the necessary arguments
month_range <- c(1,1)     #period of months
year_range <- c(2016,2016) #period of years

lat_range <- c(lat_min,lat_max)      #latitude range
lon_range <- c(lon_min,lon_max)     #longitude range

## Download data
data <-  NCEP.gather(variable='air.2m', level='gaussian',
                     months.minmax=month_range, years.minmax=year_range,
                     lat.southnorth=lat_range, lon.westeast=lon_range,
                     reanalysis2 = TRUE, return.units = TRUE)

# Check dimensions and times.
date_time <- dimnames(data)[[3]]
date_time <- ymd_h(date_time)
head(date_time)

lat <- dimnames(data)[[1]]
lon <- dimnames(data)[[2]]

head(lat);head(lon)

# Analize data strucure:
str(data)

# Group by day
group <- day(date_time)
data_day <- aperm(
  apply(
    data,
    c(1,2),
    by,
    group,
    function(x)ifelse(all(is.na(x)),NA,mean(x))),
                      c(2,3,1))

head(data_day)
str(data_day)
data_day

#first we create all the combinations of lon-lat
lonlat <- expand.grid(lon=lon,lat=lat)

#as lonlat was a row/column name, it is character, that's why we convert it into numeric
lonlat <- apply(lonlat,2,as.numeric)

#lon and lat are not in the order as we expect
#row=lon; column=lat
data_day <- aperm(data_day,c(2,1,3))

#subtract 273.15K to convert K to ºC.
df <- data.frame(lonlat,
                 Ta01=as.vector(data_day[,,1])-273.15,
                 Ta07=as.vector(data_day[,,7])-273.15)

if(!require("reticulate")) install.packages("reticulate")
if(!require("ncdf4")) install.packages("ncdf4") #to manage netCDF format

#load packages
library(reticulate)
library(ncdf4)
py_install("ecmwf-api-client")
setwd("~/")

ecmwf <- import("ecmwfapi")
server = ecmwf$ECMWFDataServer()
query <-r_to_py(list(
  'class'="ei",
  'dataset'= "interim", #dataset
  'date'= "2017-01-01/to/2017-12-31", #time period
  'expver'= "1",
  'grid'= "0.125/0.125", #resolution
  'levtype'="sfc",
  'param'= "167.128", # air temperature (2m)
  'area'="45/-10/30/5", #N/W/S/E
  'step'= "0",
  'stream'="oper",
  'time'="00:00:00/06:00:00/12:00:00/18:00:00", #hours
  'type'="an",
  'format'= "netcdf", #format
  'target'='ta2017.nc' #file name
))
server$retrieve(request)
