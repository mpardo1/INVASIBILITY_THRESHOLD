# code to validate the PA for japonicus in europe
rm(list=ls())
library(sf)
library(ggplot2)

# read presence absence data ------------------------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/status_2303.shp"
pa_jap <- read_sf(path)
pa_jap <- pa_jap[which(pa_jap$leave == 1),]

ggplot(pa_jap) +
  geom_sf(aes(fill = japonicus)) 

# load data for RM Japonicus Europe 2020
clim_pop <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/EU_R0_",2020,".Rds"))

# transform crs raster
coord_ref <- st_crs(clim_pop)
pa <- st_transform(pa_jap, coord_ref)