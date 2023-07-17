rm(list=ls())
library(mapSpain)
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library("ggpubr")
library(viridis)
library(stringr)
library(gdata)
library("data.table")
library("plot3D")
library(httr)
library(tidyr)
library(jsonlite)

# Population density Europe
Path <- "~/Documentos/PHD/2023/INVASIBILITY/data/ESTAT_Census_2011_V1-0.gpkg"
pop_dens <- st_read(Path)
ggplot(pop_dens) + 
  geom_sf(aes(fill=OBS_VALUE_T))
