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
