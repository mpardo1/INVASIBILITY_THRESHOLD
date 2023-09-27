# code to validate the PA for japonicus in europe
rm(list=ls())

# read presence absence data ------------------------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/status_2303.shp"
pa_jap <- read_sf(path)[Which(pa_)]
pa_jap <- pa_jap[which(pa_jap$leave == 1),]