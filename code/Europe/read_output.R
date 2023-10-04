rm(list=ls())
library(terra)
rast_list <- rast("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/3monthly_aggregated_raster.tif")
plot(rast_list[[1]])
