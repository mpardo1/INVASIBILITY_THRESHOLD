rm(list = ls())
library(terra)

path <- "~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/cerra_2020_TEMP.grib"
temp_eu <- rast(path)
temp_eu_m <- tapp(temp_eu, "months", fun=mean)

saveRDS(temp_eu_m,"~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/cerra_2020_TEMP.Rds" )