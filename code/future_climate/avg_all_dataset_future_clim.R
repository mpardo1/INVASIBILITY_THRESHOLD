# Code to average all the tiff files from future climate projections for 
# each dataset.
# Each dataset is download from the following code:future_climate_tmin.R, future_climate_tmax.R, future_climate_prec.R
rm(list = ls())
library(terra)

var = "tmin"
time = "2041-2060"
avg_rast <- function(var, time){
  path <- paste0("~/future_climate/",var,"/",time,"/")
  list_files <- list.files(path)
  rast_mean <- rast(paste0(path,list_files[1]))
  for(i in c(2:24)){
    print(paste0("i:",i))
    rast_aux <- rast(paste0(path,list_files[i]))
    rast_mean <- mean(rast_mean, rast_aux)
  }
  writeRaster(rast_mean,paste0("~/INVASIBILITY_THRESHOLD/data/future-climate/",
                               var,"_mean",time,".tif")) 
}

var = "tmin"
time = "2041-2060"
avg_rast(var,time)

var = "tmin"
time = "2061-2080"
avg_rast(var,time)

var = "tmax"
time = "2041-2060"
avg_rast(var,time)

var = "tmax"
time = "2061-2080"
avg_rast(var,time)

var = "prec"
time = "2041-2060"
avg_rast(var,time)

var = "prec"
time = "2061-2080"
avg_rast(var,time)
