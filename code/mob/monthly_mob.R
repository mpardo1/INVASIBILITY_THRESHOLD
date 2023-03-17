rm(list=ls())
# .rs.restartR()
library(readxl)
library(tidyverse)
# library(comperes)
library(sf)

wd <- "~/INVASIBILITY_THRESHOLD/data/"
setwd(wd)

Path <- "~/INVASIBILITY_THRESHOLD/data/mob/June/June/"
list_file <- list.files(Path)

for(i in c(1:length(list_file))){
  ##### MITMA #####
  Path <- paste0("~/INVASIBILITY_THRESHOLD/data/mob/June/June/", list_file[i])
  print(paste0("File:",list_file[i]))
  muni_ref_17 <- as.data.frame(read.csv(Path, sep = "|"))
  muni_ref_17 <- muni_ref_17[,c("origen", "destino", "viajes", "viajes_km")] %>%
    group_by(origen, destino) %>%
    summarise(viajes = sum(viajes),viajes_mean = mean(viajes),
              viajes_km = sum(viajes_km), viajes_km_mean = mean(viajes_km)) 
  
  print("Dentro del mclapply")
  if(exists('monthly_mob') && is.data.frame(get('monthly_mob'))){
    monthly_mob <- rbind(monthly_mob,muni_ref_17)
    monthly_mob <- monthly_mob[,c("origen", "destino", "viajes", "viajes_km", 
                                  "viajes_mean","viajes_km_mean")] %>%
      group_by(origen, destino) %>%
      summarise(viajes = sum(viajes),viajes_mean = mean(viajes_mean),
                viajes_km = sum(viajes_km), viajes_km_mean = mean(viajes_km_mean)) 
  }else{
    monthly_mob <- muni_ref_17
  }
}

# sum over período for daily quantities

write_rds(monthly_mob, paste0("~/INVASIBILITY_THRESHOLD/output/mob/mobility_June.Rds"))
