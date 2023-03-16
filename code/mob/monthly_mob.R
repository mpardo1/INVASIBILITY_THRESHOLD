rm(list=ls())
.rs.restartR()
library(readxl)
library(tidyverse)
library(comperes)
library(sf)
library(mapSpain)

wd <- "~/INVASIBILITY_THRESHOLD/data/"
setwd(wd)

Path <- "/home/marta/INVASIBILITY_THRESHOLD/data/mob/June/"
list_file <- list.files(Path)

for(i in c(1:2)){
  ##### MITMA #####
  Path <- paste0("~/INVASIBILITY_THRESHOLD/data/mob/June/", list_file[i])
  print(paste0("File:",list_file[i]))
  muni_ref_17 <- as.data.frame(read.csv(Path, sep = "|"))
  
  print("Dentro del mclapply")
  if(exists('monthly_mob') && is.data.frame(get('monthly_mob'))){
    monthly_mob <- rbind(monthly_mob,muni_ref_17)
  }else{
    monthly_mob <- muni_ref_17
  }
}
# filter for Girona. 67 total dists, 63 total munis (+2 missing)
monthly_mob_group <- monthly_mob[,c("origen", "destino", "viajes", "viajes_km")] %>%
  group_by(origen, destino) %>%
  summarise(viajes = sum(viajes),viajes_mean = mean(viajes),
            viajes_km = sum(viajes_km), viajes_km_mean = mean(viajes_km)) 
# sum over período for daily quantities

write_rds(monthly_mob_group, paste0("~/INVASIBILITY_THRESHOLD/output/mob/mobility_June.Rds"))
