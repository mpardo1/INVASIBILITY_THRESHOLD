rm(list=ls())
library("mapSpain")
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
library(parallel)

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()

#------------------------FUNCTIONS---------------------------#
# Main functions 
Briere_func <- function(cte, tmin, tmax, temp){
  outp <- temp*cte*(temp - tmin)*(tmax - temp)^(1/2)
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

Quad_func <- function(cte, tmin, tmax, temp){
  outp <- -cte*(temp - tmin)*(temp - tmax)
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}


#### -------------------------- Albopictus ------------------------- ####
## Thermal responses Aedes Albopictus from Mordecai 2017:
a_f_alb <- function(temp){Briere_func(0.000193,10.25,38.32,temp)} # Biting rate
TFD_f_alb <- function(temp){Briere_func(0.0488,8.02,35.65,temp)} # Fecundity
pEA_f_alb <- function(temp){Quad_func(0.00361,9.04,39.33,temp)} # Survival probability Egg-Adult
MDR_f_alb <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_f_alb <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span



### Incorporating rain and human density:
h_f <- function(hum, rain){
  # Constants: 
  erat = 0.5
  e0 = 1.5
  evar = 0.05
  eopt = 8
  efac = 0.01
  edens = 0.01
  
  
  hatch <- (1-erat)*(((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) + e0)) +
    erat*(edens/(edens + exp(-efac*hum)))
  return(hatch)
}

# R0 function by temperature:
R0_func_alb <- function(rain,hum,Te){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  deltaa <- 1/lf_f_alb(Te)
  probla <- pEA_f_alb(Te)
  h <- h_f(hum,rain)
  deltE = 0.1
  R0 <- sqrt(f*(a/deltaa)*probla*(h*(h+deltE)))
  return(R0)
}


# Population density in each municipality.
census <- mapSpain::pobmun19
esp_can_pop <- esp_can %>% left_join(census, by = c("cmun" = "cmun","cpro" = "cpro"))
esp_can_pop$area <- as.numeric(st_area(esp_can_pop))/1000000
esp_can_pop$pop_km <- esp_can_pop$pob19/esp_can_pop$area

#####
list_files <- list.files("~/INVASIBILITY_THRESHOLD/output/weather/Daily/")
ind <- which(list_files %like% ".Rds")
list_files <- list_files[ind]
Cores = 2
for(i in c(1:length(list_files))){ 
  Path <- paste0("~/INVASIBILITY_THRESHOLD/output/weather/Daily/", list_files[i])
  print(paste0("File:",Path))
  weather <- readRDS(Path)
  if((list_files[i] %like% "Marz")){
    print("Marz or _2_")
    weather_dt <- setDT(weather) # Convert data.frame to data.table
    rm(weather)
    weather_dt$month <- lubridate::month(weather_dt$fecha)
    weather_dt$year <- lubridate::year(weather_dt$fecha)
    # weather_dt <- weather_dt[ , .(tmed = mean(tmed),
    #                               tmin = min(tmin),
    #                               tmax = max(tmax),
    #                               precmed = mean(precmed)),by = list(month,year,name)] 
    weather_dt <- weather_dt %>% left_join(esp_can_pop, by = c("name" = "name.x"))
    weather_dt$NATCODE <- as.numeric(paste0("34",weather_dt$codauto,weather_dt$cpro,weather_dt$LAU_CODE))
    weather_dt <- weather_dt[,c(1:7,20,22,23)]
  }else{
    print("else")
    weather_df <- as.data.frame(do.call(rbind, weather))
    weather_dt <- setDT(weather_df) # Convert data.frame to data.table
    colnames(weather_dt)[1] <- "NAMEUNIT"
    rm(weather_df)
    rm(weather)
    weather_dt$month <- lubridate::month(weather_dt$fecha)
    weather_dt$year <- lubridate::year(weather_dt$fecha)
    # weather_dt <- weather_dt[ , .(tmed = mean(tmed),
    #                               tmin = min(tmin),
    #                               tmax = max(tmax),
    #                               precmed = mean(precmed)),by = list(month,year,NAMEUNIT)] 
    weather_dt <- weather_dt %>% left_join(esp_can_pop, by = c("NAMEUNIT" = "name.x"))
    weather_dt$NATCODE <- as.numeric(paste0("34",weather_dt$codauto,weather_dt$cpro,weather_dt$LAU_CODE))
    weather_dt <- weather_dt[,c(1:7,19,21,22)]
  }
  
  print("After if")
  weather_dt$R0_tmin <- 0
  weather_dt$R0_tmax <- 0
  weather_dt$R0_tmed <- 0
  
  ## Function to read all output weather file compute R0 and create a list of df.
  print("Before loop")
  for(k in c(1:nrow(weather_dt))){
    print(paste0("K:",k))
    weather_dt$R0_tmin[k] <- R0_func_alb(weather_dt$precmed[k],
                                         weather_dt$pop_km[k],weather_dt$tmin[k])
    weather_dt$R0_tmax[k] <- R0_func_alb(weather_dt$precmed[k],
                                         weather_dt$pop_km[k],weather_dt$tmax[k])
    weather_dt$R0_tmed[k] <- R0_func_alb(weather_dt$precmed[k],
                                         weather_dt$pop_km[k],weather_dt$tmed[k])
  }
  
  print("After loop")
  Path <- paste0("~/INVASIBILITY_THRESHOLD/output/weather/Daily/rainfall/daily_rainf_", list_files[i])
  saveRDS(weather_dt,Path)
  print("After saving")
  rm(weather_dt)
}

