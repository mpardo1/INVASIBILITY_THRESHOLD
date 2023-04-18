rm(list=ls())
library("mapSpain")
library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library("ggpubr")
library(viridis)
library(stringr)
library(gdata)
library("data.table")
# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$R0_test <- runif(length(esp_can$codauto),0,5)
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

# R0 function by temperature:
R0_func_alb <- function(Te){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  deltaa <- 1/lf_f_alb(Te)
  probla <- pEA_f_alb(Te)
  R0 <- sqrt(f*(a/deltaa)*probla)
  return(R0)
}

# Read the weather data for a specific month and year for all municipalities
list_files <- list.files("~/INVASIBILITY_THRESHOLD/output/weather/Daily/")
for(i in c(1:length(list_files))){
  Path <- paste0("~/INVASIBILITY_THRESHOLD/output/weather/Daily/", list_files[i])
  print(paste0("File:",Path))
  weather <- readRDS(Path)
  weather_df <- as.data.frame(do.call(rbind, weather))
  
  ## Function to read all output weather file compute R0 and create a list of df.  
  weather_df$R0_tmin <- sapply(weather_df$tmin, R0_func_alb)
  weather_df$R0_tmed <- sapply(weather_df$tmed, R0_func_alb)
  weather_df$R0_tmax <- sapply(weather_df$tmax, R0_func_alb)
  
  saveRDS(weather_df,  paste0("~/INVASIBILITY_THRESHOLD/output/weather/Daily/R0_", list_files[i]))
}
