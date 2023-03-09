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
  (1-erat)*(((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) + e0)) +
    erat*(edens/(edens + exp(-efac*hum)))
}

# Constants: 
erat = 0.5
e0 = 1.5
evar = 0.05
eopt = 8
efac = 0.01
edens = 0.01
deltE = 0.1
# R0 function by temperature:
R0_func_alb <- function(rain,hum,Te){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  deltaa <- 1/lf_f_alb(Te)
  probla <- pEA_f_alb(Te)
  h <- h_f(hum,rain)
  R0 <- sqrt(f*(a/deltaa)*probla*(h*(h+deltE)))
  return(R0)
}

#####
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/aemet_weather_year_Marz_1.Rds"
weather <- readRDS(Path)
weather_df <- as.data.frame(do.call(rbind, weather))
weather_df <- weather_df %>% left_join(esp_can_pop, by = c("name" = "name.x"))
weather_df <- weather_df[,c(1:8,10,12,15,19,22:24)]
colnames(weather_df) <- c("name", "ccaa_name", "prov_name",colnames(weather_df)[4:ncol(weather_df)])
weather_df$R0_tmin <- 0
weather_df$R0_tmax <- 0
weather_df$R0_tmed <- 0
weather_dt <- setDT(weather_df)
## Function to read all output weather file compute R0 and create a list of df.
Cores <- 10
weather_df_R0 <- mclapply(1:nrow(weather_dt), mc.cores = Cores, mc.preschedule = F,function(i){ 
  print(paste0("i:",i))
  weather_dt$R0_tmin[i] <- R0_func_alb(weather_dt$precmed[i],weather_dt$pop_km[i],weather_dt$tmin[i])
  weather_dt$R0_tmax[i] <- R0_func_alb(weather_dt$precmed[i],weather_dt$pop_km[i],weather_dt$tmax[i])
  weather_dt$R0_tmed[i] <- R0_func_alb(weather_dt$precmed[i],weather_dt$pop_km[i],weather_dt$tmed[i])
  weather_dt
})

weather_dfs <- weather_df_R0
saveRDS(weather_dfs,"~/INVASIBILITY_THRESHOLD/output/weather/Daily/weather_out_R0.Rds")


