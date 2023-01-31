library("mapSpain")
library(tidyverse)
library(sf)

# Read temperature John File.
Path <- "/home/marta/Documentos/PHD/2022/INVASIBILY/OUTPUT/aemet_weather_daily_deep_history_sf.Rds"
df <- readRDS(Path)
Path <- "/home/marta/Documentos/PHD/2022/INVASIBILY/OUTPUT/aemet_weather_daily_deep_history.Rds"
df <- readRDS(Path)

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$R0 <- runif(length(esp_can$codauto),0,5)
can_box <- esp_get_can_box()

ggplot(esp_can) +
  geom_sf(aes(fill = R0), size = 0.1) +
  geom_sf(data = can_box) + theme_bw()

# Path <- "/home/marta/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_peninbal_etrs89.shp"
# df_muni <- read_sf(Path)
Path <- "/home/marta/Documentos/PHD/2022/INVASIBILY/Municipios_Data/Municipios_IGN.shp"
df_muni <- read_sf(Path)

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

## Thermal responses Aedes Albopictus from Mordecai 2017:
a_f <- function(temp){Briere_func(0.000193,10.25,38.32,temp)} # Biting rate
TFD_f <- function(temp){Briere_func(0.0488,8.02,35.65,temp)} # Fecundity
pEA_f <- function(temp){Quad_func(0.00361,9.04,39.33,temp)} # Survival probability Egg-Adult
MDR_f <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_f <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span

# R0 function by temperature:
R0_func <- function(Te){
  a <- a_f(Te)
  f <- TFD_f(Te)
  deltaa <- 1/lf_f(Te)
  probla <- pEA_f(Te)
  R0 <- sqrt(f*(a/deltaa)*probla)
  return(R0)
}

Path <- "~/INVASIBILITY_THRESHOLD/data/aemet_weather_daily_deep_history_sf_2023-01-31.Rds"
weather <- readRDS(Path)
