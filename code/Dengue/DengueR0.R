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
b_f_alb <- function(temp){Briere_func(0.000723,15.84,36.4,temp)} # Adult life span
c_f_alb <- function(temp){Briere_func(0.000439,3.62,36.82,temp)} # Adult life span
pdr_f_alb <- function(temp){Briere_func(0.000109,10.39,43.05,temp)} # Adult life span

# R0 function by temperature:
R0_func_alb <- function(Te, hum){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  mu <- 1/lf_f_alb(Te)
  mdr <- MDR_f_alb(Te)
  probla <- pEA_f_alb(Te)
  b <- b_f_alb(Te)
  c <- c_f_alb(Te)
  pdr <- pdr_f_alb(Te)
  N <- hum
  r = 0.1
  R0 <- sqrt((a^2*b*c*exp(-mu/pdr)*f*probla*mdr)/(N*r*mu^3))
  
  return(R0)
}

# R0 for Dengue
vec <- seq(10,40,0.01)
hum_cte <- 0.1
te_cte <- 15
out <- sapply(vec,R0_func_alb,hum=hum_cte)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

max(out)

# Population density in each municipality.
census <- mapSpain::pobmun19
esp_can_pop <- esp_can %>% left_join(census, by = c("cmun" = "cmun","cpro" = "cpro"))
esp_can_pop$area <- as.numeric(st_area(esp_can_pop))/1000000
esp_can_pop$pop_km <- esp_can_pop$pob19/esp_can_pop$area
esp_can_pop$pop_km[which(is.na(esp_can_pop$pop_km))] <- 10
esp_can_pop$NATCODE <- as.numeric(paste0("34",
                                         esp_can_pop$codauto,
                                         esp_can_pop$cpro,
                                         esp_can_pop$LAU_CODE))

#####
year = "2019"
Path = paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/",year,"/")
listfile <- list.files(Path)
weather_t <- data.table()
for(file in listfile){
  Path1 <- paste0(Path,file)
  weather <- readRDS(Path1)
  weather_df <- as.data.frame(do.call(rbind, weather))
  weather_dt <- setDT(weather_df) # Convert data.frame to data.table
  rm(weather,weather_df)
  weather_t <- rbind(weather_t,weather_dt)
  
}

esp_can_pop <- esp_can_pop[,c("NATCODE","pob19")]
esp_can_pop$geometry <- NULL
head(esp_can_pop)
head(weather_t)
weather_t <- weather_t %>% left_join(esp_can_pop)
weather_t$R0 <- 0
weather_t <- setDT(weather_t)

## Function to read all output weather file compute R0 and create a list of df.
num_cores <- 12
df_chunks <- split(weather_t, 0:(nrow(weather_t) - 1) %% num_cores)
modify_column <- function(chunk) {
  # Modify the values in the desired column
  for (j in c(1:nrow(chunk))){ 
    print(paste0("j:",j))
    chunk$R0[j] <- R0_func_alb(chunk$tmean[j],
                               chunk$pob19[j])
    
  }
  
  return(chunk)
}
modified_chunks <- mclapply(df_chunks, modify_column, mc.cores = num_cores)
DengueR0 <- do.call(rbind, modified_chunks)

saveRDS(DengueR0,
        "~/INVASIBILITY_THRESHOLD/output/R0/R0_Dengue_2019.Rds")


## Read Zivko file Spain
Path <- "~/INVASIBILITY_THRESHOLD/code/Dengue/gadm_410_transformed_spain.gpkg"
spain <- st_read(Path)
spain_ine <- st_read("~/INVASIBILITY_THRESHOLD/data/municipios_sh/Municipios_IGN.shp")
