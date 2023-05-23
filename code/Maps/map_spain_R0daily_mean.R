rm(list=ls())
library(mapSpain)
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library("data.table")
library(tidyr)

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
pLA_f_alb <- function(temp){Quad_func(2.663e-03,6.668e+00,3.892e+01,temp)} # Survival probability Egg-Adult
MDR_f_alb <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_f_alb <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span
dE_f_alb <- function(temp){4.66e-03*temp -4.23e-02} # Adult life span

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
esp_can_pop$NATCODE <- as.numeric(paste0("34",
                                         esp_can_pop$codauto,
                                         esp_can_pop$cpro,
                                         esp_can_pop$LAU_CODE))
esp_can_pop <- esp_can_pop[,c("pop_km","NATCODE")]
esp_can_pop$geometry <- NULL

#####
# Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/R0_aemet_weather_year_2_22.Rds"
R0_monthly <- function(year){
  Path = paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/",year,"/")
  listfile <- list.files(Path)
  Path2 = paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/rainfall/",year,"/")
  listfile2 <- list.files(Path2)
  weather_t <- data.table()
  for(i in c(1:12)){
    Path <- paste0(Path,listfile[i])
    weather <- readRDS(Path)
    weather_df <- as.data.frame(do.call(rbind, weather))
    weather_dt <- setDT(weather_df) # Convert data.frame to data.table
    rm(weather,weather_df)
   
    Path <- paste0(Path,listfile2[i])
    weather <- readRDS(Path)
    weather_df <- reshape2::melt(weather,id.vars = "NATCODE")
    colnames(weather_df) <- c("NATCODE", "date", "precmed")
    weather_df$date <- as.Date(weather_df$date, "%Y-%m-%d" )
    weather_dr <- setDT(weather_df) # Convert data.frame to data.table
    rm(weather,weather_df)
    weather_dt <- weather_dt %>% left_join(weather_dr, by = c("NATCODE", "date"))
    weather_r <- rbind(weather_r,weather_dt)
    rm(weather_dr)
  }
  
  weather <- weather_r %>% left_join(esp_can_pop, 
                                     by = c("NATCODE"))
  weather$R0 <- 0
  
  Cores = 10
  weather_df_y <- mclapply(1:nrow(weather), mc.cores = Cores, mc.preschedule = F,function(i){ 
    print(paste0("i:",i))
    weather$R0[i] <- R0_func_alb(weather$precmed[i],
                                     weather$pop_km[i], 
                                     weather$tmean[i])
  })
  return(weather_df_y)
}

year_n = "2020"
R0mon <- R0_monthly(year_n)
saveRDS(R0mon,"~/INVASIBILITY_THRESHOLD/output/R0/R0_ERA5_daily_2020.Rds")

