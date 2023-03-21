rm(list=ls())
library("mapSpain")
library(tidyverse)
library(sf)
library(pollen)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gganimate)
library(rmapshaper)
library("ggpubr")
library(viridis)
library(stringr)
library(gdata)
library("data.table")
library("zoo")
require("chillR")

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$R0_test <- runif(length(esp_can$codauto),0,5)
can_box <- esp_get_can_box()

ggplot(esp_can) +
  geom_sf(aes(fill = R0_test), size = 0.1) +
  geom_sf(data = can_box) + theme_bw()

# Path <- "/home/marta/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_peninbal_etrs89.shp"
# df_muni <- read_sf(Path)
# Path <- "/home/marta/Documentos/PHD/2022/INVASIBILY/Municipios_Data/Municipios_IGN.shp"
# df_muni <- read_sf(Path)

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


vec <- seq(0,40,1)
out_f_alb <- sapply(vec,a_f_alb)
out_TFD_alb <- sapply(vec,TFD_f_alb)
df_alb <- data.frame(Temp = vec, out_f_alb,out_TFD_alb, EFD_alb =out_f_alb*out_TFD_alb)
plot_EFD_alb <- ggplot(df_alb) + 
  geom_line(aes(Temp,EFD_alb))

ggplot(df_alb) + 
  geom_line(aes(vec,out_TFD_alb))
ggplot(df_alb) + 
  geom_line(aes(vec,out_f_alb))

# R0 function by temperature:
R0_func_alb <- function(Te){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  deltaa <- 1/lf_f_alb(Te)
  probla <- pEA_f_alb(Te)
  R0 <- sqrt(f*(a/deltaa)*probla)
  return(R0)
}

vec <- seq(0,40,1)
out_R0_alb <- sapply(vec,R0_func_alb)
df_alb <- data.frame(Temp = vec, out_R0_alb)
plot_EFD_alb <- ggplot(df_alb) + 
  xlab("Temperature") + ylab("R0, suitability index") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_line(aes(Temp,out_R0_alb)) + theme_bw()
plot_EFD_alb

# Read the weather data for a specific month and year for all municipalities
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/Daily/aemet_weather_year_whole_7.Rds"
weather <- readRDS(Path)
weather_df <- as.data.frame(do.call(rbind, weather))
colnames(weather_df) <- c("muni_name", "ccaa_name", "prov_name",
                          "date", "tmin", "tmax", "tmed", "precmed", "n")

# Merge the municipalities shapefile with the weather data:
weather_df$day <- lubridate::day(weather_df$date)
weather_df$month <- lubridate::month(weather_df$date)
weather_df$year <- lubridate::year(weather_df$date)
weather_df$R0_tmin <- sapply(weather_df$tmin, R0_func_alb)
weather_df$R0_tmed <- sapply(weather_df$tmed, R0_func_alb)
weather_df$R0_tmax <- sapply(weather_df$tmax, R0_func_alb)

weather_municip_R01_dt <- setDT(weather_df) # Convert data.frame to data.table
plot_df_filt <- weather_municip_R01_dt[which((weather_municip_R01_dt$muni_name == "Barcelona") ),]
plot_df_filt$R0_rollmean <- 0
ind = 20
len <- nrow(plot_df_filt) - ind
for(i in c(1:len)){
  plot_df_filt$R0_rollmean[i] <- mean(plot_df_filt$R0_tmed[i:(i+ind)])
}

ggplot(plot_df_filt, aes(x = date)) + 
  geom_line(aes(y = R0_rollmean), color=temperatureColor) + 
  geom_line(aes(y = tmed), color="#0072B2", linetype = "dotted") + 
  ylab("R0") +
  scale_y_continuous(name = "R0",
    sec.axis = sec_axis(~.*coeff, name="Average temperature (Cº)"))  + 
  theme_bw() +
  theme(text = element_text(size = 15)) +
  scale_x_date(date_breaks = "1 month",date_labels = "%b")

require(chillR)
all_daylengths<-cbind(JDay=1:365,sapply(daylength(latitude=41.39,JDay=1:365),cbind))
knitr::kable(head(all_daylengths))
weather_h <- unique(plot_df_filt[,c("year","month","day", "tmax", "tmin")])
colnames(weather_h) <- colnames(KA_weather)

hourtemps<-stack_hourly_temps(weather_h, latitude=41.39)$hourtemps
hourtemps$R0_tmed <- sapply(hourtemps$Temp, R0_func_alb)
hourtemps$date <- as.Date(paste0(hourtemps$Day,"/",hourtemps$Month,"/",hourtemps$Year), "%d/%m/%y")
hourtemps <- hourtemps %>% group_by(date) %>%
  summarise(avg_R0 = mean(R0_tmed), avg_temp = mean(Temp))

temperatureColor <- "#E69F00"
priceColor <- rgb(0.2, 0.6, 0.9, 1)
coeff = 1
ggplot(hourtemps, aes(x = date)) + 
  geom_line(aes(y = avg_R0), color=temperatureColor) + 
  geom_line(aes(y = avg_temp), color="#0072B2", linetype = "dotted") + 
  ylab("R0") +
   scale_y_continuous(
    # Features of the first axis
    name = "R0",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Average temperature (Cº)")
  )  + 
  theme_bw() +
   theme(text = element_text(size = 15)
  ) +
  scale_x_date(date_breaks = "1 month",date_labels = "%b")

