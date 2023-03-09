rm(list=ls())
library("mapSpain")
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gganimate)
library("ggpubr")
library(viridis)
library(stringr)
library(gdata)
library("data.table")
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

vec <- seq(0,30,0.01)
hum_cte <- 200
te_cte <- 15
out <- sapply(vec,R0_func_alb,hum=hum_cte, Te=te_cte)

# df_out <- data.frame(vec, out)
# ggplot(df_out) + 
#   geom_line(aes(vec,out))
# 
# out <- sapply(vec,h_f,hum=hum_cte)
# 
# df_out <- data.frame(vec, out)
# ggplot(df_out) + 
#   geom_line(aes(vec,out))


# Population density in each municipality.
census <- mapSpain::pobmun19
esp_can_pop <- esp_can %>% left_join(census, by = c("cmun" = "cmun","cpro" = "cpro"))
esp_can_pop$area <- as.numeric(st_area(esp_can_pop))/1000000
esp_can_pop$pop_km <- esp_can_pop$pob19/esp_can_pop$area

# ggplot(esp_can_pop) +
#   geom_sf(aes(fill = area), size = 0.1) +
#   scale_fill_viridis(name = "area") +
#   geom_sf(data = can_box) + theme_bw()
# 
# ggplot(esp_can_pop) +
#   geom_sf(aes(fill = pob29), size = 0.1) +
#   scale_fill_viridis(name = "Population") +
#   geom_sf(data = can_box) + theme_bw()
# 
# ggplot(esp_can_pop) +
#   geom_sf(aes(fill = pop_km), size = 0.1) +
#   scale_fill_viridis(name = "Population per Km2") +
#   geom_sf(data = can_box) + theme_bw()

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
## Function to read all output weather file compute R0 and create a list of df.
weather_dt <- setDT(weather_df)
for(i in c(1:nrow(weather_df))){
  print(paste0("i:",i))
  weather_dt$R0_tmin[i] <- R0_func_alb(weather_dt$precmed[i],weather_dt$pop_km[i],weather_dt$tmin[i])
  weather_dt$R0_tmax[i] <- R0_func_alb(weather_dt$precmed[i],weather_dt$pop_km[i],weather_dt$tmax[i])
  weather_dt$R0_tmed[i] <- R0_func_alb(weather_dt$precmed[i],weather_dt$pop_km[i],weather_dt$tmed[i])
}

saveRDS(weather_dt,"~/INVASIBILITY_THRESHOLD/output/weather/Daily/weather_out_1R0.Rds")
# 
# weather_df$R0_tmin <- sapply(weather_df$precmed, R0_func_alb,
#                              hum = weather_df$pop_km, Te = weather_df$tmin)
# weather_df$R0_tmed <- sapply(weather_df$precmed, R0_func_alb)
# weather_df$R0_tmax <- sapply(weather_df$precmed, R0_func_alb)

# colnames(esp_can) <- c(colnames(esp_can)[1:5], "muni_name",colnames(esp_can)[7:length(colnames(esp_can))])
# 
# # Merge the municipalities shapefile with the weather data:
# weather_df$month <- lubridate::month(weather_df$date)
# weather_df$year <- lubridate::year(weather_df$date)
# 
# weather_municip_R01_dt <- setDT(weather_df) # Convert data.frame to data.table
# data_sum <- weather_municip_R01_dt[ , .(R0_med = mean(R0_tmed),
#                                         R0_min = min(R0_tmin),
#                                         R0_max = mean(R0_tmax)), by = list(month,muni_name)]     # Aggregate data
# 
# plot_df <-  esp_can %>%  left_join(data_sum)
# weather_municip_R01_monthly <- weather_municip_R01_dt %>% group_by(month,muni_name) %>% 
#     summarise(R0_med = mean(R0_tmed),R0_min = min(R0_tmin),R0_max = mean(R0_tmax))

# 
# # Create plots:
# ggplot(plot_df) +
#   geom_sf(aes(fill = R0_max), size = 0.01) + 
#   scale_fill_viridis(name = "R0(T)", limits = c(0,max(plot_df$R0_max))) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) + 
#   theme_void() +
#   labs(title = "Month: {current_frame}") +
#   transition_manual(month)