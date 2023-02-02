rm(list=ls())
library("mapSpain")
library(tidyverse)
library(sf)
library(pollen)

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

# Read the weather data for a specific month and year for all municipalities
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/aemet_weather_daily_deep_history_sf_2023-02-01.Rds"
weather <- as.data.frame(readRDS(Path))
weather <- weather %>%
  mutate(gdd = gdd(tmax = tmax, tmin = tmin, tbase = 10,
                   tbase_max = 30)) %>%
  mutate(daily_acc_gdd = c(NA, diff(gdd)))

weather$R0_tmin <- sapply(weather$tmin, R0_func)
weather$R0_tmed <- sapply(weather$tmed, R0_func)
weather$R0_tmax <- sapply(weather$tmax, R0_func)
# Merge the municipalities shapefile with the weather data:
weather_municip_R0 <- merge(x=esp_can, y=weather,
                          by.x="name",by.y="NAMEUNIT", all.x=TRUE, all.y = TRUE)


# Plot Map
library(viridis)
R0_tmin_plot <- ggplot(weather_municip_R0) +
  geom_sf(aes(fill = R0_tmin), size = 0.01) + scale_fill_viridis() +
  geom_sf(data = can_box) + theme_bw() + ggtitle("Min temperature")

R0_tmed_plot <- ggplot(weather_municip_R0) +
  geom_sf(aes(fill = R0_tmed), size = 0.01) + scale_fill_viridis() +
  geom_sf(data = can_box) + theme_bw() + ggtitle("Average temperature")

R0_tmax_plot <- ggplot(weather_municip_R0) +
  geom_sf(aes(fill = R0_tmax), size = 0.01) + scale_fill_viridis() +
  geom_sf(data = can_box) + theme_bw() + ggtitle("Max temperature")

library("ggpubr")
plot_3 <- ggarrange(R0_tmin_plot + 
                      scale_fill_discrete(name = "R0"),
                    R0_tmed_plot,
                    common.legend = TRUE,
                    legend = "bottom", ncol = 2)

plot <- ggarrange(plot_3,
          R0_tmax_plot,
          common.legend = TRUE, legend = "none", heights = c(1,0.8), ncol = 1)

plot + annotate("text", x = 0, y = 0.8, label = "August 2021")
          