rm(list=ls())
library("mapSpain")
library(tidyverse)
library(sf)
library(pollen)
library(raster)
library(ggplot2)
library(sf)
library(dplyr)
library(lubridate)
library(gganimate)
library(rmapshaper)

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
        
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Monthly/"
list_file <- list.files(Path)

plot_map <- function(path){
  
  if(substr(path,24,24) == "_"){
        month_num <- as.numeric(substr(path,23,23))
      }else{
        month_num <- as.numeric(substr(path,23,24))
      }
  
  weather <- as.data.frame(readRDS(paste0("~/INVASIBILITY_THRESHOLD/output/weather/Monthly/",path)))
  weather$R0_tmin <- sapply(weather$tmin, R0_func)
  weather$R0_tmed <- sapply(weather$tmed, R0_func)
  weather$R0_tmax <- sapply(weather$tmax, R0_func)
  
  # Merge the municipalities shapefile with the weather data:
  weather_municip_R0 <- merge(x=esp_can, y=weather,
                              by.x="name",by.y="NAMEUNIT", all.x=TRUE, all.y = TRUE)
  
  Path_plots <- "~/INVASIBILITY_THRESHOLD/output/weather/"
  # Create plots:
  png(paste0(paste0(Path_plots,"plots/min",'/', as.character(month_num), '.png')), 
      width=6, height=6, unit="in", res=175, pointsize=10)
  
  R0_tmin_plot <- ggplot(weather_municip_R0) +
    geom_sf(aes(fill = R0_tmin), size = 0.01) + scale_fill_viridis(name = "R0(T)", limits = c(0, 40)) +
    geom_sf(data = can_box) + theme_bw() + ggtitle(paste("Min temperature,", "month:", month_num))
  dev.off()
  
  png(paste0(paste0(Path_plots,"plots/avg",'/', as.character(month_num), '.png')), 
      width=6, height=6, unit="in", res=175, pointsize=10)
  
  R0_tmed_plot <- ggplot(weather_municip_R0) +
    geom_sf(aes(fill = R0_tmed), size = 0.01) + scale_fill_viridis(name = "R0(T)", limits = c(0, 40)) +
    geom_sf(data = can_box) + theme_bw() + ggtitle(paste("Avg temperature,", "month:", month_num))
  dev.off()
  
  png(paste0(paste0(Path_plots,"plots/max",'/', as.character(month_num), '.png')), 
      width=6, height=6, unit="in", res=175, pointsize=10)
  
  R0_tmax_plot <- ggplot(weather_municip_R0) +
    geom_sf(aes(fill = R0_tmax), size = 0.01) + scale_fill_viridis(name = "R0(T)", limits = c(0, 40)) +
    geom_sf(data = can_box) + theme_bw() + ggtitle(paste("Max temperature,", "month:", month_num))
  dev.off()
  
  return(list(R0_tmin_plot,R0_tmed_plot,R0_tmax_plot))
}


plot_map <- function(path){

  if(substr(path,24,24) == "_"){
    month_num <- as.numeric(substr(path,23,23))
  }else{
    month_num <- as.numeric(substr(path,23,24))
  }

  weather <- as.data.frame(readRDS(paste0("~/INVASIBILITY_THRESHOLD/output/weather/Monthly/",path)))
  weather$R0_tmin <- sapply(weather$tmin, R0_func)
  weather$R0_tmed <- sapply(weather$tmed, R0_func)
  weather$R0_tmax <- sapply(weather$tmax, R0_func)
  
  colnames(esp_can) <- c(colnames(esp_can)[1:5], "NAMEUNIT",colnames(esp_can)[7:length(colnames(esp_can))])
  # Merge the municipalities shapefile with the weather data:
  weather_municip_R01 <-  esp_can %>% 
    left_join(weather)
    
    # merge(x=esp_can, y=weather,
    #                            by.x="name",by.y="NAMEUNIT", all.x=TRUE, all.y = TRUE)

  weather_municip_R01$month_n <- month_num
  if(exists('weather_municip_R0') && is.data.frame(get('weather_municip_R0'))){
    weather_municip_R0 <- rbind(weather_municip_R0,weather_municip_R01)
  }else{
    weather_municip_R0 <- weather_municip_R01
  }

  return(weather_municip_R0)
}

rm(weather_municip_R0)
df_plot <- lapply(list_file, plot_map)
df_plot <- do.call(rbind.data.frame, df_plot)

# Create plots:
df_plot <- df_plot[,c(6,9,17,18,19,21)]
df_plot %>% 
  ggplot() +
  geom_sf(aes(fill = R0_tmin)) +
  scale_fill_viridis_c() +
  theme_void() +
  coord_sf(datum = NA) +
  labs(title = "Month: {current_frame}") +
  transition_manual(month_n)

ggplot(df_plot) +
  geom_sf(aes(fill = R0_tmed), size = 0.01) + 
  scale_fill_viridis(name = "R0(T)", limits = c(0, 40)) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) + 
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(month_n)
