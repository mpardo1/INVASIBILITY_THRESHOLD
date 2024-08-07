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

vec <- seq(0,40,0.1)
out_R0_alb <- sapply(vec,R0_func_alb)
df_alb <- data.frame(Temp = vec, out_R0_alb)
plot_EFD_alb <- ggplot(df_alb) + 
  xlab("Temperature") + ylab("R0, suitability index") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_line(aes(Temp,out_R0_alb)) + 
  theme_bw() +
  theme(text = element_text(size = 15)
  )
plot_EFD_alb


# Read the weather data for a specific month and year for all municipalities
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/Daily/aemet_weather_year_whole_8.Rds"
weather <- readRDS(Path)
weather_df <- as.data.frame(do.call(rbind, weather))
colnames(weather_df) <- c("muni_name", "ccaa_name", "prov_name",
                          "date", "tmin", "tmed", "tmax", "precmed", "n")

## Function to read all output weather file compute R0 and create a list of df.  
weather_df$R0_tmin <- sapply(weather_df$tmin, R0_func_alb)
weather_df$R0_tmed <- sapply(weather_df$tmed, R0_func_alb)
weather_df$R0_tmax <- sapply(weather_df$tmax, R0_func_alb)
weather_df$R0_avg <- mean(weather_df$R0_tmin,weather_df$R0_tmax)
  
colnames(esp_can) <- c(colnames(esp_can)[1:5], "muni_name",colnames(esp_can)[7:length(colnames(esp_can))])

# Merge the municipalities shapefile with the weather data:
weather_df$month <- lubridate::month(weather_df$date)
weather_df$year <- lubridate::year(weather_df$date)

weather_municip_R01_dt <- setDT(weather_df) # Convert data.frame to data.table
plot_df_filt <- weather_municip_R01_dt[which((weather_municip_R01_dt$muni_name == "Barcelona") ),]
plot_df_filt$R0_mean <- ifelse(plot_df_filt$R0_tmin == 0 | plot_df_filt$R0_tmax == 0,0, plot_df_filt$R0_tmed )

ggplot(plot_df_filt) + 
  geom_smooth(aes(date, R0_tmed)) + 
  ylab("R0") + 
  theme_bw()

data_sum <- weather_municip_R01_dt[ , .(R0_med = mean(R0_tmed),
                                        R0_min = min(R0_tmin),
                                        R0_max = max(R0_tmax)), by = list(month,muni_name)]     # Aggregate data


plot_df <-  esp_can %>%  left_join(data_sum)
# weather_municip_R01_monthly <- weather_municip_R01_dt %>% group_by(month,muni_name) %>% 
#     summarise(R0_med = mean(R0_tmed),R0_min = min(R0_tmin),R0_max = mean(R0_tmax))


# Create plots:
ggplot(plot_df) +
  geom_sf(aes(fill = R0_max), size = 0.01) + 
  scale_fill_viridis(name = "R0(T)", limits = c(0,max(plot_df$R0_med))) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) + 
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(month)

anim_save("~/Documentos/PHD/2023/INVASIBILITY/Plots/animation_alb_tmax_2020.gif",
          animation = last_animation())

data_sum$bool <- ifelse(data_sum$R0_med >= 1, 1,0)
plot_df_grouped <- data_sum[ , .(sum_bool = sum(bool)), 
                                           by = list(muni_name)]     # Aggregate data

plot_df <-  esp_can %>%  left_join(plot_df_grouped)

plot_sum_albo <- ggplot(plot_df) +
  geom_sf(aes(fill = sum_bool), lwd = 0) + 
  scale_fill_viridis(name = "Nº of months with R0>1",
                     limits = c(0, 12), option="turbo") +
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("Aedes Albopictus 2020") + 
  theme_bw() 

plot_sum_albo
ggsave("~/Documentos/PHD/2023/INVASIBILITY/Plots/num_months_max_alb_2020.png")

rm(data_sum,weather_df,weather,plot_df,plot_df_grouped)

# Time series in CCAA R0:
weather_municip_R01_dt <- weather_municip_R01_dt[-which( is.na(weather_municip_R01_dt$muni_name) ),]
df_plot_ccaa <- weather_municip_R01_dt[ , .(avg_R0 = mean(R0_tmed)), 
                         by = list(ccaa_name, date)]     # Aggregate data

df_plot_ccaa$date <- as.Date(paste0("01/",df_plot_ccaa$month,"/20",df_plot_ccaa$year))
plot_df_ccaa <- merge(esp_can, df_plot_ccaa, 
                      by.x = "ine.ccaa.name", by.y = "ccaa_name")


plot_df_ccaa$rollmean_R0_med <- rollmean(avg_r0,30)
plot_df_ccaa_filt <- plot_df_ccaa[which((plot_df_ccaa$ine.ccaa.name == "Cataluña") |  
                                          (plot_df_ccaa$ine.ccaa.name == "Valencia")|  
                                          (plot_df_ccaa$ine.ccaa.name == "Extremadura")|  
                                          (plot_df_ccaa$ine.ccaa.name == "Galicia")),]
avg_r0 <- c(as.numeric(plot_df_ccaa_filt$avg_R0), as.numeric(plot_df_ccaa_filt$avg_R0[1:7]))
roll_mean <-  rollmean(as.numeric(avg_r0),30)
plot_df_ccaa_filt$roll_mean <- roll_mean[1:nrow(plot_df_ccaa_filt)]

ggplot(plot_df_ccaa_filt) + 
  geom_line(aes(date, roll_mean, color = ine.ccaa.name )) + 
  ylab("R0") + 
  theme_bw()

ggplot(plot_df_ccaa_filt) + 
  geom_line(aes(date,avg_R0, color = ine.ccaa.name )) + 
  ylab("R0") + 
  theme_bw()

ggplot(plot_df_ccaa_filt) + 
  geom_smooth(aes(date, avg_R0, color = ine.ccaa.name )) + 
  ylab("R0") + 
  theme_bw()

ggsave("~/Documentos/PHD/2023/INVASIBILITY/Plots/ccaa_alb_2010.png")

plot_df_filt <- plot_df[which((plot_df$ine.ccaa.name == "Cataluña")),]
#### -------------------------- Aegypti ------------------------- ####
## Thermal responses Aedes Aegypti from Mordecai 2017:
a_f_aeg <- function(temp){Briere_func(0.000202,13.35,40.08,temp)} # Biting rate
EFD_f_aeg <- function(temp){Briere_func(0.00856,14.58,34.61,temp)} # Fecundity
pEA_f_aeg <- function(temp){Quad_func(0.00599,13.56,38.29,temp)} # Survival probability Egg-Adult
MDR_f_aeg <- function(temp){Briere_func(0.0000786,11.36,39.17,temp)} # Mosquito Development Rate
lf_f_aeg <- function(temp){Quad_func(0.148,9.16,37.73,temp)} # Adult life span

vec <- seq(0,40,1)
EFD_aeg <- sapply(vec,EFD_f_aeg)
df_aeg <- data.frame(Temp = vec, EFD_aeg)
plot_aeg_EFD <- ggplot(df_aeg) + 
  geom_line(aes(Temp,EFD_aeg))

df_merge <- merge(df_alb,df_aeg)
df_merge <- df_merge[,c("Temp","EFD_alb", "EFD_aeg")]
colnames(df_merge) <- c("Temperature", "Aedes albopictus", "Aedes aegypti")
df_merge <- reshape2::melt(df_merge, id.var = "Temperature")
ggplot(df_merge) + ylab("EFD, eggs per female per day") +
  geom_line(aes(Temperature,value, color = variable)) + theme_bw()

library("ggpubr")
ggarrange(plot_EFD_alb + ylim(c(0,26)) + xlab("Temperature") +
            ylab("EFD, eggs per female per day") +
            ggtitle("Aedes Albopictus") + theme_bw(),
          plot_aeg_EFD + ylim(c(0,26)) + ylab("") + xlab("Temperature") +
            ggtitle("Aedes Aegipty") +theme_bw())

# R0 function by temperature:
R0_func_aeg <- function(Te){
  a <- a_f_aeg(Te)
  f <- EFD_f_aeg(Te)
  deltaa <- 1/lf_f_aeg(Te)
  probla <- pEA_f_aeg(Te)
  R0 <- sqrt((f/deltaa)*probla)
  return(R0)
}

Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/aemet_weather_year_Marz_3.Rds"
weather <- readRDS(Path)
weather_df <- as.data.frame(do.call(rbind, weather))
colnames(weather_df) <- c("muni_name", "ccaa_name", "prov_name",
                          "date", "tmin", "tmed", "tmax", "precmed", "n")

## Function to read all output weather file compute R0 and create a list of df.  
weather_df$R0_tmin <- sapply(weather_df$tmin, R0_func_aeg)
weather_df$R0_tmed <- sapply(weather_df$tmed, R0_func_aeg)
weather_df$R0_tmax <- sapply(weather_df$tmax, R0_func_aeg)

colnames(esp_can) <- c(colnames(esp_can)[1:5], "muni_name",colnames(esp_can)[7:length(colnames(esp_can))])

# Merge the municipalities shapefile with the weather data:
weather_df$month <- lubridate::month(weather_df$date)
weather_df$year <- lubridate::year(weather_df$date)

weather_municip_R01_dt <- setDT(weather_df) # Convert data.frame to data.table
data_sum <- weather_municip_R01_dt[ , .(R0_med = mean(R0_tmed),
                                        R0_min = min(R0_tmin),
                                        R0_max = max(R0_tmax)), by = list(month,muni_name)]     # Aggregate data

plot_df <-  esp_can %>%  left_join(data_sum)
# weather_municip_R01_monthly <- weather_municip_R01_dt %>% group_by(month,muni_name) %>% 
#     summarise(R0_med = mean(R0_tmed),R0_min = min(R0_tmin),R0_max = mean(R0_tmax))

# Create plots:
ggplot(plot_df) +
  geom_sf(aes(fill = R0_med), size = 0.01) + 
  scale_fill_viridis(name = "R0(T)", limits = c(0, 40)) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) + 
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(month)

anim_save("~/Documentos/PHD/2023/INVASIBILITY/Plots/animation_aeg_2003.gif",
          animation = last_animation())

data_sum$bool <- ifelse(data_sum$R0_med >= 1, 1,0)
plot_df_grouped <- data_sum[ , .(sum_bool = sum(bool)), 
                             by = list(muni_name)]     # Aggregate data

plot_df <-  esp_can %>%  left_join(plot_df_grouped)

plot_sum_aeg <- ggplot(plot_df) +
  geom_sf(aes(fill = sum_bool), lwd = 0) + 
  scale_fill_viridis(name = "Nº of months with R0>1",
                     limits = c(0, 12), option="turbo") +
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("Aedes Albopictus 2020") + 
  theme_bw() 

plot_sum_aego
ggsave("~/Documentos/PHD/2023/INVASIBILITY/Plots/num_months_aeg_2003.png")

rm(data_sum,weather_df,weather,plot_df,plot_df_grouped)
# Time series in CCAA R0:
df_plot_ccaa <- weather_municip_R01_dt[ , .(avg_R0 = mean(R0_tmed)), 
                                        by = list(ccaa_name, date)]     # Aggregate data

df_plot_ccaa$date <- as.Date(paste0("01/",df_plot_ccaa$month,"/20",df_plot_ccaa$year))
plot_df_ccaa <- merge(esp_can, df_plot_ccaa, 
                      by.x = "ine.ccaa.name", by.y = "ccaa_name")

plot_df_ccaa_filt <- plot_df_ccaa[which((plot_df_ccaa$ine.ccaa.name == "Cataluña") |  
                                          plot_df_ccaa$ine.ccaa.name == "Valencia"),]
ggplot(plot_df_ccaa) + 
  geom_smooth(aes(date, avg_R0, color = ine.ccaa.name )) + 
  ylab("R0") + 
  theme_bw()

ggsave("~/Documentos/PHD/2023/INVASIBILITY/Plots/ccaa_aeg_2010.png")