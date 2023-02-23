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
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/aemet_weather_daily_deep_history_sf_2023-02-01.Rds"
weather <- as.data.frame(readRDS(Path))
weather <- weather %>%
  mutate(gdd = gdd(tmax = tmax, tmin = tmin, tbase = 10,
                   tbase_max = 30)) %>%
  mutate(daily_acc_gdd = c(NA, diff(gdd)))

weather$R0_tmin <- sapply(weather$tmin, R0_func_alb)
weather$R0_tmed <- sapply(weather$tmed, R0_func_alb)
weather$R0_tmax <- sapply(weather$tmax, R0_func_alb)
# Merge the municipalities shapefile with the weather data:
weather_municip_R0 <- merge(x=esp_can, y=weather,
                          by.x="name",by.y="NAMEUNIT", all.x=TRUE, all.y = TRUE)


# Plot Map
R0_tmin_plot <- ggplot(weather_municip_R0) +
  geom_sf(aes(fill = R0_tmin), size = 0.01) + scale_fill_viridis() +
  geom_sf(data = can_box) + theme_bw() + ggtitle("Min temperature")

R0_tmed_plot <- ggplot(weather_municip_R0) +
  geom_sf(aes(fill = R0_tmed), size = 0.01) + scale_fill_viridis() +
  geom_sf(data = can_box) + theme_bw() + ggtitle("Average temperature")

R0_tmax_plot <- ggplot(weather_municip_R0) +
  geom_sf(aes(fill = R0_tmax), size = 0.01) + scale_fill_viridis() +
  geom_sf(data = can_box) + theme_bw() + ggtitle("Max temperature")


plot_3 <- ggarrange(R0_tmin_plot + 
                      scale_fill_discrete(name = "R0"),
                    R0_tmed_plot,
                    common.legend = TRUE,
                    legend = "bottom", ncol = 2)

plot <- ggarrange(plot_3,
          R0_tmax_plot,
          common.legend = TRUE, legend = "none", heights = c(1,0.8), ncol = 1)

plot + annotate("text", x = 0, y = 0.8, label = "August 2021")
     
## Function to read all output weather file compute R0 and create a list of df.  
plot_map <- function(path){

  weather <- as.data.frame(readRDS(paste0("~/INVASIBILITY_THRESHOLD/output/weather/Monthly/weather/",path)))
  weather$R0_tmin <- sapply(weather$tmin, R0_func_alb)
  weather$R0_tmed <- sapply(weather$tmed, R0_func_alb)
  weather$R0_tmax <- sapply(weather$tmax, R0_func_alb)
  
  colnames(esp_can) <- c(colnames(esp_can)[1:5], "NAMEUNIT",colnames(esp_can)[7:length(colnames(esp_can))])
  # Merge the municipalities shapefile with the weather data:
  weather_municip_R01 <-  esp_can %>%  left_join(weather)
    
    if(exists('weather_municip_R0') && is.data.frame(get('weather_municip_R0'))){
      weather_municip_R0 <- rbind(weather_municip_R0,weather_municip_R01)
    }else{
      weather_municip_R0 <- weather_municip_R01
    }

  return(weather_municip_R0)
}

Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Monthly/weather/"
list_file <- list.files(Path)
list_file_filt <- list_file[which(str_sub(list_file, -6,-5) == "10")]
rm(weather_municip_R0)
df_plot <- lapply(list_file_filt, plot_map)
rm(weather_municip_R0)
df_plot <- do.call(rbind.data.frame, df_plot)


# Create plots:
ggplot(df_plot) +
  geom_sf(aes(fill = R0_tmed), size = 0.01) + 
  scale_fill_viridis(name = "R0(T)", limits = c(0, 40)) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) + 
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(month)

anim_save("~/Documentos/PHD/2023/INVASIBILITY/Plots/animation_alb_2010.gif",
          animation = last_animation())

df_plot$bool <- ifelse(df_plot$R0_tmed >= 1, 1,0)
df_plot_bool <- df_plot %>%  group_by(NAMEUNIT) %>% 
  summarise( sum_bool = sum(bool))

plot_sum_albo <- ggplot(df_plot_bool) +
  geom_sf(aes(fill = sum_bool), lwd = 0) + 
  scale_fill_viridis(name = "Nº of months with R0>1",
                     limits = c(0, 12), option="magma") +
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("Aedes Albopictus 2010") + 
  theme_bw() 

plot_sum_albo
ggsave("~/Documentos/PHD/2023/INVASIBILITY/Plots/num_months_alb_2010.png")

# Time series in CCAA R0:
df_plot_ccaa <- df_plot %>% group_by(ine.ccaa.name, month, year) %>% 
  summarise( avg_R0 = mean(R0_tmed), n = n())
df_plot_ccaa$date <- as.Date(paste0("01/",df_plot_ccaa$month,"/20",df_plot_ccaa$year))

ggplot(df_plot_ccaa) + 
  geom_line(aes(date, avg_R0, color = ine.ccaa.name )) + 
  theme_bw()

ggsave("~/Documentos/PHD/2023/INVASIBILITY/Plots/ccaa_alb_2010.png")

#### -------------------------- Aegypti ------------------------- ####
## Thermal responses Aedes Aegypti from Mordecai 2017:
a_f_aeg <- function(temp){Briere_func(0.000202,13.35,40.08,temp)} # Biting rate
EFD_f_aeg <- function(temp){Briere_func(0.00856,14.58,34.61,temp)} # Fecundity
pEA_f_aeg <- function(temp){Quad_func(0.00599,13.56,38.29,temp)} # Survival probability Egg-Adult
MDR_f_aeg <- function(temp){Briere_func(0.0000786,11.36,39.17,temp)} # Mosquito Development Rate
lf_f_aeg <- function(temp){Quad_func(0.148,9.16,37.73,temp)} # Adult life span

# R0 function by temperature:
R0_func_aeg <- function(Te){
  a <- a_f_aeg(Te)
  f <- EFD_f_aeg(Te)
  deltaa <- 1/lf_f_aeg(Te)
  probla <- pEA_f_aeg(Te)
  R0 <- sqrt((f/deltaa)*probla)
  return(R0)
}

## Function to read all output weather file compute R0 and create a list of df.  
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Monthly/weather"
list_file <- list.files(Path)
## Function to read all output weather file compute R0 and create a list of df.  
plot_map <- function(path){
  
  weather <- as.data.frame(readRDS(paste0("~/INVASIBILITY_THRESHOLD/output/weather/Monthly/weather/",path)))
  weather$R0_tmin <- sapply(weather$tmin, R0_func_aeg)
  weather$R0_tmed <- sapply(weather$tmed, R0_func_aeg)
  weather$R0_tmax <- sapply(weather$tmax, R0_func_aeg)
  
  colnames(esp_can) <- c(colnames(esp_can)[1:5], "NAMEUNIT",colnames(esp_can)[7:length(colnames(esp_can))])
  # Merge the municipalities shapefile with the weather data:
  weather_municip_R01 <-  esp_can %>%  left_join(weather)
  
  if(exists('weather_municip_R0') && is.data.frame(get('weather_municip_R0'))){
    weather_municip_R0 <- rbind(weather_municip_R0,weather_municip_R01)
  }else{
    weather_municip_R0 <- weather_municip_R01
  }
  
  return(weather_municip_R0)
}

rm(weather_municip_R0)
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Monthly/weather/"
list_file <- list.files(Path)
list_file_filt <- list_file[which(str_sub(list_file, -6,-5) == "15")]
df_plot <- lapply(list_file_filt, plot_map)
df_plot <- do.call(rbind.data.frame, df_plot)

ggplot(df_plot) +
  geom_sf(aes(fill = R0_tmed), size = 0.01) + 
  scale_fill_viridis(name = "R0(T)", limits = c(0, 45)) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) + 
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(month)

anim_save("~/Documentos/PHD/2023/INVASIBILITY/Plots/animation_aeg.gif", animation = last_animation())

df_plot$bool <- ifelse(df_plot$R0_tmed >= 1, 1,0)
df_plot_bool <- df_plot %>%  group_by(NAMEUNIT) %>% 
  summarise( sum_bool = sum(bool))

plot_num_aeg <- ggplot(df_plot_bool) +
  geom_sf(aes(fill = sum_bool), lwd = 0) + 
  scale_fill_viridis(name = "Nº of months with R0>1", limits = c(0, 12), option="magma") +
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("Aedes aegypti 2010") +
  theme_bw() 

plot_num_aeg
ggsave("~/Documentos/PHD/2023/INVASIBILITY/Plots/num_months_aeg.png")

vec <- seq(0,40,1)
df_R0 <- data.frame(temp = vec,
           R0_alb = sapply(vec,R0_func_alb),
           R0_aeg = sapply(vec,R0_func_aeg))

df_plot <- reshape2::melt(df_R0, id.vars = c('temp'))
ggplot(df_plot) + 
  geom_line(aes(temp, value, color = variable)) +
  scale_colour_manual(name = "",
                      values = c("#d8b365", "#5ab4ac"),
                      labels = c("Aedes Albopictus", "Aedes Aegypti")) +
  ylab("R0") + xlab("Temperature (Cº)") + xlim(c(14,37)) +
  theme_bw() + theme(text = element_text(size=20))

ggarrange(plot_num_aeg,plot_sum_albo, common.legend = TRUE)

# Spain map municipalities with population density
spain_muni_map = st_read("~/Documentos/PHD/2023/INVASIBILITY/data/Municipios_de_Espa%C3%B1a._Padron2017.shp")
muni_geo <- st_read("~/Documentos/PHD/2023/INVASIBILITY/data/Municipios_IGN.shp")
spain_muni_map$PAD_2C02_norm <- spain_muni_map$PAD_2C02/max(spain_muni_map$PAD_2C02)
esp_pop <- merge(spain_muni_map,esp_can, by.x = "Texto", by.x = )

ggplot(spain_muni_map) +
  geom_sf(aes(fill = PAD_2C02_norm), size = 0.01) +
  geom_sf(data = can_box_pop) + theme_bw()

# ## Daily output
Path <- "~/INVASIBILITY_THRESHOLD/output/aemet_weather_year_2_18.Rds"
weather <- readRDS(Path)
weather <- as.data.frame(do.call(rbind, weather))
 
weather_ccaa <- weather %>%  group_by(ine.ccaa.name,fecha) %>% 
  summarise(tmin = ifelse(is.na(tmin) | is.infinite(tmin),0,min(tmin)),
            tmax = ifelse(is.na(tmax) | is.infinite(tmax),0,max(tmax)),
            tmed = ifelse(is.na(tmed) | is.infinite(tmed),0,mean(tmed)),
            precmed = ifelse(is.na(precmed) | is.infinite(precmed),0,mean(precmed)), n = n())

weather_ccaa <- unique(weather_ccaa)
weather_ccaa$R0_alb <- sapply(weather$tmed, R0_func_alb)
weather_ccaa$R0_aeg <- sapply(weather$tmed, R0_func_aeg)

weather_ccaa_filt <- weather_ccaa[which(weather_ccaa$ine.ccaa.name == "Canarias"),]
ggplot(weather_ccaa_filt) + 
  geom_point(aes(fecha,R0_alb, color = ine.ccaa.name)) + 
  theme_bw()
