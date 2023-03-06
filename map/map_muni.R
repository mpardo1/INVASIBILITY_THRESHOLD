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

# Population density in each municipality.
census <- mapSpain::pobmun19

# Read the weather data for a specific month and year for all municipalities
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/aemet_weather_year_2_20.Rds"
weather <- readRDS(Path)
weather_df <- as.data.frame(do.call(rbind, weather))
colnames(weather_df) <- c("muni_name", "ccaa_name", "prov_name",
                          "date", "tmin", "tmed", "tmax", "precmed", "n")

## Function to read all output weather file compute R0 and create a list of df.  
weather_df$R0_tmin <- sapply(weather_df$tmin, R0_func_alb)
weather_df$R0_tmed <- sapply(weather_df$tmed, R0_func_alb)
weather_df$R0_tmax <- sapply(weather_df$tmax, R0_func_alb)
  
colnames(esp_can) <- c(colnames(esp_can)[1:5], "muni_name",colnames(esp_can)[7:length(colnames(esp_can))])

# Merge the municipalities shapefile with the weather data:
weather_df$month <- lubridate::month(weather_df$date)
weather_df$year <- lubridate::year(weather_df$date)

weather_municip_R01_dt <- setDT(weather_df) # Convert data.frame to data.table
data_sum <- weather_municip_R01_dt[ , .(R0_med = mean(R0_tmed),
                                        R0_min = min(R0_tmin),
                                        R0_max = mean(R0_tmax)), by = list(month,muni_name)]     # Aggregate data

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

anim_save("~/Documentos/PHD/2023/INVASIBILITY/Plots/animation_alb_2020.gif",
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
ggsave("~/Documentos/PHD/2023/INVASIBILITY/Plots/num_months_alb_2020.png")

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

ggsave("~/Documentos/PHD/2023/INVASIBILITY/Plots/ccaa_alb_2010.png")

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

# R0 function by temperature:
R0_func_alb <- function(Te,hum,rain){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  deltaa <- 1/lf_f_alb(Te)
  probla <- pEA_f_alb(Te)
  h <- h_f(hum,rain)
  R0 <- sqrt(f*(a/deltaa)*probla*(h*(h+deltE)))
  return(R0)
}


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

weather_ccaa <- as.data.frame(unique(weather_ccaa))
weather_ccaa$R0_alb_med <- sapply(weather_ccaa$tmed, R0_func_alb)
weather_ccaa$R0_aeg_med <- sapply(weather_ccaa$tmed, R0_func_aeg)
weather_ccaa$R0_alb_max <- sapply(weather_ccaa$tmax, R0_func_alb)
weather_ccaa$R0_aeg_max <- sapply(weather_ccaa$tmax, R0_func_aeg)
weather_ccaa$R0_alb_min <- sapply(weather_ccaa$tmin, R0_func_alb)
weather_ccaa$R0_aeg_min <- sapply(weather_ccaa$tmin, R0_func_aeg)
weather_ccaa$R0_alb_avg <- (weather_ccaa$R0_alb_med + weather_ccaa$R0_alb_min + weather_ccaa$R0_alb_max)/3
weather_ccaa$R0_aeg_avg <- (weather_ccaa$R0_aeg_med + weather_ccaa$R0_aeg_min + weather_ccaa$R0_aeg_max)/3

ggplot(weather_ccaa) + 
  geom_line(aes(fecha,R0_alb, color = ine.ccaa.name)) + 
  theme_bw()

weather_ccaa_filt <- weather_ccaa[which(weather_ccaa$ine.ccaa.name == "Extremadura"),]
ggplot(weather_ccaa_filt) + 
  geom_smooth(aes(fecha,R0_alb_avg, color = ine.ccaa.name)) + 
  theme_bw()
