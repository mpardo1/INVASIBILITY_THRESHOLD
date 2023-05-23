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
library(gganimate)
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
b_f_alb <- function(temp){Briere_func(0.000723,16,36.3,temp)} # Adult life span
c_f_alb <- function(temp){Briere_func(0.00433,2.8,36.7,temp)} # Adult life span
pdr_f_alb <- function(temp){Briere_func(0.00433,2.8,36.7,temp)} # Adult life span

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

# R0 with hacthing rate
vec <- seq(0,30,0.01)
hum_cte <- 100
te_cte <- 15
out <- sapply(vec,R0_func_alb,hum=hum_cte)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

# Population density in each municipality.
census <- mapSpain::pobmun19
esp_can_pop <- esp_can %>% left_join(census, by = c("cmun" = "cmun","cpro" = "cpro"))
esp_can_pop$area <- as.numeric(st_area(esp_can_pop))/1000000
esp_can_pop$pop_km <- esp_can_pop$pob19/esp_can_pop$area
esp_can_pop$NATCODE <- as.numeric(paste0("34",
                                         esp_can_pop$codauto,
                                         esp_can_pop$cpro,
                                         esp_can_pop$LAU_CODE))

#####
year = "2020"
Path = paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/",year,"/")
listfile <- list.files(Path)
weather_t <- data.table()
for(file in listfile){
  print(Path1)
  Path1 <- paste0(Path,file)
  weather <- readRDS(Path1)
  weather_df <- as.data.frame(do.call(rbind, weather))
  weather_dt <- setDT(weather_df) # Convert data.frame to data.table
  rm(weather,weather_df)
  weather_t <- rbind(weather_t,weather_dt)
  
}

weather_t <- weather_t %>% left_join(esp_can_pop)
weather_t$R0_tmin <- 0
weather_t$R0_tmax <- 0
weather_t$R0_tmed <- 0
weather_t <- setDT(weather_t)

## Function to read all output weather file compute R0 and create a list of df.
DengueR0 <- mclapply(1:nrow(weather_t), mc.cores = Cores, mc.preschedule = F,function(j){
  weather_t$R0_tmin[j] <- R0_func_alb(weather_t$tmin[j],weather_t$pob19[j])
  weather_t$R0_tmax[j] <- R0_func_alb(weather_t$tmax[j],weather_t$pob19[j])
  weather_t$R0_tmed[j] <- R0_func_alb(weather_t$tmed[j],weather_t$pob19[j])
})

saveRDS(DengueR0,
        "~/INVASIBILITY_THRESHOLD/output/R0/R0_Dengue_2020.Rds")
# dengue_df <- readRDS("~/INVASIBILITY_THRESHOLD/output/R0/R0_Dengue_2020.Rds")
# dengue_df <- dengue_df[,c("NATCODE","R0_tmed","month")]
# # Spain map municipalities
# esp_can <- esp_get_munic_siane(moveCAN = TRUE)
# can_box <- esp_get_can_box()
# esp_can$NATCODE <- as.numeric(paste0("34",
#                                      esp_can$codauto,
#                                      esp_can$cpro,
#                                      esp_can$LAU_CODE))
# dengue_df <- esp_can %>% left_join(dengue_df)
# 
# # # Create plots:
# ggplot(dengue_df) +
#   geom_sf(aes(fill = R0_tmed/max(R0_tmed)), linewidth = 0.01) +
#   scale_fill_viridis(name = "R0(T)", limits = c(0,1)) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
#   theme_void() +
#   labs(title = "Month: {current_frame}") +
#   transition_manual(as.factor(month))
# 
# saveRDS(weather_dt,"~/INVASIBILITY_THRESHOLD/output/weather/Daily/weather_out_R0_rain_2022.Rds")
# # 
# # weather_df$R0_tmin <- sapply(weather_df$precmed, R0_func_alb,
# #                              hum = weather_df$pop_km, Te = weather_df$tmin)
# # weather_df$R0_tmed <- sapply(weather_df$precmed, R0_func_alb)
# # weather_df$R0_tmax <- sapply(weather_df$precmed, R0_func_alb)
# 
# # colnames(esp_can) <- c(colnames(esp_can)[1:5], "muni_name",colnames(esp_can)[7:length(colnames(esp_can))])
# # 
# # # Merge the municipalities shapefile with the weather data:
# # weather_df$month <- lubridate::month(weather_df$date)
# # weather_df$year <- lubridate::year(weather_df$date)
# # 
# # weather_municip_R01_dt <- setDT(weather_df) # Convert data.frame to data.table
# # data_sum <- weather_municip_R01_dt[ , .(R0_med = mean(R0_tmed),
# #                                         R0_min = min(R0_tmin),
# #                                         R0_max = mean(R0_tmax)), by = list(month,muni_name)]     # Aggregate data
# # 
# # plot_df <-  esp_can %>%  left_join(data_sum)
# # weather_municip_R01_monthly <- weather_municip_R01_dt %>% group_by(month,muni_name) %>% 
# #     summarise(R0_med = mean(R0_tmed),R0_min = min(R0_tmin),R0_max = mean(R0_tmax))
# 
# # 
# # # Create plots:
# # ggplot(plot_df) +
# #   geom_sf(aes(fill = R0_max), size = 0.01) + 
# #   scale_fill_viridis(name = "R0(T)", limits = c(0,max(plot_df$R0_max))) +
# #   geom_sf(data = can_box) + coord_sf(datum = NA) +
# #   theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) + 
# #   theme_void() +
# #   labs(title = "Month: {current_frame}") +
# #   transition_manual(month)
# 
# # Plot 3D function hatching rates
# y <- seq(0, 16, length= 200)
# x <- seq(0, 1000, length= 1000)
# df_xy <- expand.grid(x,y)
# df_xy$z <- 0
# for(i in c(1:nrow(df_xy))){
#   print(i)
#   df_xy[i,3] <- h_f(df_xy$Var1[i], df_xy$Var2[i])
# }
# # 
# Path <- "~/Documents/PHD/2023//Mosquito\ invasibility/outputhatching.Rds"
# # saveRDS(df_xy,Path)
# df_xy <- readRDS(Path)
# 
# library(rgl)
# library(RColorBrewer)
# # Create a color palette using the color_brewer package
# my_colors <- c("yellow","red", "black")
# my_palette <- colorRampPalette(my_colors)(100)
# 
# plot3d(x=df_xy$Var1,y=df_xy$Var2,z=df_xy$z, col = my_palette[df_xy$z * 100],
#        cex.lab=10,cex.axis=5,
#        xlab = "Human density (km2)", ylab = "Rainfall(mm day)",
#        zlab = "Haching rate") 
# 
# 
# rgl.postscript("~/Documents/PHD/2023//Mosquito\ invasibility/Plots/my_plot.ps", fmt = "eps")
# 
# 
