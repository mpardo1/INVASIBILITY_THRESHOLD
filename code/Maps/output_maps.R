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
library(parallel)

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$R0 <- runif(nrow(esp_can),0,1) 
ggplot(esp_can) + 
  geom_sf(aes(fill = R0), linewidth = 0.01) +
  scale_fill_distiller(palette = "Spectral") 

Path <- "/home/marta/INVASIBILITY_THRESHOLD/output/R0/R0/R0_Dengue_2020.Rds"
df_Dengue <- readRDS(Path)
df_Dengue <- df_Dengue[,c(1,2,3,4,6)]
# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_Dengue$month <- lubridate::month(df_Dengue$date)
df_Dengue_m <- df_Dengue[,.(R0 = mean(R0)), by = list(NATCODE,month) ]
df_Dengue <- esp_can %>% left_join(df_Dengue_m)
df_Dengue$range_R0 <- ifelse(df_Dengue$R0 < 1,"[0,1)",
                             ifelse(df_Dengue$R0 >=1 &df_Dengue$R0 <100,"[1,100]",
                                    ifelse(df_Dengue$R0 >=100 &df_Dengue$R0 <300,"[100,300]",
                                           ifelse(df_Dengue$R0 >=300 &df_Dengue$R0 <600,"[300,600]",">600"))))
# # Create plots:
library("viridis")
library("gganimate")
ggplot(df_Dengue) +
  geom_sf(aes(fill = range_R0), linewidth = 0.01) +
  scale_fill_brewer(palette = "YlOrBr") +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month))

df_Dengue$bool_R0 <- ifelse(df_Dengue$R0 < 1,0,1)
df_Dengue_y <- df_Dengue %>% group_by(NATCODE) %>%
  summarise(R0_sum = sum(bool_R0))

library(RColorBrewer)
n <- 5
palette_name <- "Blues"
colors <- brewer.pal(n, palette_name)

ggplot(df_Dengue_y) +
  geom_sf(aes(fill = R0_sum), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral") +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void()

saveRDS(weather_dt,"~/INVASIBILITY_THRESHOLD/output/weather/Daily/weather_out_R0_rain_2022.Rds")
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