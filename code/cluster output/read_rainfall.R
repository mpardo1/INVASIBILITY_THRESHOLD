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

Path_R0 <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/rainfall/monthly_rainf_aemet_weather_year_2_21.Rds"
R0_rain <- readRDS(Path_R0)
can_box <- esp_get_can_box()
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
R0_rain$geometry <- NULL
R0_rain$bool <- ifelse(R0_rain$R0_tmed>1,1,0)
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
R0_rain <- esp_can %>% left_join(R0_rain)
R0_rain$bool[which(is.na(R0_rain$bool))] <- 0
# Create plots:
ggplot(R0_rain) +
  geom_sf(aes(fill = factor(bool)), size = 0.01) + 
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) + 
  theme_bw() +
  theme(legend.text=element_text(size=15),
        legend.position = c(0.13,0.8)) + 
  scale_fill_manual(values = c("#ffeda0", "#B60808"), name = "",
                    labels = c("R0<1", "R0>1")) +
  labs(title = "Month: {current_frame}") +
  transition_manual(month)

anim_save("~/Documentos/PHD/2023/INVASIBILITY/Plots/animation_alb_PA_2021.gif",
          animation = last_animation())
R0_rain <- setDT(R0_rain)
data_sum <- R0_rain[ , .(R0_med = mean(R0_tmed),
                                        R0_min = min(R0_tmin),
                                        R0_max = max(R0_tmax)),
                                    by = list(month,name)]     # Aggregate data

data_sum$bool <- ifelse(data_sum$R0_med >= 1, 1,0)
plot_df_grouped <- data_sum[ , .(sum_bool = sum(bool)), 
                             by = list(name)]     # Aggregate data

plot_df <-  esp_can %>%  left_join(plot_df_grouped)

plot_sum_albo <- ggplot(plot_df) +
  geom_sf(aes(fill = sum_bool), lwd = 0) + 
  scale_fill_viridis(name = "Nº of months\n with R0>1",
                     limits = c(0, 12), option="turbo") + 
  theme_bw() +
  theme(legend.text=element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = c(0.1,0.75),
        legend.title = element_text(size=15)) +
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("") 
plot_sum_albo

## Plots reain annual
R0_rain <- readRDS(Path_R0)
can_box <- esp_get_can_box()
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
R0_rain$geometry <- NULL
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE))
# R0_rain <- esp_can %>% left_join(R0_rain)
# Create plots:

var_data <- R0_rain[ , .(tmed = mean(tmed),
                         sumprecmed = sum(precmed),
                         pop_km_max = max(pop_km),
                         pop_km_min = min(pop_km),
                         avg_R0 = mean(R0_tmed),
                         sum_R0 = sum(R0_tmed)),
                     by = list(year,NATCODE)]     # Aggregate data

var_data$bool <- ifelse(var_data$avg_R0 > 1, 1,0)
var_data$bool[which(is.na(var_data$bool))] <- 0
plot_df <-  esp_can %>%  left_join(var_data)

plot_avg_R0 <- ggplot(plot_df) +
  geom_sf(aes(fill = avg_R0), lwd = 0) + 
  scale_fill_viridis(name = "Average R0\n temperature", option="D") +
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("") + 
  theme_bw() 
plot_avg_R0

plot_bool_R0 <- ggplot(plot_df) +
  geom_sf(aes(fill = factor(bool)), lwd = 0) + 
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  theme_bw() +
  theme(legend.text=element_text(size=15),
        legend.position = c(0.10,0.8)) + 
  scale_fill_manual(values = c("#ffeda0", "#B60808"), name = "",
                    labels = c("Absence", "Presence")) +
  ggtitle("") 
plot_bool_R0

plot_sum_R0 <- ggplot(plot_df) +
  geom_sf(aes(fill = sum_R0), lwd = 0) + 
  scale_fill_viridis(name = "Sum R0\n ",
                     option="D") +
  theme(legend.text=element_text(size=15),
        plot.title = element_text(size=15)) + 
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("") + 
  theme_bw() 
plot_sum_R0

plot_avg_pop <- ggplot(plot_df) +
  geom_sf(aes(fill = pop_km_max), lwd = 0) + 
  scale_fill_viridis(name = "Population density", option="H") +
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("") + 
  theme_bw() 
plot_avg_pop

plot_df$range_pop <- ""
plot_df$pop_km_max[which(is.na(plot_df$pop_km_max))] <- 2000
for (i in c(1:nrow(plot_df))) {
  if(plot_df$pop_km_max[i] <= 5){
    plot_df$range_pop[i] <- "[0,5]"
  }else if(plot_df$pop_km_max[i] >= 5 & plot_df$pop_km_max[i] <= 9){
    plot_df$range_pop[i] <- "[5,9]"
  }else if(plot_df$pop_km_max[i] >= 10 & plot_df$pop_km_max[i] <= 19){
    plot_df$range_pop[i] <- "[10,19]"
  }else if(plot_df$pop_km_max[i] >= 20 & plot_df$pop_km_max[i] <= 49){
    plot_df$range_pop[i] <- "[20,49]"
  }else if(plot_df$pop_km_max[i] >= 50 & plot_df$pop_km_max[i] <= 74){
    plot_df$range_pop[i] <- "[50,74]"
  }else if(plot_df$pop_km_max[i] >= 75 & plot_df$pop_km_max[i] <= 124){
    plot_df$range_pop[i] <- "[75,124]"
  }else if(plot_df$pop_km_max[i] >= 125 & plot_df$pop_km_max[i] <= 249){
    plot_df$range_pop[i] <- "[125,249]"
  }else if(plot_df$pop_km_max[i] >= 250 & plot_df$pop_km_max[i] <= 499){
    plot_df$range_pop[i] <- "[250,499]"
  }else if(plot_df$pop_km_max[i] > 500 & plot_df$pop_km_max[i] <= 1000){
    plot_df$range_pop[i] <- "[500,1000]"
  }else{
    plot_df$range_pop[i] <- ">1000"
  }
  
}
library(RColorBrewer)

plot_avg_pop <- ggplot(plot_df) +
  geom_sf(aes(fill = factor(range_pop)), lwd = 0) + 
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_brewer(palette = "YlOrRd") + 
  ggtitle("") + 
  theme_bw() 
plot_avg_pop

write.csv(plot_df, "~/Documentos/PHD/2023/INVASIBILITY/Plots/data_process.csv")
### Rainfall and temp map from dataset aemet
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/aemet_weather_year_2_21.Rds"
rainfall <- readRDS(Path)
rainfall <- setDT(do.call(rbind, rainfall))
rainfall$year <- lubridate::year(rainfall$fecha)
var_data <- rainfall[ , .(tmed = mean(tmed),
                         sumprecmed = sum(precmed),
                         avg_rain = mean(precmed)),
                     by = list(year,name)]     # Aggregate data

plot_df <-  esp_can %>%  left_join(var_data)

write.csv(plot_df, "~/Documentos/PHD/2023/INVASIBILITY/Plots/data_process2.csv")

plot_avg_rain <- ggplot(plot_df) +
  geom_sf(aes(fill = avg_rain), lwd = 0) +  
  theme_bw() +
  theme(legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        legend.position = c(0.10,0.75)) + 
  scale_fill_viridis(name = "Mean year\n precipitation", option="D") +
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("") 
plot_avg_rain

plot_avg_temp <- ggplot(plot_df) +
  geom_sf(aes(fill = tmed), lwd = 0) +  
  theme_bw() +
  theme(legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        legend.position = c(0.10,0.75)) + 
  scale_fill_viridis(name = "Mean year\n temperature", option="D") +
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("") 
plot_avg_temp

### Time series plot
R0_rain <- readRDS(Path)
plot_df_filt <- R0_rain[which((R0_rain$NAMEUNIT == "Barcelona") ),]

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