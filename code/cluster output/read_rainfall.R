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

Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/rainfall/monthly_rainf_aemet_weather_year_2_21.Rds"
R0_rain <- readRDS(Path)
can_box <- esp_get_can_box()
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
R0_rain$geometry <- NULL
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE))
R0_rain <- esp_can %>% left_join(R0_rain)
# Create plots:
ggplot(R0_rain) +
  geom_sf(aes(fill = R0_tmed), size = 0.01) + 
  scale_fill_viridis(name = "R0(T)", limits = c(0,max(R0_rain$R0_tmed))) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) + 
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(month)

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
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("") + 
  theme_bw() 
plot_sum_albo

## Plots reain annual
R0_rain <- readRDS(Path)
can_box <- esp_get_can_box()
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
R0_rain$geometry <- NULL
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE))
R0_rain <- esp_can %>% left_join(R0_rain)
# Create plots:
var_data <- R0_rain[ , .(tmed = mean(tmed),
                         sumprecmed = sum(precmed),
                         pop_km_max = max(pop_km),
                         pop_km_min = min(pop_km)),
                     by = list(year,NATCODE)]     # Aggregate data

plot_df <-  esp_can %>%  left_join(var_data)

plot_avg_temp <- ggplot(plot_df) +
  geom_sf(aes(fill = tmed), lwd = 0) + 
  scale_fill_viridis(name = "Average year\n temperature", option="D") +
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("") + 
  theme_bw() 
plot_avg_temp

plot_avg_rain <- ggplot(plot_df) +
  geom_sf(aes(fill = sumprecmed), lwd = 0) + 
  scale_fill_viridis(name = "Sum year\n precipitation", option="D") +
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("") + 
  theme_bw() 
plot_avg_rain

plot_avg_pop <- ggplot(plot_df) +
  geom_sf(aes(fill = pop_km_max/max(plot_df$pop_km_max)), lwd = 0) + 
  scale_fill_viridis(name = "Population density", option="H") +
  geom_sf(data = can_box) + coord_sf(datum = NA)  + 
  ggtitle("") + 
  theme_bw() 
plot_avg_pop

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