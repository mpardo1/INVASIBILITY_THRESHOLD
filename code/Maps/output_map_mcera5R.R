rm(list=ls())
library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)
library("viridis")
library("gganimate")

df_group <- readRDS("~/INVASIBILITY_THRESHOLD/output/R0/R0_ERA5_daily_mcera_2020.Rds")

#-----------------------------Create plots------------------------#
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_group$month <- lubridate::month(df_group$date)

# Temporal R0 for Barcelona
NATCODE_BCN <- esp_can$NATCODE[which(esp_can$name == "Barcelona")]
df_BCN <- df_group[which(df_group$NATCODE == NATCODE_BCN),
                   c("NATCODE","date", "R0_daily_comp",
                     "R0_daily_comp_min","R0_daily_comp_max")]
colnames(df_BCN) <- c("NATCODE","date", "R0 mean temp",
                      "R0 min temp","R0 max temp")
df_BCN <- reshape2::melt(df_BCN, id.vars = c("NATCODE", "date"))
df_BCN <- esp_can %>%
  left_join(df_BCN)
df_BCN <- df_BCN[which(df_BCN$name =="Barcelona"),]

ggplot(df_BCN) + 
  geom_line(aes(date,value, color = variable)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  ylab("Vector Suitability Indes (VSI)") +
  xlab("Date") + ggtitle("Barcelona 2020") +
  theme_bw()

# Plot R0
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_group <- esp_can %>% left_join(df_group)
df_group <- setDT(df_group)

# Remove NA from R0 with municipilities
ind <- is.na(df_group$R0_mean_hourly)
R0_cpro <- df_group[which(df_group$cpro %in%
                            df_group$cpro[ind] & 
                                 is.na(df_group$R0_mean_hourly) == FALSE),] %>%
  group_by(cpro,month) %>%
  summarise(R0_tmed1 = mean(R0_mean_hourly))

df_group  <- df_group %>% left_join(R0_cpro)
df_group  <- df_group[,c("NATCODE", "month", "temp", "min_temp",
                         "max_temp","prec", "R0_tmed1", "R0_mean_hourly",
                         "R0_daily_comp", "pop", "R0_daily_comp_min",
                         "R0_daily_comp_max")]
rm(R0_cpro)
df_group$R0_daily_comp <- ifelse(is.na(df_group$R0_daily_comp),
                           df_group$R0_tmed1,
                           df_group$R0_daily_comp)
df_group$R0_mean_hourly <- ifelse(is.na(df_group$R0_mean_hourly),
                                 df_group$R0_tmed1,
                                 df_group$R0_mean_hourly)

## Whole map moving monthly
df_group_mon <- df_group[, .(temp=mean(temp),
                             prec = sum(prec), 
                             pop=min(pop),
                             R0_mean_monthly = mean(R0_mean_hourly)), 
                         by=list(NATCODE,month)]
df_group_mon <- esp_can %>%
  left_join(df_group_mon)

ggplot(df_group_mon) +
  geom_sf(aes(fill = R0_mean_monthly), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month))

df_group_mon$bool_R0 <- ifelse(df_group_mon$R0_mean_monthly < 1,0,1)
df_group_y <- df_group_mon %>% group_by(NATCODE) %>%
  summarise(R0_sum = sum(bool_R0),
            avg_temp = mean(temp),
            sum_prec = sum(prec))

# Whole map group by number of months suitable
library(RColorBrewer)

ggplot(df_group_y) +
  geom_sf(aes(fill = R0_sum), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral") +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void()

ggplot(df_group_y) +
  geom_sf(aes(fill = avg_temp), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral") +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void()

ggplot(df_group_y) +
  geom_sf(aes(fill = sum_prec), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral") +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void()
