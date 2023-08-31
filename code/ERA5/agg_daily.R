##  Aggregate data ERA5 daily
rm(list=ls())
library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)

# Read data temp -------------------------------------------------
df_20 <- data.frame()
for(i in c(1:12)){
  mon <- ifelse(i<10, paste0("0",i),as.character(i))
  Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/temp_out_daily_2020_",mon,".Rds")
  df_aux <- readRDS(Path)
  df_aux <- setDT(do.call(rbind,df_aux))
  df_20 <- rbind(df_20, df_aux)
}

# Test weather data -------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_day <- df_20[which(df_20$date == as.Date("2020-04-05")),]
df_day <- esp_can %>% left_join(df_day)
df_day <- ggplot(df_day) + 
  geom_sf(aes(fill = tmean ), color = NA) +
  scale_fill_viridis_c(option = "magma",limits = c(-5,25)) + 
  theme_bw()

# Read data rain -------------------------------------------------
year = 2020
Path <- paste0("/home/marta/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",year,".Rds")
df_aux <- readRDS(Path)
df_aux <- setDT(df_aux[,c("NATCODE", "prec1", "date")])

# Join two data sets ---------------------------------------------
df_20 <- df_20 %>% left_join(df_aux)

# Save data frame ------------------------------------------------
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/clim_",year,".Rds")
saveRDS(df_20, Path)

# 2022 other Copernicus ------------------------------------------
# Read data temp -------------------------------------------------
df_22 <- data.frame()
for(i in c(1:12)){
  mon <- ifelse(i<10, paste0("0",i),as.character(i))
  Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/2022/temp_",mon,"_2022.Rds")
  df_aux <- readRDS(Path)
  df_22 <- rbind(df_22, df_aux)
}

# Test weather data -------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_day <- df_22[which(df_22$date == as.Date("2020-04-05")),]
df_day <- esp_can %>% left_join(df_day)
df_day <- ggplot(df_day) + 
  geom_sf(aes(fill = tmean ), color = NA) +
  scale_fill_viridis_c(option = "magma",limits = c(-5,25)) + 
  theme_bw()

# Read data rain -------------------------------------------------
year = 2022
Path <- paste0("/home/marta/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",year,".Rds")
df_aux <- readRDS(Path)
df_aux <- setDT(df_aux[,c("NATCODE", "prec1", "date")])

# Join two data sets ---------------------------------------------
df_22 <- df_22 %>% left_join(df_aux)

# Save data frame ------------------------------------------------
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/clim_",year,".Rds")
saveRDS(df_22, Path)

# 2004 with CERRA and mcera5 for rainfall -------------------------
# Read data temp -------------------------------------------------
df_20 <- data.frame()
for(i in c(1:12)){
  mon <- ifelse(i<10, paste0("0",i),as.character(i))
  Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2004/temp_out_daily_2004_",mon,".Rds")
  df_aux <- readRDS(Path)
  df_aux <- setDT(do.call(rbind,df_aux))
  df_20 <- rbind(df_20, df_aux)
}

# Test weather data -------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_day <- df_20[which(df_20$date == as.Date("2004-02-05")),]
df_day <- esp_can %>% left_join(df_day)
df_day <- ggplot(df_day) + 
  geom_sf(aes(fill = tmean ), color = NA) +
  scale_fill_viridis_c(option = "magma",limits = c(-5,25)) + 
  theme_bw()
df_day

# Read data rain -------------------------------------------------
year = 2004
Path <- paste0("/home/marta/INVASIBILITY_THRESHOLD/output/mcera5/process_hourly_daily_ERA5_daily_mcera_2004.Rds")
df_aux <- readRDS(Path)
df_aux <- setDT(df_aux[,c("NATCODE", "prec1", "date")])

# Join two data sets ---------------------------------------------
df_20 <- df_20 %>% left_join(df_aux)

# Save data frame ------------------------------------------------
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/clim_",year,".Rds")
saveRDS(df_20, Path)

