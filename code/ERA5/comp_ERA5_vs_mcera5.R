## Test weather era5 from mcera
rm(list=ls())
library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)

# Spain map -------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$R0_test <- runif(length(esp_can$codauto),0,5)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

# Read data ERA5 ---------------------------------------------
Path <- "~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2018/temp_out_daily_2018_05.Rds"
era5_18 <- readRDS(Path)
era5_18 <- as.data.frame(do.call(rbind,era5_18))
era5_18$month <- lubridate::month(era5_18$date)
era5_18 <- era5_18 %>% group_by(NATCODE) %>%
  summarize(tmean = mean(tmean))
era5_18 <- esp_can  %>% left_join(era5_18)

ggplot(era5_18) +
  geom_sf(aes(fill=tmean), color = NA) +
  scale_fill_viridis_c(option="magma")

# Read weather mcera5 -----------------------------------------
year = 2018
Path <- paste0("/home/marta/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",
               year,".Rds")
df_group <- setDT(readRDS(Path))
df_group$month <- lubridate::month(df_group$date)
df_5 <- df_group[which(df_group$month == 5),]
df_5 <- df_5 %>% group_by(NATCODE) %>%
  summarize(tmean = mean(tmean))
df_5 <- esp_can  %>% left_join(df_5)

ggplot(df_5) +
  geom_sf(aes(fill=tmean), color = NA) +
  scale_fill_viridis_c(option="magma")

Path <- "~/INVASIBILITY_THRESHOLD/output/ERA5/rainfall/2018/rainfall_2018_05.Rds"
