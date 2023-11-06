# Compute map with diff between years for winter months
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

# Load data weather ----------------------------------------------------------
# this comes from maps_mcera5_group.R
year = 2004
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/R0_clim_monthly",
               year,".Rds")
df_2004 <- setDT(readRDS(Path))
df_2004 <- df_2004[,c("NATCODE", "month", "R0_mon_alb", "R0_mon_aeg", "R0_mon_jap")]
df_2004$year <- 2004

year = 2020
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/R0_clim_monthly",
               year,".Rds")
df_2020 <- setDT(readRDS(Path))
df_2020 <- df_2020[,c("NATCODE", "month", "R0_mon_alb")]

# this comes from future_climate/future_climate.R
year = 2040
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/monthly_clim_",
               year,".Rds")
df_2040 <- setDT(readRDS(Path))
df_2040 <- df_2040[,c("NATCODE", "month", "R0_alb")]
colnames(df_2040)  <- c("NATCODE", "month", "R0_alb_2040")
df_2040$month <- ifelse(df_2040$month == "Jan", 1, ifelse(
  df_2040$month == "Feb", 2, ifelse(
    df_2040$month == "Mar", 3, ifelse(
      df_2040$month == "Apr", 4, ifelse(
        df_2040$month == "May", 5, ifelse(
          df_2040$month == "Jun", 6, ifelse(
            df_2040$month == "Jul", 7, ifelse(
              df_2040$month == "Aug", 8, ifelse(
                df_2040$month == "Sep", 9, ifelse(
                  df_2040$month == "Oct", 10, ifelse(
                    df_2040$month == "Nov", 11, 12)))))))))))

year = 2060
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/monthly_clim_",
               year,".Rds")
df_2060 <- setDT(readRDS(Path))
df_2060 <- df_2060[,c("NATCODE", "month", "R0_alb")]
colnames(df_2060)  <- c("NATCODE", "month", "R0_alb_2060")
df_2060$month <- ifelse(df_2060$month == "Jan", 1, ifelse(
  df_2060$month == "Feb", 2, ifelse(
    df_2060$month == "Mar", 3, ifelse(
      df_2060$month == "Apr", 4, ifelse(
        df_2060$month == "May", 5, ifelse(
          df_2060$month == "Jun", 6, ifelse(
            df_2060$month == "Jul", 7, ifelse(
              df_2060$month == "Aug", 8, ifelse(
                df_2060$month == "Sep", 9, ifelse(
                  df_2060$month == "Oct", 10, ifelse(
                    df_2060$month == "Nov", 11, 12)))))))))))

# Filter winter months --------------------------------------------------------
list_months_n <- c(11,12,1,2)
df_clim <- df_2020[which(df_2020$month %in% list_months_n ), ] %>%
  left_join(df_2040[which(df_2040$month %in% list_months_n ), ]) %>%
  left_join(df_2060[which(df_2060$month %in% list_months_n ), ])

# Compute number of months suitable ------------------------------------------
df_clim$bool_alb_2020 <- ifelse(df_clim$R0_mon_alb >1,1,0)
df_clim$bool_alb_2040 <- ifelse(df_clim$R0_alb_2040 >1,1,0)
df_clim$bool_alb_2060 <- ifelse(df_clim$R0_alb_2060 >1,1,0)

df_clim_y <- df_clim %>% group_by(NATCODE) %>%
  summarise(sumr0_2020 = sum(bool_alb_2020),
            sumr0_2040 = sum(bool_alb_2040),
            sumr0_2060 = sum(bool_alb_2060))

# Diff num months between years ----------------------------------------------
df_clim_y$diff_60_20 <- df_clim_y$sumr0_2060 - df_clim_y$sumr0_2020
df_clim_y$diff_40_20 <- df_clim_y$sumr0_2040 - df_clim_y$sumr0_2020

# Join with sf spain map -----------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

df_clim_y <- esp_can %>% left_join(df_clim_y)

ggplot(df_clim_y) +
  geom_sf(aes(fill= as.factor(diff_40_20)), color = NA) +
  scale_fill_viridis_d(name = "",
                       option = "magma",
                       direction = -1) +
  ggtitle("Difference number of months suitable in winter from 2040 to 2020") +
  theme_minimal()

# Test if the NA in the future clim coincides with negative diff ----------
NA_futclim <- readRDS("~/INVASIBILITY_THRESHOLD/data/list_NA_futureclim.Rds")
list_NA_minuszero <- df_clim_y[which(df_clim_y$diff_40_20 < 0), "NATCODE"]
list_NA_minuszero$geometry <- NULL
intersect(NA_futclim,list_NA_minuszero)
