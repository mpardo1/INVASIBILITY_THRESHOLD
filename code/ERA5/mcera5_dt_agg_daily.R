rm(list=ls())
library(mcera5)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)

#----------------------------------------------------------------------#
## Read the data for the R0 computed daily:

extract_weather <- function(year_n){
  Path <- paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/ERA5_daily_mcera_",year_n,".Rds")
  df_group <- readRDS(Path)
  # head(df_group[[5000]])
  dt_weather <- data.table()
  for(i in c(1:length(df_group))){
    print(paste0("i:",i))
    dt_aux <- setDT(df_group[[1]])
    dt_aux$date <- as.Date(dt_aux$obs_time)
    dt_aux <- dt_aux[,.(tmean = mean(temperature),
                        tmin = min(temperature),
                        tmax = max(temperature),
                        prec =sum(prec),
                        prec1 = sum(prec1),
                        pop = min(pop),
                        area= min(area)), by = list(date,NATCODE)]
    dt_weather <- rbind(dt_aux,dt_weather)
    df_group[[1]] <- NULL
    rm(dt_aux)
  }
  return(dt_weather)
}

year = 2014
out <- extract_weather(year)
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",year,".Rds")
saveRDS(out,Path)

year = 2015
out <- extract_weather(year)
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",year,".Rds")
saveRDS(out,Path)

year = 2016
out <- extract_weather(year)
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",year,".Rds")
saveRDS(out,Path)

year = 2017
out <- extract_weather(year)
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",year,".Rds")
saveRDS(out,Path)

year = 2018
out <- extract_weather(year)
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",year,".Rds")
saveRDS(out,Path)

year = 2019
out <- extract_weather(year)
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",year,".Rds")
saveRDS(out,Path)

year = 2020
out <- extract_weather(year)
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",year,".Rds")
saveRDS(out,Path)

year = 2021
out <- extract_weather(year)
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",year,".Rds")
saveRDS(out,Path)

year = 2022
out <- extract_weather(year)
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",year,".Rds")
saveRDS(out,Path)
