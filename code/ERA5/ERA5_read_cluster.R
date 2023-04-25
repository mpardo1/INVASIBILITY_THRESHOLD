rm(list=ls())
library(tidyverse)

temp_2000_02 <- readRDS("~/INVASIBILITY_THRESHOLD/output/ERA5/temp_out_daily_2000_02.Rds")
temp_2000_02 <- as.data.frame(do.call(rbind, temp_2000_02))

