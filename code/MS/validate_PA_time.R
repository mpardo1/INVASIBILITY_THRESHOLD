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

## year = 2022, we will use this year to validate the data
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/R0/datasets/R0_2022.Rds")
df_group_tot <- readRDS(Path)

## Data Catu 
Path <- "~/INVASIBILITY_THRESHOLD/data/Datos_Catu/gi_min_model_pred.RData"
load(Path)
unique(gi_min_model_pred$trap_name)