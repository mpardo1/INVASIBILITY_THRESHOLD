# code to validate the PA for japonicus in europe
rm(list=ls())
library(sf)
library(ggplot2)
library(tidyverse)
library(terra)
library(parallel)
library(data.table)
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")

# read presence absence data ------------------------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/status_2303.shp"
pa_jap <- read_sf(path)
pa_jap <- pa_jap[which(pa_jap$leave == 1),]
list_eu <- unique(pa_jap$cntryName)[c(1:2,4,6,7:8,10:14,16,19:23,25:26,28:30,
                                      32:35,39:42,
                                      45:52,54:56,58:59,61,62,67,68,70,71)]
pa_jap <- pa_jap[which(pa_jap$cntryName %in% list_eu),]
# ggplot(pa_jap) +
#   geom_sf(aes(fill = japonicus)) 

# load data for RM Japonicus Europe 2020
clim_pop <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_clim_2020.Rds"))

# ------------------------- CLUSTER --------------------------------#
# transform crs raster ----------------------------------------------
 temp_eu <- rast("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/3monthly_aggregated_raster.tif")
# # plot(temp_eu[[1]])
 clim_pop <- st_as_sf(clim_pop,
                        coords = c("lon","lat"),
                        crs = crs(temp_eu))
# # ggplot(clim_pop_s) + 
# #   geom_sf(aes(color = sum_alb)) + 
# #   scale_fill_viridis_c()
 
# # transform to match coord system -----------------------------------------
clim_pop_s <- st_transform(clim_pop_s, crs(pa_jap))
pa_jap$ind <- seq(1, nrow(pa_jap),1)

# # extract intersection for each geometry-----------------------------------
 inter <- function(pol_id){
   pnts <- clim_pop_s[,c("geometry", "sum_jap", "id")] %>% 
      mutate(intersection = as.integer(st_intersects(geometry,
                                             pa_jap[pol_id,c("geometry")]))
   )
   pnts <- pnts[which(is.na(pnts$intersection) == FALSE),c("sum_jap","id")]
   if(nrow(pnts) > 0){
     df_out <- data.frame(sum_jap = pnts$sum_jap, pnt_id = pnts$id, geom_id = pol_id)
     colnames(df_out) <- c("sum_jap", "pnt_id","geom_id")
     df_out <- df_out[which(is.na(df_out$sum_jap) == FALSE), ]
     return(df_out)
   }else{
     df_out <- data.frame(sum_jap = 0, pnt_id = 0,  geom_id = 0)
     return(df_out)
   }
 }

# test
 pol_id = 15
 inter(pol_id)
 ggplot(pa_jap) +
   geom_sf(aes(fill = japonicus))  +
   geom_sf(data =pnts[which(pnts$intersection == 1),],
           aes(color = intersection), color = "red")


# parallelize
 cores = 12
 intersect_p_g <- mclapply(1:nrow(pa_jap), mc.cores = cores,
                          mc.preschedule = F,inter)
 saveRDS(intersect_p_g,
         "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/out_inter_dd.Rds")
