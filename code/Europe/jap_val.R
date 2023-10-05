# code to validate the PA for japonicus in europe
rm(list=ls())
library(sf)
library(ggplot2)
library(tidyverse)
library(terra)
library(parallel)

# read presence absence data ------------------------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/status_2303.shp"
pa_jap <- read_sf(path)
pa_jap <- pa_jap[which(pa_jap$leave == 1),]

# ggplot(pa_jap) +
#   geom_sf(aes(fill = japonicus)) 

# load data for RM Japonicus Europe 2020
clim_pop <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/EU_R0_2020.Rds"))

# transform crs raster ----------------------------------------------
temp_eu <- rast("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/3monthly_aggregated_raster.tif")
# plot(temp_eu[[1]])
clim_pop_s <- st_as_sf(clim_pop,
                       coords = c("lon","lat"),
                       crs = crs(temp_eu))
# ggplot(clim_pop_s) + 
#   geom_sf(aes(color = sum_alb)) + 
#   scale_fill_viridis_c()

# transform to match coord system -----------------------------------------
clim_pop_s <- st_transform(clim_pop_s, crs(pa_jap))

# extract intersection for each geometry-----------------------------------
inter <- function(pol_id){
  pnts <- clim_pop_s[,c("geometry", "id")] %>% mutate(
    intersection = as.integer(st_intersects(geometry, pa_jap[pol_id,c("geometry")]))
  )
  
  pnts <- pnts[which(is.na(pnts$intersection) == FALSE),c("id","intersection")]
  if(nrow(pnts) > 0){
    pnts$geometry <- NULL
    pnts <- pnts %>% left_join(clim_pop_s[,c("id","sum_alb")])
    pnts <- pnts[which(is.na(pnts$sum_alb)==FALSE),]
    avg_alb <- mean(pnts$sum_alb)
    return(c(unique(pnts$intersection),avg_alb))
  }else{
    return(c(pol_id,0))
  }
}

# test
# pol_id = 15
# inter(pol_id)
# ggplot(pa_jap) +
#   geom_sf(aes(fill = japonicus))  +
#   geom_sf(data =pnts[which(pnts$intersection == 1),],
#           aes(color = intersection), color = "red")


# # parallelize
cores = 12
intersect_p_g <- mclapply(1:nrow(pa_jap), mc.cores = cores,
                         mc.preschedule = F,inter)
saveRDS(intersect_p_g, "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/out_inter.Rds")
