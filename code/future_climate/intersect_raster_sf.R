# Code to compute intesection between raster with points and geometry R
# Load libraries ----------------------------------------------------------
rm(list = ls())
library(sf)
library(terra)
library(ggplot2)
library(parallel)

# Read data of raster climatic --------------------------------------------------
esp_can <- readRDS("~/INVASIBILITY_THRESHOLD/data/future-climate/esp_can.Rds")
prec_sf <- readRDS("~/INVASIBILITY_THRESHOLD/data/future-climate/prec_sf.Rds")

# Test and check how intersects work
st_crs(prec_sf) <- st_crs(esp_can)
points <- prec_sf[11,"geometry"]
geometry <-  esp_can[,"geometry"]
intersections_lp <- st_intersects(geometry,points) %>% lengths > 0
esp_can[which(intersections_lp == TRUE),]
ggplot(geometry) +
  geom_sf(aes(fill = 1), lwd = 0) + 
  geom_sf(data = points, aes(fill = 2), color = "red", size = 0.5)

intersect_df <- data.frame(point = numeric(0), geometry = numeric(0))

# Intersect data from climate to mapSpain -----------------------------------
# There are points that do not intersect any geometry
# for(i in c(1:nrow(prec_sf))){
#   print(paste0("step:",i))
#   points <- prec_sf[i,"geometry"]
#   intersections_lp <- st_intersects(geometry,points) %>% lengths > 0
#   if(length(which(intersections_lp==TRUE)) > 0){
#     print("Intersect")
#     intersect_df[nrow(intersect_df) + 1,] <- c(i,which(intersections_lp==TRUE))
#   }
# }
# 
# # Add the NATCODE and the geometry point ------------------------------------ 
# intersect_df$geom <- prec_sf[intersect_df$point,"geometry"]
# intersect_df$NATCODE <- esp_can[intersect_df$geometry,"NATCODE"]

# Paralelize code ----------------------------------------------------------
inters <- function(i){
  points <- prec_sf[i,"geometry"]
  intersections_lp <- st_intersects(geometry,points) %>% lengths > 0
  if(length(which(intersections_lp==TRUE)) > 0){
    intersect_df <- c(i,which(intersections_lp==TRUE))
  }
  
  return(intersect_df)
}

num_cores = 10
intersect_df <- mclapply(c(1:nrow(prec_sf)), 
                         inters, 
                         mc.cores = num_cores)

# Save Rds with the intersection --------------------------------------------
saveRDS(intersect_df,
        "~/INVASIBILITY_THRESHOLD/data/future-climate/intersect_df_parall.Rds")

