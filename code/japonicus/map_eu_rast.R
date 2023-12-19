rm(list = ls())
library(eurostat)
library(terra)
library(tidyverse)
library(data.table)
library(exactextractr)
library(sf)
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=form

# Check hatching rate japonicus
vec <- seq(0,20,0.1)
out <- sapply(vec, h_f_jap, land = 0.4)
df_h <- data.frame(vec, out)
ggplot(df_h) + geom_line(aes(vec,out)) +
  xlab("Monthly average rainfall, mm") +
  ylab("Hatching rate") + theme_bw()

# climate data europe ------------------------------------------------
temp_eu <- rast("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/3monthly_aggregated_raster.tif")
plot(temp_eu[[8]])

path_w <- "~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/monthlyrain_2022.grib"
rain_eu <- rast(path_w)
plot(rain_eu[[1]])

# European boundary ---------------------------------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/status_2303.shp"
eu_pa <- st_read(path)
eu_pa <- eu_pa[,"locCode"]
plot(eu_pa[,"locCode"])

# Eu
eu <- get_eurostat_geospatial(resolution = 3, 
                                 nuts_level = 3, 
                                 year = 2016)
plot(eu[,"id"])

# remove ocean from temp and rain
eu <- st_transform(eu, crs(temp_eu))
temp_eu <- mask(temp_eu, eu) # remove Oceans
plot(temp_eu[[7]])
# plot(eu)

eu <- get_eurostat_geospatial(resolution = 3, 
                              nuts_level = 3, 
                              year = 2021)
eu <- st_transform(eu, crs(rain_eu))
rain_eu <- mask(rain_eu, eu) # remove Oceans
plot(rain_eu[[3]])

# Load land cover ------------------------------------------------------
Path <- "~/INVASIBILITY_THRESHOLD/data/landcover/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif"
landcover <- rast(Path)
df_cat <- as.data.frame(levels(landcover))
plot(landcover)

# aggregate to bigger cells
# landcover_ag <- aggregate(landcover, fact=4,
#                           fun=function(vals){
#         # sum(vals == 23)/length(vals)})                    
#         sum(vals %in% c(23,18))/length(vals)})

# writeRaster(landcover_ag, "~/INVASIBILITY_THRESHOLD/data/landcover/landcover_2318.tiff")
# landcover_ag <- rast("~/INVASIBILITY_THRESHOLD/data/landcover/landcover.tiff")
landcover_ag <- rast("~/INVASIBILITY_THRESHOLD/data/landcover/landcover_2318.tiff")
plot(landcover_ag)

# Project landcover into temp coordinate system
landcover_proj <- project(landcover_ag,temp_eu, method = "average" )
rain_eu <- project(rain_eu, temp_eu, method = "average")
plot(rain_eu[[5]])
plot(landcover_proj)

# Create a grid of longitude and latitude values
lon <- seq(from = xmin(temp_eu), to = xmax(temp_eu),
           by = res(temp_eu)[1])
lat <- seq(from = ymin(temp_eu), to = ymax(temp_eu),
           by = res(temp_eu)[2])
grid_points <- expand.grid(lon = lon, lat = lat)
# grid_points$id <- c(1:nrow(grid_points))
# saveRDS(setDT(grid_points), "~/INVASIBILITY_THRESHOLD/data/landcover/grid_points.Rds")

# extract values as df --------------------------------------
temp <- terra::extract(temp_eu,
                       grid_points, xy =TRUE)
colnames(temp)[2:13] <- c(1:12)
temp <- reshape::melt(temp[,c(1:13)],id.vars = "ID")
colnames(temp) <- c("id", "month", "tmean")
rain <- terra::extract(rain_eu,
                       grid_points, xy =TRUE)
colnames(rain)[2:13] <- c(1:12)
rain <- reshape::melt(rain[,c(1:13)],id.vars = "ID")
colnames(rain) <- c("id", "month", "prec")
land <- terra::extract(landcover_proj,
                       grid_points, xy =TRUE)
colnames(land)[1] <- "id"

# Join all df and save ---------------------------------------
clim_df <- temp %>% left_join(rain)
clim_df <- clim_df %>% left_join(land)
saveRDS(setDT(clim_df), "~/INVASIBILITY_THRESHOLD/data/landcover/clim_land.Rds")
saveRDS(setDT(clim_df), "~/INVASIBILITY_THRESHOLD/data/landcover/clim_land2318.Rds")

# Load df Eu with clim and landcover
clim_df <- readRDS( "~/INVASIBILITY_THRESHOLD/data/landcover/clim_land.Rds")
# clim_df <- readRDS( "~/INVASIBILITY_THRESHOLD/data/landcover/clim_land2318.Rds")
# clim_df[which(is.na(clim_df$LABEL3)), "LABEL3"] <- 0
clim_df$prec <- clim_df$prec*1000 # change prec to mm (copernicus guideline)

# apply RM
# compute R0 --------------------------------------------------
clim_df <- setDT(clim_df)
clim_df[, R0_jap := mapply(R0_func_jap, tmean, prec, LABEL3)]
clim_df[, R0_jap2 := mapply(R0_func_jap_3, tmean, prec, LABEL3)]

# Plot monthly europe map
month_s <- 3
clim_df_m <- clim_df[which(clim_df$month == month_s),]
# grid_points <-readRDS( "~/INVASIBILITY_THRESHOLD/data/landcover/grid_points.Rds")
# clim_df_m <- clim_df_m %>% left_join(grid_points)
library(latex2exp)
ggplot(clim_df_m,aes(x = x, y = y,
                   fill = R0_jap2)) +
  geom_raster() +
  scale_fill_distiller(palette = "Spectral",
                       na.value = "white",
                       name = TeX("$R_M$"),
                       limits = c(0,3.76)) + 
  theme_minimal() + theme(legend.position = "right",
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          axis.ticks = element_blank()) 


# Compute bool for suitability each month 
clim_df$bool_jap <- ifelse(clim_df$R0_jap>1,1,0)
clim_df$bool_jap2 <- ifelse(clim_df$R0_jap2>1,1,0)

# aggregate per location
clim_df <- clim_df[,.(sum_jap = sum(bool_jap),
                      sum_jap2 = sum(bool_jap2),
                      avg_jap = mean(bool_jap),
                      avg_jap2 = mean(bool_jap2)), by = list(id)]
saveRDS(setDT(clim_df), "~/INVASIBILITY_THRESHOLD/data/landcover/clim_land23_RM.Rds")

# Load df and join with lon lat
clim_df <- readRDS("~/INVASIBILITY_THRESHOLD/data/landcover/clim_land_RM.Rds")
clim_df <- readRDS("~/INVASIBILITY_THRESHOLD/data/landcover/clim_land2318_RM.Rds")

grid_points <-readRDS( "~/INVASIBILITY_THRESHOLD/data/landcover/grid_points.Rds")
clim_df <- clim_df %>% left_join(grid_points)

# plot ------------------------------------------------
library(RColorBrewer)
name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal[11]
pal[12] = "#74011C"
pal[13] = "#4B0011"
letsize = 16
# clim_df_sp <- st_as_sf(clim_df,coords = c("lon", "lat"))
# ggplot(clim_df_sp) +
#   geom_sf(aes(color = as.factor(sum_jap)), size = 0.1) +
#   scale_color_manual(values = pal,
#                     name = "Nº months\n suitable",
#                     limits = factor(seq(0,12,1)),
#                     na.value = "white") +
#   theme_bw()
library(ggthemes)
ggplot(clim_df,aes(x = lon, y = lat,
                   fill = as.factor(sum_jap))) +
  geom_raster() +
  scale_fill_manual(values = pal,
                    name = "Nº months\n suitable",
                    limits = factor(seq(0,12,1)),
                    na.value = "white") + 
  theme_minimal() + theme(legend.position = "right",
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          axis.ticks = element_blank()) 

# annual average
ggplot(clim_df,aes(x = lon, y = lat,
                     fill = avg_jap)) +
  geom_raster() +
  scale_fill_distiller(palette = "Spectral",
                       na.value = "white",
                       name = TeX("$R_M$")) + 
  theme_minimal() + theme(legend.position = "right",
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          axis.ticks = element_blank()) 
