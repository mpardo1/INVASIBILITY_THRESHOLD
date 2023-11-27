rm(list = ls())
library(eurostat)
library(terra)
library(tidyverse)
library(data.table)
library(sf)
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=form
# read population density europe -----------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/pop/Eurostat_Census-GRID_2021_V1-0/ESTAT_OBS-VALUE-T_2021_V1-0.tiff"
pop_eu <- rast(path)
plot(pop_eu[[1]])

# climate data europe ----------------------------------------
temp_eu <- rast("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/3monthly_aggregated_raster.tif")
plot(temp_eu[[8]])

path_w <- "~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/monthlyrain_2022.grib"
rain_eu <- rast(path_w)
plot(rain_eu[[1]])

# shapefile europe eurostats ---------------------------------
SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 3, 
                                 year = 2016)

plot(SHP_0[,"id"])

# intersection between geometries and lon lat ---------------
inter_L <- readRDS("~/INVASIBILITY_THRESHOLD/data/japonicus/pa/out_inter_d.Rds")

# map europe province level ----------------------------------
# SHP_1 <- get_eurostat_geospatial(
#   resolution = 10,
#   nuts_level = 3,
#   year = 2016)
# 
# ggplot(SHP_1) +
#   geom_sf(aes(fill = NUTS_NAME), color = NA) +
#   theme(legend.position = "none") +
#   scale_fill_viridis_d()

# change coordinate system to crop ---------------------------
pop_eu <- terra::project(pop_eu,temp_eu, method = "near")
rain_eu <- terra::project(rain_eu,temp_eu, method = "near")
plot(pop_eu[[1]])
plot(temp_eu[[1]])
plot(rain_eu[[8]])

# Create a grid of longitude and latitude values
lon <- seq(from = xmin(temp_eu), to = xmax(temp_eu),
           by = res(temp_eu)[1])
lat <- seq(from = ymin(temp_eu), to = ymax(temp_eu),
           by = res(temp_eu)[2])
grid_points <- expand.grid(lon = lon, lat = lat)

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

# transform rain into mm per squared km
rain$prec <- (rain$prec*1000)/3.47
pop <- terra::extract(pop_eu,
                       grid_points, xy =TRUE)[,c(1:2)]
colnames(pop) <- c("id", "pop")

# join df -----------------------------------------------------
clim <- temp %>% left_join(rain)
clim_pop <- setDT(clim %>% left_join(pop))

# create df europe climate monthly data -----------------------
grid_points$id <- c(1:nrow(grid_points))
clim_pop <- clim_pop %>% left_join(grid_points)
saveRDS(clim_pop,
         paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_clim_",2020,".Rds"))
# clim_pop <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_clim_",2020,".Rds"))

# compute R0 --------------------------------------------------
clim_pop[, R0_alb := mapply(R0_func_alb, tmean, prec, pop)]
clim_pop[, R0_aeg := mapply(R0_func_aeg, tmean, prec, pop)]
clim_pop[, R0_jap := mapply(R0_func_jap, tmean, prec, pop)]

clim_pop$bool_alb <- ifelse(clim_pop$R0_alb>1,1,0)
clim_pop$bool_aeg <- ifelse(clim_pop$R0_aeg>1,1,0)
clim_pop$bool_jap <- ifelse(clim_pop$R0_jap>1,1,0)

clim_pop <- clim_pop[,.(sum_alb = sum(bool_alb),
                        sum_aeg = sum(bool_aeg),
                        sum_jap = sum(bool_jap)), by = list(id)]

# add the lon lat -------------------------------------
grid_points$id <- c(1:nrow(grid_points))
clim_pop <- clim_pop %>% left_join(grid_points)

# save the df
saveRDS(clim_pop,
        paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_R0_",2020,".Rds"))
clim_pop <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_R0_",2020,".Rds"))

clim_pop_r <- rast(clim_pop, crs= crs(rain_eu), extent = ext(rain_eu))
terra::writeRaster(temp_eu_mon,
                   "~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/3monthly_aggregated_raster.tif",
                   overwrite = TRUE)
# plot ------------------------------------------------
library(RColorBrewer)
name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal[11]
pal[12] = "#74011C"
pal[13] = "#4B0011"
letsize = 16
clim_pop_sf <- st_as_sf(clim_pop,coords = c("lon", "lat"))
alb <- ggplot(clim_pop,
       aes(x = lon, y = lat,
           fill = as.factor(sum_alb))) +
  geom_tile() +
  scale_fill_manual(values = pal,
                    name = "Nº months\n suitable",
                    limits = factor(seq(0,12,1)),
                    na.value = "white") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks.length = unit(0, "null"),
        axis.ticks.margin = unit(0, "null")) 


aeg <- ggplot(clim_pop, aes(x = lon, y = lat, fill = as.factor(sum_aeg))) +
  geom_tile() +
  scale_fill_manual(values = pal,
                    name = "Nº months\n suitable",
                    limits = factor(seq(0,12,1)),
                    na.value = "white")+  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks.length = unit(0, "null"),
        axis.ticks.margin = unit(0, "null")) 

jap <- ggplot(clim_pop, aes(x = lon, y = lat, fill = as.factor(sum_jap))) +
  geom_tile() +
  scale_fill_manual(values = pal,
                    name = "Nº months\n suitable",
                    limits = factor(seq(0,12,1)),
                    na.value = "white") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks.length = unit(0, "null"),
        axis.ticks.margin = unit(0, "null")) 

library(ggpubr)
ggarrange(alb + rremove("xlab")+ rremove("ylab"),
          aeg+ rremove("xlab")+ rremove("ylab"),
          jap+ rremove("xlab")+ rremove("ylab"),
          ncol = 3, common.legend = TRUE)
