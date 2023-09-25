rm(list = ls())
library(raster)
library(eurostat)
library(terra)
# read population density europe -----------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/pop/Eurostat_Census-GRID_2021_V1-0/ESTAT_OBS-VALUE-T_2021_V1-0.tiff"
pop_eu <- rast(path)
plot(pop_eu[[1]])

# climate data europe ----------------------------------------
path_w <- "~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/monthlytemp_2020.grib"
temp_eu <- rast(path_w)
plot(temp_eu[[11]])

path_w <- "~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/monthlyrain_2020.grib"
rain_eu <- rast(path_w)
plot(rain_eu[[10]])

# shapefile europe eurostats ---------------------------------
SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)

# change coordinate system to crop ---------------------------
coord_sys <- crs("+proj=longlat +datum=WGS84")
crs(temp_eu) <- crs(SHP_0)
temp_eu <- terra::crop(temp_eu, SHP_0) %>% terra::mask(., SHP_0)
rain_eu <- terra::crop(rain_eu, SHP_0) %>% terra::mask(., SHP_0)

# match to raster objects ------------------------------------
pop_eu <- project(pop_eu, temp_eu)
plot(pop_eu)

# Create a grid of longitude and latitude values
lon <- seq(from = xmin(temp_eu), to = xmax(temp_eu), by = res(temp_eu)[1])
lat <- seq(from = ymin(temp_eu), to = ymax(temp_eu), by = res(temp_eu)[2])
grid_points <- expand.grid(lon = lon, lat = lat)
values1 <- extract(temp_eu[[2]], grid_points)
values2 <- extract(raster2, grid_points)
