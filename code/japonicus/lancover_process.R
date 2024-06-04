# Code to extract the landcover for japonicus
# from Corine landcover: https://land.copernicus.eu/en/products/corine-land-cover/clc2018/download-by-area
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, tidyverse, 
               sf, RColorBrewer,exactextractr,
               data.table, raster,mapSpain)

g <- gc(reset = T)
rm(list = ls())

# Load raster
Path <- "~/INVASIBILITY_THRESHOLD/data/landcover/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif"
landcover <- rast(Path)
plot(landcover) # Plot the raster
landcover
df_cat <- levels(landcover)[[1]]

# crop Italy from raster
it <- geodata::gadm(country = 'IT', level = 0,
                      path = 'tmpr')
plot(it)
it <- project(it, crs(landcover))
it_rast <- crop(landcover, it)
plot(it_rast)

# Compute rater percentage for each landcover ---------------------------
# Load weather raster template
Path_dir <- paste0("~/Documentos/PHD/2024/Colonization/data/weather/era5/")
clim_df <- rast(paste0(Path_dir, "template_era5_land.grib"))

# Filter raster Category
it_rast_cat <- it_rast %in% df_cat[1,2]
it_rast_cat <- as.numeric(it_rast_cat)

# Project and compute sum of areas of small squares. 
# example in:https://stackoverflow.com/questions/77421977/weighted-sum-in-terraproject-what-are-weights
it_rast_per <- project(it_rast_cat, crs(clim_df), method= "sum")
# plot(it_rast_per)
# Loop through all categories and compute percentage then add as raster layer
for(i in c(2:nrow(df_cat))){
  print(paste0("i:",i))
  # Filter raster Category
  it_rast_cat <- it_rast %in% df_cat[i,2]
  it_rast_cat <- as.numeric(it_rast_cat)
  
  # Project and compute sum of areas of small squares. 
  # example in:https://stackoverflow.com/questions/77421977/weighted-sum-in-terraproject-what-are-weights
  rast_aux <- project(it_rast_cat, crs(clim_df), method= "sum")
  # plot(it_rast_per)
  
  # Add as a layer to the raster
  it_rast_per <- list(it_rast_per, rast_aux)
  it_rast_per <- rast(it_rast_per)
}

# Save raster with area of each landcover each layer Italy
writeRaster(it_rast_per, 
            "/home/marta/Documentos/PHD/2024/Landcover-Aedes/data/Aedes-landcover/landcover_area_it.tif")

it_rast_area <-rast("/home/marta/Documentos/PHD/2024/Landcover-Aedes/data/Aedes-landcover/landcover_area_it.tif")

# Test if its correct
par(mfrow=c(3,2))
ind <- 23
plot(it_rast_area[[ind]])
it_rast_cat <- it_rast %in% df_cat[ind,2]
plot(it_rast_cat)

# Transform to percentage
max(it_rast_area)
it_rast_per <- it_rast_area/1.164094 # From Area to percentage

# Save raster with percentage of each landcover each layer Italy
writeRaster(it_rast_per, 
            "/home/marta/Documentos/PHD/2024/Landcover-Aedes/data/Aedes-landcover/landcover_perc_it.tif")

it_rast_per <-rast("/home/marta/Documentos/PHD/2024/Landcover-Aedes/data/Aedes-landcover/landcover_perc_it.tif")

# Load PA Data
Dir <- "~/Kor_jap/data/"
Path <- paste0(Dir,"Italian\ AIMs.csv")
df_it <- read.csv(Path, sep = ",",
                  header = TRUE)

# Transform data traps to shapefile
df_it$Waypoint.E <- as.numeric(df_it$Waypoint.E)
df_it$Waypoint.N <- as.numeric(df_it$Waypoint.N)
df_it <- df_it[is.na(df_it$Waypoint.E) == FALSE,]
df_it <- df_it[is.na(df_it$Waypoint.N) == FALSE,]
mat_it <- as.matrix(df_it[,c("Waypoint.E","Waypoint.N")])
df_it <- st_as_sf(df_it, coords = c("Waypoint.E","Waypoint.N"),
                  crs = 4326)


# Extract landcover percentage for each cell with ovitrap
par(mfrow=c(1,1))
plot(it_rast_per[[2]])
plot(df_it[,"Ae..koreicus"], add=TRUE)
df_perc_pa <- extract(it_rast_per,mat_it, method= "simple")

# Compute percentage after extract
df_perc_pa <- df_perc_pa/rowSums(df_perc_pa)

# Test if percentage makes sense
