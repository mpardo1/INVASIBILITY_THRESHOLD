# Code to extract the landcover for japonicus
# from Corine landcover: https://land.copernicus.eu/en/products/corine-land-cover/clc2018/download-by-area
# Load libraries ----------------------------------------------------------
library(terra)
library(fs)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(exactextractr)
library(data.table)
library(raster)

g <- gc(reset = T)
rm(list = ls())

# Load raster
Path <- "~/INVASIBILITY_THRESHOLD/data/landcover/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif"
landcover <- rast(Path)
plot(landcover) # Plot the raster
landcover
df_cat <- levels(landcover)[[1]]

# Esp can shapefile --------------------------------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/status_2303.shp"
eu <- read_sf(path)
plot(eu[,"locCode"])

# subset raster by interest categories
categories_of_interest_vec <- c(18,21,22,23,25)
subsetted_raster_p <- landcover %in% categories_of_interest_vec[4]
plot(subsetted_raster_p)

# Check crs of shapefile and raster
crs(landcover)
crs(eu)

# Intersect the raster with each geometry of esp_can
eu <- st_transform(eu, crs = crs(landcover))
plot(eu[,"locCode"])

# intersect with polygon 
landcov_fracs <- exact_extract(landcover, eu[,"locCode"],
                               function(df) {
  df %>%
    mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
    group_by(locCode, value) %>%
    summarize(freq = sum(frac_total))
}, summarize_df = TRUE, include_cols = 'locCode', progress = FALSE)

saveRDS(landcov_fracs,"~/INVASIBILITY_THRESHOLD/data/landcov_fracs_eu.Rds")
landcov_fracs <- readRDS("~/INVASIBILITY_THRESHOLD/data/landcov_fracs_eu.Rds")

# select only specific landcover type
categories_of_interest <- categories_of_interest_vec[3]
filt_land <- landcov_fracs[landcov_fracs$value %in% categories_of_interest,]
saveRDS(filt_land, "~/INVASIBILITY_THRESHOLD/data/landcover_perc_broaded-leaved.Rds")
# filt_land <- readRDS("~/INVASIBILITY_THRESHOLD/data/landcover_perc.Rds")

# Esp can shapefile --------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

# plot result
filt_land <- esp_can %>% left_join(filt_land)
filt_land$freq <- ifelse(is.na(filt_land$freq),0,filt_land$freq)
ggplot(filt_land) + 
  geom_sf(aes(fill = freq), color = NA) +
  scale_fill_viridis_c(name = paste0("Percentage \n",
                                     df_cat[df_cat$Value == categories_of_interest,
                                            "LABEL3"])) +
  theme_bw()

