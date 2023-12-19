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

# Esp can shapefile --------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

# subset raster by interest categories
categories_of_interest_vec <- c(18,21,22,23,25)
# categories_of_interest <- c(18,22,23,25)
subsetted_raster <- landcover #%in% categories_of_interest
subsetted_raster_p <- landcover %in% categories_of_interest_vec[4]
plot(subsetted_raster_p)
plot(landcover)

# Check crs of shapefile and raster
crs(subsetted_raster)
crs(esp_can)

# Intersect the raster with each geometry of esp_can
esp_can <- st_transform(esp_can, crs = crs(landcover))
plot(esp_can[,"NATCODE"])

# crop spain from raster
esp0 <- geodata::gadm(country = 'ESP', level = 0,
                      path = 'tmpr')
plot(esp0)
esp0 <- project(esp0, crs(landcover))
esp_rast <- crop(subsetted_raster, esp0)
plot(esp_rast)

# intersect with polygon 
plot(esp_can[1,"name"], add = TRUE)
landcov_fracs <- exact_extract(subsetted_raster, esp_can[,"NATCODE"],
                               function(df) {
  df %>%
    mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
    group_by(NATCODE, value) %>%
    summarize(freq = sum(frac_total))
}, summarize_df = TRUE, include_cols = 'NATCODE', progress = FALSE)


# test
landcov_fracs_f <- landcov_fracs[landcov_fracs$value == 23,]
# Esp can shapefile --------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

landcov_fracs_f <- esp_can %>% left_join(landcov_fracs_f)
landcov_fracs_f[is.na(landcov_fracs_f$freq), "freq" ] <- 0
ggplot(landcov_fracs_f) + geom_sf(aes(fill = freq), color = NA) + theme_bw()

# Save the Rds
saveRDS(landcov_fracs,"~/INVASIBILITY_THRESHOLD/data/landcov_fracs.Rds")
landcov_fracs <- readRDS("~/INVASIBILITY_THRESHOLD/data/landcov_fracs.Rds")

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

