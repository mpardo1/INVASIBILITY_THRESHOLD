# Code that extracts the future weather from CMIP6
# Tutorial: https://geofabio.com/2022/12/13/modelamiento-con-ecocrop-para-identificar-impacto-del-cambio-climatico-sobre-el-cultivo-de-cafe/
#-----------------------------------------------------#
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, tidyverse, glue, sf, RColorBrewer, 
               ggspatial, hrbrthemes, showtext, rnaturalearthdata,
               rnaturalearth, extrafont, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Font --------------------------------------------------------------------
font_add_google(family = 'Fira Sans', name = 'Fira Sans Condensed')
showtext_auto()

# Download data -----------------------------------------------------------
esp0 <- geodata::gadm(country = 'ESP', level = 0, 
                      path = 'tmpr')
# nic1 <- geodata::gadm(country = 'NIC', level = 0, 
#                       path = 'tmpr')
# plot(nic1)
plot(esp0)
vars <- c('prec', 'tmax', 'tmin')

# Download 30s ------------------------------------------------------------
prec_w <- geodata::cmip6_world(model = 'ACCESS-CM2',
                             ssp = '585', time = '2041-2060',
                             var = 'prec', path = 'tmpr', res = 2.5)

tmax_w <- geodata::cmip6_world(model = 'ACCESS-CM2',
                            ssp = '585', time = '2041-2060', 
                            var = 'tmax', path = 'tmpr', res = 2.5)

tmin_w <- geodata::cmip6_world(model = 'ACCESS-CM2',
                            ssp = '585', time = '2041-2060',
                            var = 'tmin', path = 'tmpr', res = 2.5)

# Change coordinate system to WGS84 ---------------------------------------
coord_sys <- crs("+proj=longlat +datum=WGS84")
crs(esp0) <- coord_sys
crs(prec_w) <- coord_sys
crs(tmin_w) <- coord_sys
crs(tmax_w) <- coord_sys

# Extract by mask ---------------------------------------------------------
prec_esp <- terra::crop(prec_w, esp0) %>% terra::mask(., esp0)
# plot(prec[[1]]) 
tmax_esp <- terra::crop(tmax_w, esp0) %>% terra::mask(., esp0)
tmin_esp <- terra::crop(tmin_w, esp0) %>% terra::mask(., esp0)
plot(tmax_esp[[8]])

prec_df <- terra::as.data.frame(prec_esp, xy = TRUE) 
tmax_df <- terra::as.data.frame(tmax_esp, xy = TRUE) 
tmin_df <- terra::as.data.frame(tmin_esp, xy = TRUE) 

# Transform into a sf object ----------------------------------------------
prec_sf <- st_as_sf(prec_df, coords = c('x', 'y'), remove = F)
prec_sf <- prec_sf[,c(3:ncol(prec_sf))]
colnames(prec_sf) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                       "Aug", "Sept", "Oct", "Nov", "Dec", "geometry")
tmax_sf <- st_as_sf(tmax_df, coords = c('x', 'y'), remove = F)
tmax_sf <- tmax_sf[,c(3:ncol(tmax_sf))]
colnames(tmax_sf) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                       "Aug", "Sept", "Oct", "Nov", "Dec", "geometry")
tmin_sf <- st_as_sf(tmin_df, coords = c('x', 'y'), remove = F)
tmin_sf <- tmin_sf[,c(3:ncol(tmin_sf))]
colnames(tmin_sf) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                       "Aug", "Sept", "Oct", "Nov", "Dec", "geometry")

# Spain map with NATCODE --------------------------------------------------
library(mapSpain)
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
# Transform in to the same coord. system
esp_can <- st_transform(esp_can, crs = coord_sys)

# Save data for future use --------------------------------------------------
saveRDS(esp_can, "~/INVASIBILITY_THRESHOLD/data/future-climate/esp_can.Rds")
saveRDS(prec_sf, "~/INVASIBILITY_THRESHOLD/data/future-climate/prec_sf.Rds")

# Intersect data from climate to mapSpain -----------------------------------
st_crs(prec_sf) <- st_crs(esp_can)
st_crs(tmax_sf) <- st_crs(esp_can)
st_crs(tmin_sf) <- st_crs(esp_can)
geometry <-  esp_can[,"geometry"]

