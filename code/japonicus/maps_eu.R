rm(list = ls())
library(eurostat)
library(terra)
library(tidyverse)
library(data.table)
library(exactextractr)
library(sf)
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=form

# climate data europe ------------------------------------------------
# temp_eu <- rast("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/3monthly_aggregated_raster.tif")
# plot(temp_eu[[8]])
# 
# path_w <- "~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/monthlyrain_2022.grib"
# rain_eu <- rast(path_w)
# plot(rain_eu[[1]])

# European boundary ---------------------------------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/status_2303.shp"
eu_pa <- st_read(path)
plot(eu_pa[,"locCode"])

# Load land cover ------------------------------------------------------
Path <- "~/INVASIBILITY_THRESHOLD/data/landcover/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif"
landcover <- rast(Path)
df_cat <- as.data.frame(levels(landcover))

# Filter landcover to have one class -----------------------------------
# cat_name <- "Broad-leaved forest"
# cat <- df_cat[df_cat$LABEL3 == cat_name, "Value"]
# landcover_filt <- landcover %in% cat
# plot(landcover_filt)

# Intersection function ---------------------------------------------------
# func_inter <- function(nrowinit,nrowend, rast, var){
#   eu_pa <- st_transform(eu_pa, crs = crs(rast))
#   geom_val <- extract(rast, eu_pa[c(nrowinit:nrowend),"locCode"],
#                             method = "bilinear" )
#   geom_val <- geom_val[is.na(geom_val[,3]) == FALSE,]
#   geom_val <- aggregate(geom_val, by= list(geom_val$ID), FUN = mean)
#   mymonths <- c("Jan","Feb","Mar",
#                 "Apr","May","Jun",
#                 "Jul","Aug","Sep",
#                 "Oct","Nov","Dec")
#   colnames(geom_val)[3:14] <- mymonths
#   saveRDS(geom_val,
#           paste0("~/INVASIBILITY_THRESHOLD/data/",var,"_mean_eu.Rds"))
# }
# 
# # Run for Rainfall and temperature
# nrowinit <- 1
# nrowend <- nrow(eu_pa)
# rast <- rain_eu
# func_inter(nrowinit,nrowend, rast, "temp")
# func_inter(nrowinit,nrowend, rast, "rain")

# Load data temp and rain already computes
rain_eu <- readRDS("~/INVASIBILITY_THRESHOLD/data/rain_mean_eu.Rds")
temp_eu <- readRDS("~/INVASIBILITY_THRESHOLD/data/temp_mean_eu.Rds")
colnames(rain_eu)[3:14] <- c("Jan","Feb","Mar",
                                "Apr","May","Jun",
                                "Jul","Aug","Sep",
                                "Oct","Nov","Dec")
colnames(temp_eu)[3:14] <- c("Jan","Feb","Mar",
                                "Apr","May","Jun",
                                "Jul","Aug","Sep",
                                "Oct","Nov","Dec")
# Plot rain df
eu_pa$ID <- seq(1, nrow(eu_pa),1)
eu_rain <- eu_pa %>% left_join(rain_eu)

ggplot(eu_rain) +
  geom_sf(aes(fill = Apr), color = NA) + 
  scale_fill_viridis_c(option = "magma") + theme_bw()

# Plot temp df
eu_temp <- eu_pa %>% left_join(temp_eu)

ggplot(eu_temp) +
  geom_sf(aes(fill = Apr), color = NA) + 
  scale_fill_viridis_c(option = "magma") + theme_bw()

# Transform into data table 
eu_temp_dt <- setDT(eu_temp[,c(4,15:27)])
eu_temp_dt$geometry <- NULL
eu_rain_dt <- setDT(eu_rain[,c(4,15:27)])
eu_rain_dt$geometry <- NULL

# Transform to long format
eu_temp_dt <- reshape2::melt(eu_temp_dt, id.vars = c("locCode"))
eu_rain_dt <- reshape2::melt(eu_rain_dt, id.vars = c("locCode"))
eu_rain_dt$value <- eu_rain_dt$value*1000 # transform m to mm (ERA5 documentation)
colnames(eu_rain_dt)[3] <- "rain"
colnames(eu_temp_dt)[3] <- "temp"

# Group by loc and month
eu_temp_dt <- setDT(eu_temp_dt)
eu_temp_dt <- eu_temp_dt[,.(temp =mean(temp)),
                         by = list(locCode, variable)]
eu_rain_dt <- setDT(eu_rain_dt)
eu_rain_dt <- eu_rain_dt[,.(rain =mean(rain)),
                         by = list(locCode, variable)]

# Load landcover 
landcov_fracs1 <- readRDS("/home/marta/INVASIBILITY_THRESHOLD/data/landcov_fracs_eu_1_5000.Rds")
landcov_fracs2 <- readRDS("/home/marta/INVASIBILITY_THRESHOLD/data/landcov_fracs_eu_5000_nrow.Rds")
landcov_fracs <- rbind(landcov_fracs1, landcov_fracs2)

# select only specific landcover type
categories_of_interest <- c(18,23)
filt_land <- landcov_fracs[landcov_fracs$value %in% 
                             categories_of_interest,]

# Plots land cover to see the proportion
filt_land <- eu_pa %>% left_join(filt_land)
# ggplot(filt_land[filt_land$value == 18,]) +
#   geom_sf(aes(fill = freq), color = NA)+
#   scale_fill_viridis_c() +
#   theme_bw()
# 
ggplot(filt_land[filt_land$value == 23,]) +
  geom_sf(aes(fill = freq), color = NA)+
  scale_fill_viridis_c() +
  theme_bw()

# Summ preq from the multiple landcover types
filt_land$geometry <- NULL
filt_land <- setDT(filt_land[,c("locCode", "freq")])
filt_land <- filt_land %>% group_by(locCode) %>%
  summarise(freq = sum(freq))
filt_land_p <- eu_pa %>% left_join(filt_land)
filt_land_p[is.na(filt_land_p$freq), "freq"] <- 0
ggplot(filt_land_p) +
  geom_sf(aes(fill = freq), color = NA)+
  scale_fill_viridis_c() +
  theme_bw()

# Join all data tables
clim_land_dt <- eu_temp_dt %>%
  left_join(eu_rain_dt) %>% left_join(filt_land)

# Compute a boolean variable for landcover to check difference with prop
clim_land_dt$bool_land <- ifelse(clim_land_dt$freq>0.1,1,0)
  
# Compute the R_M
clim_land_dt[, R0_jap := mapply(R0_func_jap, temp, rain, freq)]
clim_land_dt[, R0_jap_2 := mapply(R0_func_jap, temp, rain, bool_land)]

clim_land_dt$bool <- ifelse(clim_land_dt$R0_jap>1 , 1, 0)
clim_land_dt$bool_2 <- ifelse(clim_land_dt$R0_jap_2>1 , 1, 0)
clim_land_dt_y <- clim_land_dt[,.(rain =mean(rain),
                            R0_sum = sum(bool),
                            R0_sum_2 = sum(bool_2),
                            temp = mean(temp),
                            freq = min(freq)),
                         by = list(locCode)]

# Join with eu shapefile and plot
saveRDS(clim_land_dt_y, "/home/marta/INVASIBILITY_THRESHOLD/data/R0_jap_land_forest_pasture.Rds")
clim_land_dt_y <- eu_pa %>% left_join(clim_land_dt_y)
clim_land_dt_y <- clim_land_dt_y[which(is.na(clim_land_dt_y$R0_sum)==FALSE) ,]

# Add color library and palette
library(RColorBrewer)
name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal[11]
pal[12] = "#74011C"
pal[13] = "#4B0011"
letsize = 16
ggplot(clim_land_dt_y) +
  geom_sf(aes(fill = as.factor(R0_sum)), color = NA) +
  scale_fill_manual(values = pal,
                    name = "Nº suitable \n months ") + theme_bw()

# Plot for the bool landcover
ggplot(clim_land_dt_y) +
  geom_sf(aes(fill = as.factor(R0_sum_2)), color = NA) +
  scale_fill_manual(values = pal,
                    name = "Nº suitable \n months ") + theme_bw()


### OTHER APPROACH ---------------------------------------
# Try with the sum of landcover with pastures not being influence by rainfall
# Load landcover 
landcov_fracs1 <- readRDS("/home/marta/INVASIBILITY_THRESHOLD/data/landcov_fracs_eu_1_5000.Rds")
landcov_fracs2 <- readRDS("/home/marta/INVASIBILITY_THRESHOLD/data/landcov_fracs_eu_5000_nrow.Rds")
landcov_fracs <- rbind(landcov_fracs1, landcov_fracs2)

# select only specific landcover type
categories_of_interest <- c(18,23)
filt_land <- setDT(landcov_fracs[landcov_fracs$value %in% 
                             categories_of_interest,])
filt_land <- reshape(filt_land,
                     idvar = "locCode",
                     timevar = "value",
                     direction = "wide")
filt_land$freq.23 <- ifelse(is.na(filt_land$freq.23),0,filt_land$freq.23)
filt_land$freq.18 <- ifelse(is.na(filt_land$freq.18),0,filt_land$freq.18)

# Join all data tables
clim_land_dt <- eu_temp_dt %>%
  left_join(eu_rain_dt) %>% left_join(filt_land)

# Compute the R_M
clim_land_dt[, R0_jap := mapply(R0_func_jap_2, temp, rain, freq.23, freq.18)]
clim_land_dt$bool <- ifelse(clim_land_dt$R0_jap>1 , 1, 0)
clim_land_dt_y <- clim_land_dt[,.(rain =mean(rain),
                                  R0_sum = sum(bool),
                                  temp = mean(temp)),
                               by = list(locCode)]

# Join with eu shapefile and plot
# saveRDS(clim_land_dt_y, "/home/marta/INVASIBILITY_THRESHOLD/data/R0_jap_land_forest_pasture.Rds")
clim_land_dt_y <- eu_pa %>% left_join(clim_land_dt_y)
clim_land_dt_y <- clim_land_dt_y[which(is.na(clim_land_dt_y$R0_sum)==FALSE) ,]

# Add color library and palette
library(RColorBrewer)
name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal[11]
pal[12] = "#74011C"
pal[13] = "#4B0011"
letsize = 16
ggplot(clim_land_dt_y) +
  geom_sf(aes(fill = as.factor(R0_sum)), color = NA) +
  scale_fill_manual(values = pal,
                    name = "Nº suitable \n months ") + theme_bw()

