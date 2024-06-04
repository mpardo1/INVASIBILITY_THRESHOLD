rm(list=ls())
library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)
library(latex2exp)
#------------------------FUNCTIONS---------------------------#
# Main functions 
Briere_func <- function(cte, tmin, tmax, temp){
  outp <- temp*cte*(temp - tmin)*(tmax - temp)^(1/2)
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

Quad_func <- function(cte, tmin, tmax, temp){
  outp <- -cte*(temp - tmin)*(temp - tmax)
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

QuadN_func <- function(cte, c1, c2, temp){
  outp <- cte*temp^2 + c1*temp + c2
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

Lin_func <- function(cte, c1, temp){
  outp <- -cte*temp + c1
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

### Incorporating rain and human density:
h_f <- function(hum, rain){
  # Constants: 
  erat = 0.5
  e0 = 1.5
  evar = 0.05
  eopt = 8
  efac = 0.01
  edens = 0.01
  
  # hatch <- (1-erat)*(((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) + e0)) +
  #   erat*(edens/(edens + exp(-efac*hum)))
  hatch <- ((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) )
  return(hatch)
}

#### -------------------------- CULEX ------------------------- ####
## Thermal responses Culex Rachel Taylor:
a_f_cul <- function(temp){Quad_func(0.0000674,8.72,250.60,temp)} # Biting rate
TFD_f_cul <- function(temp){Quad_func(0.64,5.99,38.94,temp)} # Fecundity
deltaL_f_cul <- function(temp){1-Quad_func(0.00435,7.54,36.45,temp)} # Larva mortality
dL_f_cul <- function(temp){Briere_func(0.0000608,5.41,36.55,temp)} # larva development rate
lf_f_cul <- function(temp){Lin_func(5.24,178.32,temp)} # Adult life span
dE_f_cul <- function(temp){Lin_func(-0.031,-0.21,temp)} # Egg development rate
deltaE_f_cul <- function(temp){1-Quad_func(0.00265,-0.91,34.26,temp)} # Egg morality rate

# R0 function by temperature:
R0_func_cul <- function(Te, hum, rain){
  a <- a_f_cul(Te)
  f <- (1/2)*TFD_f_cul(Te)
  deltaa <- lf_f_cul(Te)
  dE <- dE_f_cul(Te)
  deltaE <- deltaE_f_cul(Te)
  h <- h_f(hum,rain)
  dL <- dL_f_cul(Te)
  deltaL <- deltaL_f_cul(Te)
  R0 <- sqrt(f*(a*deltaa)*(dL/(dL+deltaL))*((dE*h)/(dE*h + deltaE)))
  if(is.na(hum)){R0 <- NA}
  return(R0)
}

### Plot bitting rate, a
vec <- seq(8,30,0.01)
out <- sapply(vec,a_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))


### Plot bitting rate, a
vec <- seq(8,30,0.01)
out <- sapply(vec,deltaE_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

### Plot fecundity, F
vec <- seq(0,40,0.01)
out <- sapply(vec,TFD_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

### Plot Larva morality
vec <- seq(0,40,0.01)
out <- sapply(vec,deltaL_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

### Plot Larva development rate
vec <- seq(0,40,0.01)
out <- sapply(vec,dL_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

### Plot development rate egg, dE
vec <- seq(0,40,0.01)
out <- sapply(vec,dE_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

### Plot adult longevity, lf
vec <- seq(0,40,0.01)
out <- sapply(vec,lf_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

## R0
vec <- seq(0,40,0.01)
out <- sapply(vec,R0_func_cul, rain = 6, hum = 500)

letsize = 14
df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out)) + 
  geom_hline(aes(yintercept = 1), linetype = "dashed", color = "red") +
  xlab("Temperature, Cº") + ylab(TeX("$R_M$")) + xlim(c(5,35)) +
  theme_bw() + theme(text = element_text(size = letsize)) 

df_out[which(df_out$out == max(df_out$out)),"vec"]

# Compute R0 for culex
clim_pop <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_clim_",2020,".Rds"))
clim_pop[, R0_cul := mapply(R0_func_cul, tmean, pop, prec)]

# Save lon lat
grid_points <- unique(clim_pop[,c("id", "lon", "lat")])

# test 
ggplot(clim_pop[clim_pop$month == 1,]) +
  geom_raster(aes(x = lon, y = lat, 
                  fill = R0_cul),alpha = 1) +
  scale_fill_viridis_c() + theme_minimal()

# Create bool and aggregate
clim_pop$bool_cul <- ifelse(clim_pop$R0_cul>1,1,0)
clim_pop <- clim_pop[,.(sum_cul = sum(bool_cul)), by = list(id)]

# add the lon lat -------------------------------------
clim_pop <- clim_pop %>% left_join(grid_points)

# Plot R0_cul
library(RColorBrewer)
name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal[11]
pal[12] = "#74011C"
pal[13] = "#4B0011"
letsize = 16
ggplot(clim_pop) +
  geom_raster(aes(x = lon, y = lat, 
                  fill = as.factor(sum_cul)),alpha = 1) +
  scale_fill_manual(values = pal,
                    name = "Nº suitable \n months",
                    limits = factor(seq(0,12,1)),
                    na.value = "#FCFCFC") +
  xlab("") + ylab("") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks.length = unit(0, "null"),
        axis.ticks.margin = unit(0, "null"), 
        # legend.position = "none",
        legend.box = "horizontal") +
  coord_equal()


# Data set with England
rm(list = ls())
library(eurostat)
library(terra)
library(tidyverse)
library(data.table)
library(sf)
library(giscoR)
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=form

# Load population density from: https://sedac.ciesin.columbia.edu/data/collection/gpw-v4
Path <- "~/INVASIBILITY_THRESHOLD/data/gpw_v4_population_density_rev11_2020_2pt5_min.tif"
pop <- rast(Path)
plot(log(pop))

# Crop the extent
exact_extent <- c(xmin = -25, xmax = 40, ymin = 25, ymax = 75)
pop_eu <- crop(pop,exact_extent)
plot(log(pop_eu))

# Remove non European countries
sf_eu <- gisco_get_countries(year = "2020", region = "Europe")
sf_eu <- sf_eu[sf_eu$CNTR_ID != "BY" &
                 sf_eu$CNTR_ID != "RU"  & sf_eu$CNTR_ID != "IS", ]
plot(sf_eu[,"CNTR_ID"])
pop_eu <- terra::crop(pop_eu, sf_eu) %>% terra::mask(., sf_eu)
writeRaster(pop_eu,paste0("~/INVASIBILITY_THRESHOLD/data/pop_eu.tif"),
            overwrite=TRUE)

# read population density europe -----------------------------
# path <- "~/INVASIBILITY_THRESHOLD/data/pop/Eurostat_Census-GRID_2021_V1-0/ESTAT_OBS-VALUE-T_2021_V1-0.tiff"
# pop_eu <- rast(path)
# plot(pop_eu[[1]])

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
time = "2041-2060"
var = "tmin"
Path <- paste0("~/INVASIBILITY_THRESHOLD/data/future-climate/",
               var,"_mean",time,".tif")
tmin_w = rast(Path)
exact_extent <- c(xmin = -25, xmax = 40, ymin = 25, ymax = 75)
tmin_w <- crop(tmin_w, exact_extent)

# change coordinate system to crop ---------------------------
temp_eu <- terra::project(temp_eu,tmin_w, method = "average")
pop_eu <- terra::project(pop_eu,tmin_w, method = "near")
rain_eu <- terra::project(rain_eu,tmin_w, method = "average")

plot(pop_eu[[1]])
plot(temp_eu[[1]])
plot(rain_eu[[5]])

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
rain$prec <- rain$prec*1000
pop <- terra::extract(pop_eu,
                      grid_points, xy =TRUE)[,c(1:2)]
colnames(pop) <- c("id", "pop")

# join df -----------------------------------------------------
clim <- temp %>% left_join(rain)
clim_pop <- setDT(clim %>% left_join(pop))

# create df europe climate monthly data -----------------------
grid_points$id <- c(1:nrow(grid_points))
clim_pop <- clim_pop %>% left_join(grid_points)

# compute R0 --------------------------------------------------
clim_pop[, R0_alb := mapply(R0_func_alb, tmean, prec, pop)]
clim_pop[, R0_aeg := mapply(R0_func_aeg, tmean, prec, pop)]
# clim_pop[, R0_jap := mapply(R0_func_jap, tmean, prec, pop)]
saveRDS(clim_pop,
        paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_clim_same_coords_",2020,".Rds"))