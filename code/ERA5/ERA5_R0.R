rm(list=ls())
library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)

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


#### -------------------------- Albopictus ------------------------- ####
### Incorporating rain and human density:
h_f <- function(hum, rain){
  # Constants: 
  erat = 0.5
  e0 = 1.5
  evar = 0.05
  eopt = 8
  efac = 0.01
  edens = 0.01
  
  
  hatch <- (1-erat)*(((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) + e0)) +
    erat*(edens/(edens + exp(-efac*hum)))
  return(hatch)
}

## Thermal responses Aedes Albopictus from Mordecai 2017:
a_f_alb <- function(temp){Briere_func(0.000193,10.25,38.32,temp)} # Biting rate
TFD_f_alb <- function(temp){Briere_func(0.0488,8.02,35.65,temp)} # Fecundity
pLA_f_alb <- function(temp){Quad_func(2.663e-03,6.668e+00,3.892e+01,temp)} # Survival probability Egg-Adult
MDR_f_alb <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_f_alb <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span
dE_f_alb <- function(temp){4.66e-03*temp -4.23e-02} # Adult life span

# R0 function by temperature:
R0_func_alb <- function(Te, rain, hum){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  deltaa <- lf_f_alb(Te)
  dE <- dE_f_alb(Te)
  probla <- pLA_f_alb(Te)
  h <- h_f(hum,rain)
  deltaE = 0.1
  R0 <- sqrt(f*(a*deltaa)*probla*(h*dE/(h*dE+deltaE)))
  return(R0)
}

func_R0 <- function(ind){
  print(paste0("ind:",ind))
  # Specify desired single point (within the bounds of your .nc file) 
  x <- esp_can$centroid[ind][[1]][1]
  y <- esp_can$centroid[ind][[1]][2]
  
  # Gather all hourly variables, with spatial and temporal dimensions
  # matching the extent, or a subset, of data in one downloaded file 
  st_time <- lubridate::ymd("2020:01:01")
  en_time <- lubridate::ymd("2020:12:31")
  point_out <- extract_clim(nc = my_nc, long = x,
                            lat = y, start_time = st_time,
                            end_time = en_time) 
  
  point_out <- point_out[,c("obs_time", "temperature")]
  point_out$NATCODE <- esp_can$NATCODE[ind]
  point_out$pop <- esp_can$pob19[ind]
  # You can then inspect the data frame
  # Gather daily precipitation 
  point_out_precip <- extract_precip(nc = my_nc, long = x, 
                                     lat = y, start_time = st_time, 
                                     end_time = en_time, convert_daily = FALSE)
  
  point_out$prec <- point_out_precip
  point_out$R0 <- -1
  for(i in c(1:nrow(point_out))){
    point_out$R0[i] <- R0_func_alb(point_out$temperature[i],
                                   point_out$prec[i],
                                   point_out$pop[i])
  }
  
  return(point_out)
}

## Test package mcera5
 uid <- "187470"
 cds_api_key <- "fbef7343-1aef-44c1-a7c3-573285248e5d"

 ecmwfr::wf_set_key(user = uid,
                    key = cds_api_key,
                    service = "cds")
 # Designate your desired bounding coordinates (in WGS84 / EPSG:4326)
 xmn <- -19
 xmx <- 4.5
 ymn <- 27
 ymx <- 44

 # Temporal grid
 st_time <- lubridate::ymd("2020:01:01")
 en_time <- lubridate::ymd("2020:12:31")

 file_prefix <- "era5_Spain_2020"
 file_path <- getwd()

 req <- build_era5_request(xmin = xmn, xmax = xmx,
                           ymin = ymn, ymax = ymx,
                          start_time = st_time,
                           end_time = en_time,
                           outfile_name = file_prefix)

request_era5(request = req, uid = uid, out_path = file_path)

# List the path of an .nc file that was downloaded via
# request_era5()
my_nc <- paste0(getwd(), "/era5_Spain_2020.nc")
# my_nc = "/home/marta/era5_Spain_2020.nc"

# Compute centroid for each municipality
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
esp_can$centroid <- st_centroid(esp_can$geometry)
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
esp_can$geometry <- NULL
census <- mapSpain::pobmun19
esp_can <- esp_can %>% left_join(census,
                                 by = c("cmun" = "cmun","cpro" = "cpro"))
esp_can <- esp_can[,c("NATCODE", "centroid", "pob19")]
# Number of cores used in the parallelization
num_cores = 1
# Parallelize function in order to obtain value R0 for each municipality
R0_each_muni <- mclapply(c(1:nrow(esp_can)), 
                         func_R0, 
                         mc.cores = num_cores)

saveRDS(R0_each_muni,
        "~/INVASIBILITY_THRESHOLD/output/R0/R0_ERA5_hourly_mcera_2020.Rds")
# modified_df <- do.call(rbind, R0_each_muni)
rm(census)
head(R0_each_muni[[1000]])

df_group <- data.table()
for(i in c(1:length(R0_each_muni))){
  print(paste0("i:",i))
  dt_aux <- setDT(R0_each_muni[[i]])
  dt_aux$date <- as.Date(dt_aux$obs_time)
  dt_aux <- dt_aux[, .(temp=mean(temperature),
                       min_temp = min(temperature),
                       max_temp =max(temperature),
                                     prec = sum(prec), 
                                     pop=min(pop),
                       R0_mean_hourly = mean(R0)), 
                       by=list(NATCODE,date)]
  dt_aux[, R0_daily_comp := mapply(R0_func_alb, temp, prec, pop)]
  df_group <- rbind(dt_aux,df_group)
}
saveRDS(df_group,
        "~/INVASIBILITY_THRESHOLD/output/R0/R0_ERA5_daily_mcera_2020.Rds")

#-----------------------------Create plots------------------------#
library("viridis")
library("gganimate")

esp_can <- esp_get_munic_siane(moveCAN = FALSE)
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_group$month <- lubridate::month(df_group$date)
df_group <- esp_can %>% left_join(df_group)

ggplot(df_group) +
  geom_sf(aes(fill = R0_mean_hourly), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month))

df_group$bool_R0 <- ifelse(df_group$R0_mean_hourly < 1,0,1)
df_group_y <- df_group %>% group_by(NATCODE) %>%
  summarise(R0_sum = sum(bool_R0))

library(RColorBrewer)

ggplot(df_group_y) +
  geom_sf(aes(fill = R0_sum), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral") +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void()
