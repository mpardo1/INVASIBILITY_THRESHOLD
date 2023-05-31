rm(list=ls())
library("mapSpain")
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library("ggpubr")
library(viridis)
library(stringr)
library(gdata)
library("data.table")
library(parallel)

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$R0_test <- runif(length(esp_can$codauto),0,5)
can_box <- esp_get_can_box()

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
## Thermal responses Aedes Albopictus from Mordecai 2017:
a_f_alb <- function(temp){Briere_func(0.000193,10.25,38.32,temp)} # Biting rate
TFD_f_alb <- function(temp){Briere_func(0.0488,8.02,35.65,temp)} # Fecundity
pEA_f_alb <- function(temp){Quad_func(0.00361,9.04,39.33,temp)} # Survival probability Egg-Adult
MDR_f_alb <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_f_alb <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span
b_f_alb <- function(temp){Briere_func(0.000723,16,36.3,temp)} # Adult life span
c_f_alb <- function(temp){Briere_func(0.00433,2.8,36.7,temp)} # Adult life span
pdr_f_alb <- function(temp){Briere_func(0.00433,2.8,36.7,temp)} # Adult life span

# R0 function by temperature:
R0_func_alb <- function(Te, hum){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  mu <- 1/lf_f_alb(Te)
  mdr <- MDR_f_alb(Te)
  probla <- pEA_f_alb(Te)
  b <- b_f_alb(Te)
  c <- c_f_alb(Te)
  pdr <- pdr_f_alb(Te)
  N <- hum
  r = 0.1
  R0 <- sqrt((a^2*b*c*exp(-mu/pdr)*f*probla*mdr)/(N*r*mu^3))
  return(R0)
}

# R0 with hacthing rate
vec <- seq(0,1000,10)
te_cte <- 20
out <- sapply(vec,R0_func_alb,Te=te_cte)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

# Data table with tmed and R0 computed for albopictus and dengue
dengue_df <- readRDS("~/INVASIBILITY_THRESHOLD/output/R0/R0/R0_Dengue_2020.Rds")
# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
# Extract the population density for 2019
census <- mapSpain::pobmun19
esp_can <- esp_can %>% 
  left_join(census, by = c("cmun" = "cmun","cpro" = "cpro"))
esp_can$area <- as.numeric(st_area(esp_can))/1000000
esp_can$pop_km <- esp_can$pob19/esp_can$area
esp_can <- esp_can[,c("NATCODE","pop_km")]
dengue_df <- esp_can %>% left_join(dengue_df)
dengue_df <- setDT(dengue_df)

#Compute R0 for Spain for 2020
dengue_df[, R0_mean := mapply(R0_func_alb, tmean, pop_km)]
rm(census,df_out,esp_can)
dengue_df <- dengue_df[,c("NATCODE","R0_mean","date", "tmean","pop_km")]
dengue_df$month <- lubridate::month(dengue_df$date)
# Agrupate by month
dengue_df_g <- dengue_df[, .(R0_tmed=mean(R0_mean),
                             tmean = mean(tmean),
                             pop = min(pop_km)), 
                         by=list(NATCODE,month)]

# Plot the histogram of R0
hist(dengue_df_g$R0_tmed)
# Plot R0
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
dengue_df_g_m <- esp_can %>% left_join(dengue_df_g)
# See the map for R0
ggplot(dengue_df_g_m) +
  geom_sf(aes(fill = R0_tmed), linewidth = 0.01) +
  scale_fill_viridis_c()
# Create ranges:
dengue_df_g_m$group_R0 <- ifelse(dengue_df_g_m$R0_tmed<1 | is.na(dengue_df_g_m$R0_tmed), 0,
                                 ifelse(dengue_df_g_m$R0_tmed<25 & dengue_df_g_m$R0_tmed>=1, 1,
                                 ifelse(dengue_df_g_m$R0_tmed<50 & dengue_df_g_m$R0_tmed>=25, 2,
                                 ifelse(dengue_df_g_m$R0_tmed<100 & dengue_df_g_m$R0_tmed>=50, 3,
                                 ifelse(dengue_df_g_m$R0_tmed<200 & dengue_df_g_m$R0_tmed>=100, 4,
                                 ifelse(dengue_df_g_m$R0_tmed<=400 & dengue_df_g_m$R0_tmed>=200,5,6))))))

#-----------------FINAL MAPS--------------------#
library(gganimate)
library(RColorBrewer)
cols = brewer.pal(n = 7, name = "YlOrBr")
# # Create plots:
ggplot(dengue_df_g_m) +
  geom_sf(aes(fill = as.factor(group_R0)), linewidth = 0.01) +
  scale_fill_manual(values = cols,
                    labels = c("<1", "[1,25)", "[25,50)",
                               "[50,100)", "[100,200)", "[200,400]", ">400"),
                    name = "R0 Dengue") +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month))

# Data frame with number of months R0 >1
rm(dengue_df)
dengue_df_g_m <- dengue_df_g_m[,c("NATCODE","R0_tmed","tmean", "cpro","month")]
dengue_df_g_m <- setDT(dengue_df_g_m)
dengue_df_g_m$bool_r0 <- ifelse(dengue_df_g_m$R0_tmed<1,0,1)
dengue_df_g_bool <- dengue_df_g_m[, .(R0_sum=sum(bool_r0),
                                   tmean = mean(tmean)), 
                               by=list(NATCODE)]

cols = paletteer::paletteer_c("grDevices::Zissou 1", 13)
dengue_df_g_bool <- esp_can %>% 
  left_join(dengue_df_g_bool)

# # Create plots:
ggplot(dengue_df_g_bool) +
  geom_sf(aes(fill = as.factor(R0_sum)), linewidth = 0.01) +
  scale_fill_manual(values = cols,
                    name = "Months R0>1") +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  ggtitle("Spain risk of Dengue 2020") +
  theme_void() 

dengue_df_g_p <- dengue_df_g_m[, .(R0_tmed=mean(R0_tmed),
                             tmean = mean(tmean)), 
                         by=list(month, cpro)]
rm(dengue_df_g_m,dengue_df)

prov_esp <- esp_get_munic_siane() %>%
  mutate(
    Provincia = esp_dict_translate(ine.prov.name, "es")
  )