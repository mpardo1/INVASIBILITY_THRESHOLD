rm(list=ls())
library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)
library("viridis")
library("gganimate")

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

Lin_func <- function(cte, cte1, temp){
  outp <- temp*cte + cte1
  if(outp < 0 | is.na(outp)){
    outp <- 0.00001
  }
  return(outp)
}


Quad <- function(cte, cte1,cte2, temp){
  outp <- cte*temp^2 + cte1*temp + cte2
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

#### -------------------------- Albopictus ------------------------- ####
## Thermal responses Aedes Albopictus from Mordecai 2017:
a_f_alb <- function(temp){Briere_func(0.000193,10.25,38.32,temp)} # Biting rate
TFD_f_alb <- function(temp){Briere_func(0.0488,8.02,35.65,temp)} # Fecundity
pLA_f_alb <- function(temp){Quad_func(0.002663,6.668,38.92,temp)} # Survival probability Egg-Adult
MDR_f_alb <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_f_alb <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span
dE_f_alb <- function(temp){Briere_func(0.00006881,8.869,35.09,temp)} # Adult life span

# R0 function by temperature:
R0_func_alb <- function(Te, rain, hum){
  a <- a_f_alb(Te)
  f <- (1/2)*TFD_f_alb(Te)
  deltaa <- lf_f_alb(Te)
  dE <- dE_f_alb(Te)
  probla <- pLA_f_alb(Te)
  h <- h_f(hum,rain)
  deltaE = 0.1
  #R0 <- ((0.3365391*f*a*deltaa)*probla*(h*dE/(h*dE+deltaE)))^(1/3)
  R0 <- ((f*a*deltaa)*probla*(h*dE/(h*dE+deltaE)))^(1/3)
  return(R0)
}

####------------------------------Aegypti------------------------####
a_f_aeg <- function(temp){Briere_func(0.000202,13.35,40.08,temp)} # Biting rate
EFD_f_aeg <- function(temp){Briere_func(0.00856,14.58,34.61,temp)} # Fecundity
pLA_f_aeg <- function(temp){Quad_func(0.004186,9.373,40.26,temp)} # Survival probability Egg-Adult
MDR_f_aeg <- function(temp){Briere_func(0.0000786,11.36,39.17,temp)} # Mosquito Development Rate
lf_f_aeg <- function(temp){Quad_func(0.148,9.16,37.73,temp)} # Adult life span
dE_f_aeg <- function(temp){Briere_func(0.0003775 ,14.88,37.42,temp)} # Adult life span

# R0 function by temperature:
R0_func_aeg <- function(Te, rain,hum){
  a <- a_f_aeg(Te)
  f <- 40#(1/2)*EFD_f_aeg(Te)
  deltaa <- lf_f_aeg(Te)
  dE <- dE_f_aeg(Te)
  probla <- pLA_f_aeg(Te)
  h <- h_f(hum,rain)
  deltE = 0.1
  R0 <- ((f*a*deltaa)*probla*(h*dE/(h*dE+deltE)))^(1/3)
  return(R0)
}


#####----------------Japonicus-----------------####
dE_f_jap <- function(temp){Briere_func(0.0002859,6.360,35.53 ,temp)} # Mosquito Development Rate
dL_f_jap <- function(temp){Briere_func(7.000e-05,9.705e+00,3.410e+01,temp)} # Survival probability Egg-Adult
lf_f_jap <- function(temp){Lin_func(-2.5045,82.6525,temp)} # Adult life span
deltaL_f_jap <- function(temp){QuadN_func(0.0021476,-0.0806067 ,1.0332455,temp)} # Adult life span

# R0 function by temperature:
R0_func_jap <- function(Te, rain,hum){
  a <- 0.35
  f <- 40 #183/2
  lf <- lf_f_jap(Te)
  deltaL <- deltaL_f_jap(Te)
  deltE = 0.1
  dE <- dE_f_jap(Te)
  dL <- dL_f_jap(Te)
  h <- h_f(hum,rain)
  if(dL == 0 | f == 0 | a == 0 | dE == 0 |  Te<0){
    R0 <- 0
  }else{
    R0 <- ((f*a*lf)*(dL/(dL+deltaL))*(h*dE/(h*dE+deltE)))^(1/3)
  }
  return(R0)
}

## Path 
Path <- "~/INVASIBILITY_THRESHOLD/output/mcera5/process_hourly_daily_ERA5_daily_mcera_2022.Rds"
ho_R0 <- readRDS(Path)
ggplot(ho_R0) + 
  geom_line(aes(date,R0_alb_mean_h))
ho_R0$month <- lubridate::month(as.Date(ho_R0$date))

## Population 2022:
Path <- "~/INVASIBILITY_THRESHOLD/output/pop/pop22.Rds"
esp_can_pop <- readRDS(Path)

# Spain maps municipalities:
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

## Join
ho_R0 <- esp_can_pop %>% left_join(ho_R0)
ho_R0$dens <- as.numeric(ho_R0$POB22/ho_R0$area)
ho_R0[, R0_dai_alb := mapply(R0_func_alb, tmean, prec1, dens)]
ho_R0_mon <- ho_R0[, .(tmean = mean(tmean),
                             tmin = min(tmean),
                             tmax = max(tmean),
                             prec1 = sum(prec1), 
                             dens = min(dens),
                             dens1 = max(dens),
                             R0_h_mon = mean(R0_alb_mean_h),
                             R0_d_mon = mean(R0_dai_alb)), 
                         by=list(NATCODE,month)]

ho_R0_mon$bool_R0_h <- ifelse(ho_R0_mon$R0_h_mon<1,0,1)
ho_R0_mon$bool_R0_d <- ifelse(ho_R0_mon$R0_d_mon<1,0,1)
ho_R0_y <- ho_R0_mon[, .(tmean = mean(tmean),
                       tmin = min(tmean),
                       tmax = max(tmean),
                       dens = min(dens),
                       R0_h_sum = sum(bool_R0_h),
                       R0_d_sum = sum(bool_R0_d)), 
                   by=list(NATCODE)]

ho_R0_y <- esp_can %>% left_join(ho_R0_y)
library("latex2exp")
colors <- c("#43A2CA", "#7BCCC4", "#BAE4BC", "#F0F9E8",
            "#FFF7EC","#FEE8C8","#FDD49E","#FDBB84",
            "#FC8D59","#EF6548","#D7301F", "#B30000",
            "#7F0000") 
plot_sum_alb <- ggplot(ho_R0_y) +
  geom_sf(aes(fill = as.factor(R0_h_sum)), colour = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_manual(values = colors,
                    name = "Nº months\n suitable",
                    limits = factor(seq(0,12,1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

plot_sum_alb
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/SM/AlbMonthSumHourly.png")
ggsave(Path, plot = plot_sum_alb)

plot_sum_alb <- ggplot(ho_R0_y) +
  geom_sf(aes(fill = as.factor(R0_d_sum)), colour = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_manual(values = colors,
                    name = "Nº months\n suitable",
                    limits = factor(seq(0,12,1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

plot_sum_alb
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/SM/AlbMonthSumDaily.png")
ggsave(Path, plot = plot_sum_alb)
