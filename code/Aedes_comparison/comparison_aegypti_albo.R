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

# R0 function by temperature:
R0_func_alb <- function(rain,hum,Te){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  deltaa <- 1/lf_f_alb(Te)
  probla <- pEA_f_alb(Te)
  h <- h_f(hum,rain)
  deltE = 0.1
  R0 <- sqrt(f*(a/deltaa)*probla*(h*(h+deltE)))
  return(R0)
}


#### -------------------------- Aegypti ------------------------- ####
## Thermal responses Aedes Aegypti from Mordecai 2017:
a_f_aeg <- function(temp){Briere_func(0.000202,13.35,40.08,temp)} # Biting rate
EFD_f_aeg <- function(temp){Briere_func(0.00856,14.58,34.61,temp)} # Fecundity
pEA_f_aeg <- function(temp){Quad_func(0.00599,13.56,38.29,temp)} # Survival probability Egg-Adult
MDR_f_aeg <- function(temp){Briere_func(0.0000786,11.36,39.17,temp)} # Mosquito Development Rate
lf_f_aeg <- function(temp){Quad_func(0.148,9.16,37.73,temp)} # Adult life span

vec <- seq(0,40,1)
# R0 function by temperature:
R0_func_aeg <- function(rain,hum,Te){
  a <- a_f_aeg(Te)
  f <- EFD_f_aeg(Te)
  deltaa <- 1/lf_f_aeg(Te)
  probla <- pEA_f_aeg(Te)
  h <- h_f(hum,rain)
  deltE = 0.1
  R0 <- sqrt(f*(1/deltaa)*probla*(h*(h+deltE)))
  return(R0)
}

R0_func_te_aeg <- function(Te){R0_func_aeg(400,8,Te)}
R0_func_te_alb <- function(Te){R0_func_alb(400,8,Te)}
vec <- seq(0,40,0.1)
R0_aeg <- sapply(vec,R0_func_te_aeg)
R0_alb <- sapply(vec,R0_func_te_alb)
df_aeg <- data.frame(Temp = vec, aeg = R0_aeg/max(R0_aeg),alb = R0_alb/max(R0_alb))
colnames(df_aeg) <- c("Temperature", "Aedes aegypti", "Aedes albopictus")
df_merge <- reshape2::melt(df_aeg, id.var = "Temperature")
ggplot(df_merge) +
  geom_line(aes(Temperature,value, color = variable))  + theme_bw() + 
  theme(text = element_text(size = 15),
        legend.text=element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = c(0.12, 0.8)) +
  ylab("Relative R0") + scale_x_continuous(limits = c(10,40), breaks = c(10,15,20,25,30,35,40)) +
    scale_color_manual(name = "",
                       values = c("#4C2A85", "#FF8811"))
