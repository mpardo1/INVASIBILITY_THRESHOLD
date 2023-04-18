rm(list=ls())
library(tidyverse)
library(ggplot2)
library(dplyr)
library("ggpubr")
library(viridis)
library(gdata)
library("data.table")
library('numDeriv')

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

# R0 with hacthing rate
vec <- seq(0,30,0.01)
hum_cte <- 2000
te_cte <- 15
out <- sapply(vec,R0_func_alb,hum=hum_cte, Te=te_cte)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))
