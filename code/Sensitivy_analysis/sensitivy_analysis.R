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

# Derivative with respect to Temperature
vec <- seq(0,40,0.01)
hum_cte <- 500
rain_cte <- 8
out <- sapply(vec,R0_func_alb,hum=hum_cte, rain=rain_cte)

df_out <- data.frame(vec, out)
plot_t <- ggplot(df_out) +
  geom_line(aes(vec,out)) + 
  xlab("Temperature(Cº)") + ylab("R0") +
  theme_bw()

ind <- df_out[which(df_out$out != 0),1]
numd1_t <- grad(function(x){R0_func_alb(rain_cte,hum_cte,x)}, ind)
devf_t <- data.frame(ind,numd1_t)
dev_t <- ggplot(devf_t) +
  geom_line(aes(ind,numd1_t)) + 
  xlab("Temperature(Cº)") + ylab("dR0/dT") +
  theme_bw()

# Derivative with respect to Rainfall
vec <- seq(0,20,0.01)
hum_cte <- 500
te_cte <- 22
out <- sapply(vec,R0_func_alb,hum=hum_cte, Te=te_cte)
df_out <- data.frame(vec, out)
plot_r <- ggplot(df_out) +
  geom_line(aes(vec,out))+ 
  xlab("Rainfall(mm)") + ylab("R0") +
  theme_bw()
ind <- df_out[which(df_out$out != 0),1]
numd1_r <- grad(function(x){R0_func_alb(x,hum_cte,te_cte)}, ind)
devf_r <- data.frame(ind,numd1_r)
dev_r <- ggplot(devf_r) +
  geom_line(aes(ind,numd1_r))  + 
  xlab("Rainfall(mm)") + ylab("dR0/dR") +
  theme_bw()

# Derivative with respect to Human density
vec <- seq(0,1500,1)
rain_cte <- 8
te_cte <- 22
out <- sapply(vec,R0_func_alb,rain=rain_cte, Te=te_cte)
df_out <- data.frame(vec, out)
plot_h <- ggplot(df_out) +
  geom_line(aes(vec,out)) + 
  xlab("Human density(km2)") + ylab("R0") +
  theme_bw()

ind <- df_out[which(df_out$out != 0),1]
numd1_h <- grad(function(x){R0_func_alb(rain_cte,x,te_cte)}, ind)
devf_h <- data.frame(ind,numd1_h)
dev_h <- ggplot(devf_h) +
  geom_line(aes(ind,numd1_h)) + 
  xlab("Human density(km2)") + ylab("dR0/dH") +
  theme_bw()

library(ggpubr)
ggarrange(dev_t,dev_r,dev_h)
ggarrange(plot_t,plot_r,plot_h)
