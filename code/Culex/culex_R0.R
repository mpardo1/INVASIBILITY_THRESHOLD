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
  
  hatch <- (1-erat)*(((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) + e0)) +
    erat*(edens/(edens + exp(-efac*hum)))
  return(hatch)
}

#### -------------------------- CULEX ------------------------- ####
## Thermal responses Culex Rachel Taylor:
a_f_cul <- function(temp){Quad_func(0.0000674,8.72,250.60,temp)} # Biting rate
TFD_f_cul <- function(temp){Quad_func(0.64,5.99,38.94,temp)} # Fecundity
pLA_f_cul <- function(temp){Quad_func(0.00435,7.54,36.45,temp)} # Survival probability Egg-Adult
lf_f_cul <- function(temp){Lin_func(5.24,178.32,temp)} # Adult life span
dE_f_cul <- function(temp){Lin_func(0.031,-0.21,temp)} # Adult life span

# R0 function by temperature:
R0_func_cul <- function(Te, rain, hum){
  a <- a_f_cul(Te)
  f <- TFD_f_cul(Te)
  deltaa <- lf_f_cul(Te)
  dE <- 0.1 #dE_f_cul(Te)
  probla <- pLA_f_cul(Te)
  h <- h_f(hum,rain)
  deltaE = 0.1
  R0 <- sqrt(f*(a*deltaa)*probla*(h*dE/(h*dE+deltaE)))
  return(R0)
}

### Plot bitting rate, a
vec <- seq(8,30,0.01)
out <- sapply(vec,a_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

### Plot fecundity, F
vec <- seq(0,40,0.01)
out <- sapply(vec,TFD_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

### Plot Probability from larva to adult, pLA
vec <- seq(0,40,0.01)
out <- sapply(vec,pLA_f_cul)

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

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out)) + 
  xlab("Temperature, Cº") + ylab("R0") + xlim(c(5,35)) +
  theme_bw()

df_out[which(df_out$out == max(df_out$out)),"vec"]
