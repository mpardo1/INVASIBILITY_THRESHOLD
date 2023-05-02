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
  deltaa <- lf_f_alb(Te)
  probla <- pEA_f_alb(Te)
  h <- h_f(hum,rain)
  deltE = 0.1
  R0 <- sqrt(f*(a*deltaa)*probla*(h/(h+deltE)))
  return(R0)
}
# Main functions 
Briere_df <- function(cte, tmin, tmax, temp){
  outp <- cte*(2*temp - tmin)*(tmax - temp)^(1/2) - ((1/2)*cte*(temp^2-tmin*temp)*(tmax-temp)^(-(1/2)))
  
  return(outp)
}

Quad_df <- function(cte, tmin, tmax, temp){
  outp <- -cte*(2*temp - (tmax + tmin))
  
  return(outp)
}


#### -------------------------- Albopictus ------------------------- ####
## Thermal responses Aedes Albopictus from Mordecai 2017:
a_f_alb <- function(temp){Briere_func(0.000193,10.25,38.32,temp)} # Biting rate
TFD_f_alb <- function(temp){Briere_func(0.0488,8.02,35.65,temp)} # Fecundity
pEA_f_alb <- function(temp){Quad_func(0.00361,9.04,39.33,temp)} # Survival probability Egg-Adult
MDR_f_alb <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_f_alb <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span

#-------------------------------------------------------------------#
# Check if the numerical derivatives are working
vec <- seq(13,30,0.1)
briere_num <- grad(function(temp){Briere_func(0.000193,10.25,38.32,temp)}, vec)
briere_fun <- sapply(vec, function(temp){Briere_func(0.000193,10.25,38.32,temp)} )
briere_dev <- sapply(vec, function(temp){Briere_df(0.000193,10.25,38.32,temp)} )

df_brie <- data.frame(vec, briere_num,briere_dev)
df_plot_brie <- reshape2::melt(df_brie, id.vars = "vec")
plot_brier <- ggplot(df_plot_brie) +
  geom_point(aes(vec, value, color=variable))


vec <- seq(13,30,0.1)
quad_num <- grad(function(temp){Quad_func(0.000193,10.25,38.32,temp)}, vec)
quad_fun <- sapply(vec, function(temp){Quad_func(0.000193,10.25,38.32,temp)} )
quad_dev <- sapply(vec, function(temp){Quad_df(0.000193,10.25,38.32,temp)} )

df_quad <- data.frame(vec, quad_num,quad_dev)
df_plot_quad <- reshape2::melt(df_quad, id.vars = "vec")
plot_quad <- ggplot(df_plot_quad) +
  geom_point(aes(vec, value, color=variable, shape = variable))

ggarrange(plot_brier, plot_quad)
#-------------------------------------------------------------------#
a_df_alb <- function(temp){Briere_df(0.000193,10.25,38.32,temp)} # Biting rate
TFD_df_alb <- function(temp){Briere_df(0.0488,8.02,35.65,temp)} # Fecundity
pEA_df_alb <- function(temp){Quad_df(0.00361,9.04,39.33,temp)} # Survival probability Egg-Adult
MDR_df_alb <- function(temp){Briere_df(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_df_alb <- function(temp){Quad_df(1.43,13.41,31.51,temp)} # Adult life span

# Derivate of R0
R0_dfunc_alb <- function(rain,hum,Te){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  deltaa <- lf_f_alb(Te)
  probla <- pEA_f_alb(Te)
  h <- h_f(hum,rain)
  deltE = 0.1
  R0 <- f*deltaa*a*probla*(h/(h+deltE))
  dffT <- TFD_df_alb(Te)
  dfaT <- a_df_alb(Te)
  dfdeltaAT <- lf_df_alb(Te)
  dfplaT <- pEA_df_alb(Te)
  dffR0 <- (1/2)*(1/sqrt(R0))*((deltaa*a*h*probla)/(h+deltE))*dffT
  dfaR0 <- (1/2)*(1/sqrt(R0))*((deltaa*f*h*probla)/(h+deltE))*dfaT
  dfdeltAR0 <- (1/2)*(1/sqrt(R0))*((f*a*h*probla)/(h+deltE))*dfdeltaAT
  dfpLAR0 <- (1/2)*(1/sqrt(R0))*((deltaa*a*h*f)/(h+deltE))*dfplaT
  
  dfR0 <- dffR0 + dfaR0 + dfdeltAR0 + dfpLAR0
  return(dfR0)
}

hum_cte <- 500
rain_cte <- 8
vec <- seq(13.5,31,0.1)
R0df_num <- grad(function(x){R0_func_alb(rain_cte,hum_cte,x)}, vec)
R0df_ana <- sapply(vec, function(x){R0_dfunc_alb(rain_cte,hum_cte,x)} )
R0f_ana <- sapply(vec, function(x){R0_func_alb(rain_cte,hum_cte,x)} )
df_R0 <- data.frame(vec,R0f_ana)
ggplot(df_R0) +
  geom_point(aes(vec,R0f_ana)) +
  theme_bw()
df_R0df <- data.frame(vec, R0df_num,R0df_ana)
df_R0df <- reshape2::melt(df_R0df, id.vars = "vec")
plot_quad <- ggplot(df_R0df) +
  geom_point(aes(vec, value, color=variable, shape = variable)) +
  ylim(c(-10,10)) + theme_bw()
plot_quad
