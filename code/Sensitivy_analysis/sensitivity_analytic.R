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

