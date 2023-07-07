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
library(gganimate)
library(plto3d)
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

# R0 with temp cte
vec <- seq(0,1000,10)
te_cte <- 20
out <- sapply(vec,R0_func_alb,Te=te_cte)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

# R0 with hum density cte
vec <- seq(0,40,0.1)
hum_cte <- 20
out <- sapply(vec,R0_func_alb,hum=hum_cte)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

## 3D plot R0
library(plotly)

vec <- seq(0,40, length = 200)
vec1 <- seq(0.5,30, length = 1000)
dt_z <- setDT(expand.grid(vec,vec1))
dt_z[, R0 := mapply(R0_func_alb, Var1, Var2)]
ggplot(dt_z) + 
  geom_point(aes(Var1,Var2,color = R0)) + 
  scale_color_viridis()

library(rgl)
library(RColorBrewer)
# Create a color palette using the color_brewer package
my_colors <- c("yellow","red", "black")
my_palette <- colorRampPalette(my_colors)(8000)

library("plot3D")
plot3d(x=dt_z$Var1,y=dt_z$Var2,z=dt_z$R0, 
       col = my_palette[dt_z$R0+1],
       cex.lab=10,cex.axis=5,
       xlab = "Temperature", ylab = "Human density",
       zlab = "R0 Dengue") 

####-------------Hatching rate-----------------#
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
# Plot 3D function hatching rates
y <- seq(0, 16, length= 200)
x <- seq(0, 1000, length= 1000)
df_xy <- expand.grid(x,y)
df_xy$z <- 0
for(i in c(1:nrow(df_xy))){
  print(i)
  df_xy[i,3] <- h_f(df_xy$Var1[i], df_xy$Var2[i])
}
# 
Path <- "~/Documents/PHD/2023//Mosquito\ invasibility/outputhatching.Rds"
# saveRDS(df_xy,Path)
df_xy <- readRDS(Path)

library(rgl)
library(RColorBrewer)
# Create a color palette using the color_brewer package
my_colors <- c("yellow","red", "black")
my_palette <- colorRampPalette(my_colors)(100)

plot3d(x=df_xy$Var1,y=df_xy$Var2,z=df_xy$z,
       col = my_palette[df_xy$z * 100],
       cex.lab=10,cex.axis=5,
       xlab = "Human density (km2)", ylab = "Rainfall(mm day)",
       zlab = "Haching rate") 
