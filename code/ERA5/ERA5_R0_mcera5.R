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

vec <- seq(0,40,0.01)
out <- sapply(vec,R0_func_jap, rain = 6, hum = 500)
df_out_jap <- data.frame(vec, out)
out <- sapply(vec,R0_func_alb, rain = 6, hum = 500)
df_out_alb <- data.frame(vec, out)
out <- sapply(vec,R0_func_aeg, rain = 6, hum = 500)
df_out_aeg <- data.frame(vec, out)
# Life_span mosquito
# vec <- seq(-5,40,0.01)
# out <- sapply(vec,lf_f_jap)
# 
# df_out_jap <- data.frame(vec, out)
# ggplot(df_out_jap) +
#   geom_line(aes(vec,out))

df_out[which(df_out$out == max(df_out$out)),"vec"]

df_out_aeg$esp <- "Aegypti"
df_out_alb$esp <- "Albopictus"
df_out_jap$esp <- "Japonicus"
df_out_aeg$out <- df_out_aeg$out
df_out_alb$out <- df_out_alb$out 
df_out_jap$out <- df_out_jap$out
df_out <- rbind(df_out_alb, df_out_aeg, df_out_jap)

ggplot(df_out) +
  geom_line(aes(vec, out, group = esp, color = esp)) +
  theme_bw() + 
  scale_x_continuous(breaks = seq(5,40,5),limits = c(5, 40)) +
  xlab("Temperature (Cº)") + ylab("Vector suitability, R0") +
  theme(legend.position = c(0.2,0.8),
        text = element_text(size = 14),
        legend.title=element_blank())  

df_out_aeg[which(df_out_aeg$out == max(df_out_aeg$out)),"vec"]
df_out_alb[which(df_out_alb$out == max(df_out_alb$out)),"vec"]
df_out_jap[which(df_out_jap$out == max(df_out_jap$out)),"vec"]

## Test that the function works and make sense the values should go
# from bigger to smaller
R0_func_jap(30,5,400)
R0_func_alb(15,5,400)
R0_func_aeg(15,5,400)

# Use data taken from MCERA5 Package R from ERA5 data set Copernicus
year = 2014
Path <- paste0("~/INVASIBILITY_THRESHOLD/output//mcera5/mcera5/ERA5_daily_mcera_",year,".Rds")
weather_muni <- readRDS(Path)
# weather_muni <- setDT(do.call(rbind, weather_muni))
head(weather_muni[[1000]])

df_group <- data.table()
length(weather_muni)
for(i in c(1:length(weather_muni))){
  print(paste0("i:",i))
  dt_aux <- setDT(weather_muni[[i]])
  dt_aux$prec2 <- dt_aux$prec
  dt_aux$prec <- dt_aux$prec1
  dt_aux$date <- as.Date(dt_aux$obs_time)
  dt_aux$prec <- as.numeric(dt_aux$prec)
  ## Albopictus
  dt_aux[, R0_hourly_alb := mapply(R0_func_alb, temperature, prec, pop)]
  ## Aegypti
  dt_aux[, R0_hourly_aeg := mapply(R0_func_aeg, temperature, prec, pop)]
  ## Japonicus
  dt_aux[, R0_hourly_jap := mapply(R0_func_jap, temperature, prec, pop)]
  
  dt_aux <- dt_aux[, .(temp=mean(temperature),
                       min_temp = min(temperature),
                       max_temp = max(temperature),
                       prec = sum(prec),
                       pop= min(pop),
                       R0_hourly_alb = mean(R0_hourly_alb),
                       R0_hourly_aeg = mean(R0_hourly_aeg),
                       R0_hourly_jap = mean(R0_hourly_jap)),
                   by=list(NATCODE,date)]

  ## Albopictus
  dt_aux[, R0_min_alb := mapply(R0_func_alb, min_temp, prec, pop)]
  dt_aux[, R0_max_alb := mapply(R0_func_alb, max_temp, prec, pop)]
  dt_aux[, R0_dai_alb := mapply(R0_func_alb, temp, prec, pop)]

  ## Aegypti
  dt_aux[, R0_min_aeg := mapply(R0_func_aeg, min_temp, prec, pop)]
  dt_aux[, R0_max_aeg := mapply(R0_func_aeg, max_temp, prec, pop)]
  dt_aux[, R0_dai_aeg := mapply(R0_func_alb, temp, prec, pop)]

  ## Japonicus
  dt_aux[, R0_min_jap := mapply(R0_func_jap, min_temp, prec, pop)]
  dt_aux[, R0_max_jap := mapply(R0_func_jap, max_temp, prec, pop)]
  dt_aux[, R0_dai_jap := mapply(R0_func_alb, temp, prec, pop)]

  df_group <- rbind(dt_aux,df_group)
}

Path <- paste0("~/INVASIBILITY_THRESHOLD/output/R0/1R0_ERA5_daily_mcera_",year,".Rds")
saveRDS(df_group,Path)
