## Aggregate to daily scale: From the hourly computed R0 and weather 
# files it aggregated to daily other wise the files are to heavy to manage
rm(list=ls())
library(mcera5)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)
### Function

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

# vec = seq(0,40,0.1)
# out <- unlist(lapply(vec,a_f_alb))
# max(out)
# vec = seq(0,40,0.1)
# out <- unlist(lapply(vec,TFD_f_alb))
# max(out)
# vec = seq(0,40,0.1)
# out <- unlist(lapply(vec,lf_f_alb))
# max(out)
# vec = seq(0,40,0.1)
# out <- unlist(lapply(vec,dE_f_alb))
# max(out)
# vec = seq(0,40,0.1)
# out <- unlist(lapply(vec,pLA_f_alb))
# max(out)
# vec = seq(0,10,0.1)
# out <- unlist(lapply(vec,h_f, hum=0))
# max(out)

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
#----------------------------------------------------------------------#
## Read the data for the R0 computed daily:
Path <- "~/INVASIBILITY_TH RESHOLD/output/pop/pop22.Rds"
esp_can_pop <- readRDS( Path)

extract_weather <- function(year_n){
  Path <- paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/ERA5_daily_mcera_",year_n,".Rds")
  df_group <- readRDS(Path)
  # head(df_group[[5000]])
  dt_weather <- data.table()
  for(i in c(1:length(df_group))){
    print(paste0("i:",i))
    dt_aux <- setDT(df_group[[1]])
    dt_aux$date <- as.Date(dt_aux$obs_time)
    dt_aux$prec <- as.numeric(dt_aux$prec)
    dt_aux <- dt_aux %>% left_join(esp_can_pop)
    dt_aux$dens <- as.numeric(dt_aux$POB22/dt_aux$area)
    dt_aux$prec1 <- as.numeric(dt_aux$prec1)
    dt_aux[, R0_h_alb := mapply(R0_func_alb, temperature, prec1, dens)]
    dt_aux[, R0_h_aeg := mapply(R0_func_aeg, temperature, prec1, dens)]
    dt_aux[, R0_h_jap := mapply(R0_func_jap, temperature, prec1, dens)]
    dt_aux <- dt_aux[,.(tmean = mean(temperature),
                        tmin = min(temperature),
                        tmax = max(temperature),
                        R0_alb_mean_h = mean(R0_h_alb),
                        R0_aeg_mean_h = mean(R0_h_aeg),
                        R0_jap_mean_h = mean(R0_h_jap),
                        prec =sum(prec),
                        prec1 = sum(prec1),
                        pop = min(POB22),
                        area= min(area)), by = list(date,NATCODE)]
    dt_weather <- rbind(dt_aux,dt_weather)
    df_group[[1]] <- NULL
    rm(dt_aux)
  }
  return(dt_weather)
}

year = 2022
out <- extract_weather(year)
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/mcera5/process_hourly_daily_ERA5_daily_mcera_",year,".Rds")
saveRDS(out,Path)
