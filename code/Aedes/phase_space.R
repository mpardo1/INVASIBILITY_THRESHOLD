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
  if(dL == 0 | f == 0 | a == 0 | dE == 0 | h == 0 | Te<0){
    R0 <- 0
  }else{
    R0 <- ((f*a*lf)*(dL/(dL+deltaL))*(h*dE/(h*dE+deltE)))^(1/3)
  }
  return(R0)
}


vec <- seq(0,40,0.01)
out <- sapply(vec,R0_func_jap, rain = 6, hum = 500)

df_out_jap <- data.frame(vec, out)
ggplot(df_out_jap) +
  geom_line(aes(vec,out))

df_out[which(df_out$out == max(df_out$out)),"vec"]

df_out_aeg$esp <- "Aegypti"
df_out_alb$esp <- "Albopictus"
df_out_jap$esp <- "Japonicus"
df_out_aeg$out <- df_out_aeg$out /max(df_out_aeg$out )
df_out_alb$out <- df_out_alb$out /max(df_out_alb$out )
df_out_jap$out <- df_out_jap$out /max(df_out_jap$out )
df_out <- rbind(df_out_alb, df_out_aeg, df_out_jap)

ggplot(df_out) +
  geom_line(aes(vec, out, group = esp, color = esp)) +
  theme_bw() + xlim(c(5,36)) +
  scale_x_continuous(breaks = seq(5,35,5),limits = c(5, 36)) +
  xlab("Temperature (Cº)") + ylab("Vector suitability, R0") +
  theme(legend.position = c(0.1,0.6),
        text = element_text(size = 14),
        legend.title=element_blank())  


#### Phase space for all three species
temperature <- seq(5, 40, by = 0.1)
rainfall <- seq(0, 16, by = 0.1)
dt_ps <- setDT(expand.grid(temperature = temperature, rainfall = rainfall))
dt_ps$hum = 0
dt_ps[, Albopictus := mapply(R0_func_alb, temperature, rainfall, hum)]
dt_ps[, Japonicus := mapply(R0_func_jap, temperature, rainfall, hum)]
dt_ps[, Aegypti := mapply(R0_func_aeg, temperature, rainfall, hum)]
dt_ps$rangealb <- ifelse(dt_ps$Albopictus<1,0,
                  ifelse(dt_ps$Albopictus>=1 & dt_ps$Albopictus<2,1,
                  ifelse(dt_ps$Albopictus>=2 & dt_ps$Albopictus<3,2,
                  ifelse(dt_ps$Albopictus>=3 & dt_ps$Albopictus<4,3, 
                  ifelse(dt_ps$Albopictus>=4 & dt_ps$Albopictus<5,4,5))))) 
dt_ps$rangeaeg <- ifelse(dt_ps$Aegypti<1,0,
                  ifelse(dt_ps$Aegypti>=1 & dt_ps$Aegypti<2,1,
                  ifelse(dt_ps$Aegypti>=2 & dt_ps$Aegypti<3,2,
                  ifelse(dt_ps$Aegypti>=3 & dt_ps$Aegypti<4,3, 
                  ifelse(dt_ps$Aegypti>=4 & dt_ps$Aegypti<5,4,5))))) 
dt_ps$rangejap <- ifelse(dt_ps$Japonicus<1,0,
                  ifelse(dt_ps$Japonicus>=1 & dt_ps$Japonicus<2,1,
                  ifelse(dt_ps$Japonicus>=2 & dt_ps$Japonicus<3,2,
                  ifelse(dt_ps$Japonicus>=3 & dt_ps$Japonicus<4,3, 
                  ifelse(dt_ps$Japonicus>=4 & dt_ps$Japonicus<5,4,5))))) 
library(latex2exp)
albo <- ggplot(dt_ps) +
  geom_point(aes(temperature,rainfall,color = rangealb)) +
  scale_color_viridis_c(option = "magma", name = TeX("$$R_M$$")) + theme_bw()

aeg <- ggplot(dt_ps) +
  geom_point(aes(temperature,rainfall,color = rangeaeg)) +
  scale_color_viridis_c(option = "magma", name = TeX("$$R_M$$")) + theme_bw()

jap <- ggplot(dt_ps) +
  geom_point(aes(temperature,rainfall,color = rangejap)) +
  scale_color_viridis_c(option = "magma", name = TeX("$$R_M$$")) + theme_bw()

library(ggpubr)
ggarrange(jap + xlab("") + 
            ggtitle(expression(italic("Aedes japonicus"))),
          albo + xlab("")+ 
            ggtitle(expression(italic("Aedes albopictus"))),
          aeg + 
            ggtitle(expression(italic("Aedes aegypti"))), ncol =1, common.legend = TRUE)

# vec <- seq(0,40,0.1)
# out <- sapply(vec,h_f, hum=0)
# plot(vec,out)
