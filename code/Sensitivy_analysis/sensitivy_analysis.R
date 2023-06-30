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

### Difference between fecundity and bitting rate:
# bitting rate
vec <- seq(0,40,0.01)
aegypti <- sapply(vec,a_f_aeg)
albopictus <- sapply(vec,a_f_alb)
df_out <- data.frame(vec, aegypti, albopictus)
ggplot(df_out) + 
  geom_point(aes(vec,albopictus))

df_out <- reshape2::melt( df_out, id.vars = "vec")

ggplot(df_out) + 
  geom_point(aes(vec,value, color=variable)) +
  ylab("bitting rate") +
  theme_bw()
 
# Fecundity
vec <- seq(0,40,0.01)
aegypti <- sapply(vec,EFD_f_aeg)
albopictus <- sapply(vec,TFD_f_alb)
df_out <- data.frame(vec, aegypti, albopictus)
ggplot(df_out) + 
  geom_point(aes(vec,albopictus))

df_out <- reshape2::melt( df_out, id.vars = "vec")

ggplot(df_out) + 
  geom_point(aes(vec,value, color=variable)) +
  ylab("Fecundity") +
  theme_bw()

# Fecundity*bitting
vec <- seq(10,40,0.01)
aegypti <- sapply(vec,EFD_f_aeg)
albopictus1 <- sapply(vec,a_f_alb)
albopictus <- sapply(vec,TFD_f_alb)
df_out <- data.frame(vec, aegypti = aegypti, albopictus = albopictus*albopictus1)
scale <- max(df_out$albopictus)/max(df_out$aegypti)
df_out <- data.frame(vec, aegypti = aegypti,
                     albopictus = (1/scale)*albopictus*albopictus1)

df_out <- reshape2::melt( df_out, id.vars = "vec")

ggplot(df_out) + 
  geom_point(aes(vec,value, color=variable)) +
  ylab("Fecundity*bitting") +
  theme_bw()

# Life span
vec <- seq(10,40,0.01)
aegypti <- sapply(vec,lf_f_aeg)
albopictus <- sapply(vec,lf_f_alb)
japonicus <- sapply(vec,lf_f_jap)

df_out <- data.frame(vec, aegypti = aegypti,
                     albopictus = albopictus,
                     japonicus = japonicus)
df_out <- reshape2::melt( df_out, id.vars = "vec")

ggplot(df_out) + 
  geom_line(aes(vec,value, color=variable), size = 0.8) +
  ylab("Lifespan adult mosquito") + 
  theme_bw()

# p_LA
vec <- seq(10,40,0.01)
aegypti <- sapply(vec,pLA_f_aeg)
albopictus <- sapply(vec,pLA_f_alb)
japo1 <- sapply(vec,dL_f_jap)
japo2 <- sapply(vec,deltaL_f_jap)

df_out <- data.frame(vec, aegypti = aegypti,
                     albopictus = albopictus,
                     japonicus = japo1/(japo1+japo2))
df_out <- reshape2::melt( df_out, id.vars = "vec")

ggplot(df_out) + 
  geom_line(aes(vec,value, color=variable)) +
  ylab("pLA") +
  theme_bw()

# dE
vec <- seq(10,40,0.01)
aegypti <- sapply(vec,dE_f_aeg)
albopictus <- sapply(vec,dE_f_alb) 
japonicus <- sapply(vec,dE_f_jap) 

df_out <- data.frame(vec, aegypti = aegypti,
                     albopictus = albopictus,
                     japonicus = japonicus)
df_out <- reshape2::melt( df_out, id.vars = "vec")

ggplot(df_out) + 
  geom_line(aes(vec,value, color=variable)) +
  ylab("dE") +
  theme_bw()

# R0
vec <- seq(5,40,0.001)
aegypti <- sapply(vec,R0_func_aeg, hum = 500,rain = 8)
albopictus <- sapply(vec,R0_func_alb, hum = 500,rain = 8) 
japonicus <- sapply(vec,R0_func_jap, hum = 500,rain = 8) 

df_out <- data.frame(vec, aegypti = aegypti,
                     albopictus = albopictus,
                     japonicus = japonicus)
df_out <- reshape2::melt( df_out, id.vars = "vec")

ggplot(df_out) + 
  geom_line(aes(vec,value, color=variable)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  ylab("R0") +
  theme_bw()

#------------------------------Albopictus------------------------------------#
# R0 with hacthing rate
vec <- seq(0,30,0.01)
hum_cte <- 2000
te_cte <- 15
out <- sapply(vec,R0_func_alb,hum=hum_cte, Te=te_cte)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

# hatching rate plot
out <- sapply(vec,h_f,hum=hum_cte)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

# Human density versus R0 
vec <- seq(0,30000,0.01)
rain_cte <- 8
te_cte <- 15
out <- sapply(vec,function(x){R0_func_alb(rain_cte,x,te_cte)})

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

# Derivative with respect to Temperature
vec <- seq(0,40,0.0001)
hum_cte <- 500
rain_cte <- 8
out_alb <- sapply(vec,R0_func_alb,hum=hum_cte, rain=rain_cte)

df_out_alb <- data.frame(vec, out_alb)
plot_t <- ggplot(df_out_alb) +
  geom_line(aes(vec,out_alb)) + 
  xlab("Temperature(Cº)") + ylab("R0") +
  theme_bw()
plot_t

ind <- df_out_alb[which(df_out_alb$out_alb != 0),1]
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

#####------ Aegypti ------------##
# Derivative with respect to Temperature
vec <- seq(0,40,0.0001)
hum_cte <- 500
rain_cte <- 8
out_aeg <- sapply(vec,R0_func_aeg,hum=hum_cte, rain=rain_cte)

df_out_aeg <- data.frame(vec, out_aeg)
plot_t <- ggplot(df_out_aeg) +
  geom_line(aes(vec,out_aeg)) + 
  xlab("Temperature(Cº)") + ylab("R0") +
  theme_bw()
plot_t

ind <- df_out_aeg[which(df_out_aeg$out_aeg != 0),1]
numd1_t <- grad(function(x){R0_func_aeg(rain_cte,hum_cte,x)}, ind)
devf_t <- data.frame(ind,numd1_t)
dev_t <- ggplot(devf_t) +
  geom_line(aes(ind,numd1_t)) + 
  xlab("Temperature(Cº)") + ylab("dR0/dT") +
  theme_bw()

# Derivative with respect to Rainfall
vec <- seq(0,20,0.01)
hum_cte <- 500
te_cte <- 22
out <- sapply(vec,R0_func_aeg,hum=hum_cte, Te=te_cte)
df_out <- data.frame(vec, out)
plot_r <- ggplot(df_out) +
  geom_line(aes(vec,out))+ 
  xlab("Rainfall(mm)") + ylab("R0") +
  theme_bw()
ind <- df_out[which(df_out$out != 0),1]
numd1_r <- grad(function(x){R0_func_aeg(x,hum_cte,te_cte)}, ind)
devf_r <- data.frame(ind,numd1_r)
dev_r <- ggplot(devf_r) +
  geom_line(aes(ind,numd1_r))  + 
  xlab("Rainfall(mm)") + ylab("dR0/dR") +
  theme_bw()

# Derivative with respect to Human density
vec <- seq(0,1500,1)
rain_cte <- 8
te_cte <- 22
out <- sapply(vec,R0_func_aeg,rain=rain_cte, Te=te_cte)
df_out <- data.frame(vec, out)
plot_h <- ggplot(df_out) +
  geom_line(aes(vec,out)) + 
  xlab("Human density(km2)") + ylab("R0") +
  theme_bw()

ind <- df_out[which(df_out$out != 0),1]
numd1_h <- grad(function(x){R0_func_aeg(rain_cte,x,te_cte)}, ind)
devf_h <- data.frame(ind,numd1_h)
dev_h <- ggplot(devf_h) +
  geom_line(aes(ind,numd1_h)) + 
  xlab("Human density(km2)") + ylab("dR0/dH") +
  theme_bw()

library(ggpubr)
ggarrange(dev_t,dev_r,dev_h)
ggarrange(plot_t,plot_r,plot_h)

##### --------------------- JAPONICUS---------------------#
vec = seq(0,40,0.001)
df <- data.frame(vec, out <- sapply(vec,dL_f_jap))
ggplot(df) +
  geom_point(aes(vec,out))

# Derivative with respect to Temperature
vec <- seq(0,40,0.0001)
hum_cte <- 500
rain_cte <- 8
out_jap <- sapply(vec,R0_func_jap,hum=hum_cte, rain=rain_cte)

df_out_jap <- data.frame(vec, out_jap)
plot_t <- ggplot(df_out_jap) +
  geom_line(aes(vec,out_jap)) + 
  xlab("Temperature(Cº)") + ylab("R0") +
  theme_bw()
plot_t

ind <- df_out_jap[which(df_out_jap$out_jap != 0),1]
numd1_t <- grad(function(x){R0_func_jap(rain_cte,hum_cte,x)}, ind)
devf_t <- data.frame(ind,numd1_t)
dev_t <- ggplot(devf_t) +
  geom_line(aes(ind,numd1_t)) + 
  xlab("Temperature(Cº)") + ylab("dR0/dT") +
  theme_bw()

# Derivative with respect to Rainfall
vec <- seq(0,20,0.01)
hum_cte <- 500
te_cte <- 22
out <- sapply(vec,R0_func_jap,hum=hum_cte, Te=te_cte)
df_out <- data.frame(vec, out)
plot_r <- ggplot(df_out) +
  geom_line(aes(vec,out))+ 
  xlab("Rainfall(mm)") + ylab("R0") +
  theme_bw()
ind <- df_out[which(df_out$out != 0),1]
numd1_r <- grad(function(x){R0_func_jap(x,hum_cte,te_cte)}, ind)
devf_r <- data.frame(ind,numd1_r)
dev_r <- ggplot(devf_r) +
  geom_line(aes(ind,numd1_r))  + 
  xlab("Rainfall(mm)") + ylab("dR0/dR") +
  theme_bw()

# Derivative with respect to Human density
vec <- seq(0,1500,1)
rain_cte <- 8
te_cte <- 22
out <- sapply(vec,R0_func_jap,rain=rain_cte, Te=te_cte)
df_out <- data.frame(vec, out)
plot_h <- ggplot(df_out) +
  geom_line(aes(vec,out)) + 
  xlab("Human density(km2)") + ylab("R0") +
  theme_bw()

ind <- df_out[which(df_out$out != 0),1]
numd1_h <- grad(function(x){R0_func_jap(rain_cte,x,te_cte)}, ind)
devf_h <- data.frame(ind,numd1_h)
dev_h <- ggplot(devf_h) +
  geom_line(aes(ind,numd1_h)) + 
  xlab("Human density(km2)") + ylab("dR0/dH") +
  theme_bw()

library(ggpubr)
ggarrange(dev_t,dev_r,dev_h)
ggarrange(plot_t,plot_r,plot_h)

df_out_tot <- data.frame(temp = df_out_jap$vec, 
                         Albopictus=df_out_alb$out_alb/max(df_out_alb$out_alb),
                         Aegypti=df_out_aeg$out_aeg/max(df_out_aeg$out_aeg),
                         Japonicus=df_out_jap$out_jap/max(df_out_jap$out_jap))


df_plot <- reshape2::melt(df_out_tot,id.vars="temp")
ggplot(df_plot) +
  geom_line(aes(temp,value,color=variable), size = 0.8) +
  xlim(c(8,40)) + theme_bw() + ylab("Relative R0") +
  xlab("Temperature(Cº)")

