### Code to do the sensitivity analysis for the R0
# it computes numerically the derivatives of R0 with respect of 
# each variable I have check that this numerical derivatives are
# equals to the ones done by hand (done in sensitivity_analytic.R)
# Also, there is a plot for each variable in the R0 to see its shape 
# for the three species.
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(dplyr)
library("ggpubr")
library(viridis)
library(gdata)
library("data.table")
library('numDeriv')
library("ggbreak")
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
## Thermal responses Aedes Albopictus from Mordecai 2017 and from literature:
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
  deltaa <- 1/lf_f_alb(Te)
  dE <- dE_f_alb(Te)
  probla <- pLA_f_alb(Te)
  h <- h_f(hum,rain)
  deltaE = 0.1
  #R0 <- ((0.3365391*f*a*deltaa)*probla*(h*dE/(h*dE+deltaE)))^(1/3)
  R0 <- ((f*a*(1/deltaa))*probla*(h*dE/(h*dE+deltaE)))^(1/3)
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
  R0 <- ((f*a*lf)*(dL/(dL+deltaL))*(h*dE/(h*dE+deltE)))^(1/3)
  R0 <- ifelse(is.na(R0),0,R0)
  return(R0)
}


##--------------Variable comparison between species----------------#
### Difference between fecundity and bitting rate:
# biting rate
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

###---------------------------------------------------------##
#------------------------- R0---------------------------##
vec <- seq(5,40,0.001)
aegypti <- sapply(vec,R0_func_aeg, hum = 500,rain = 8)
albopictus <- sapply(vec,R0_func_alb, hum = 500,rain = 8) 
japonicus <- sapply(vec,R0_func_jap, hum = 500,rain = 8) 

df_out <- data.frame(vec, aegypti = aegypti,
                     albopictus = albopictus,
                     japonicus = japonicus)
df_out <- reshape2::melt( df_out, id.vars = "vec")

library(RColorBrewer)
name_pal = "Set1"
display.brewer.pal(3, name_pal)
pal <- brewer.pal(3, name_pal)
letsize = 16
library("latex2exp")
ggplot(df_out) + 
  geom_line(aes(vec,value, color=variable), size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  ylab(TeX("$R_M$")) + 
  scale_color_manual(name = "", values =pal,
                     labels = c(expression(italic("Ae. aegypti")),
                                expression(italic("Ae. albopictus")),
                                expression(italic("Ae. japonicus")))) +
  xlab("Temperature") +
  scale_x_continuous(breaks = seq(5,41,4)) +
  theme_bw() + theme(legend.position = c(0.18,0.75),
                     text = element_text(size = letsize),
                     legend.text.align = 0)

##----------------Max,Min, PEAK VALUES---------------#
# Compute the min and max temperature for suitability and
# the temperature for the suitability peak for the three species
specie = "albopictus"
df_aux <- df_out[which(df_out$variable == specie),]
min(df_aux[which(df_aux$value >1), "vec"])
max(df_aux[which(df_aux$value >1), "vec"])
df_aux[which(df_aux[, "value"] == max(df_aux[, "value"])), "vec"]

#-----------------Albopictus--------------------------#
## R0
vec <- seq(5,35,0.01)
hum_cte <- 2000
rain_cte <- 3
out <- sapply(vec,R0_func_alb,hum=hum_cte, rain=3)

library("latex2exp")
df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out)) + theme_bw() +
  ylab(TeX("$R_M$")) + xlab("Temperature") +
  geom_hline(aes(yintercept = 1),
             linetype = "dashed", color = "red")+ 
  theme(legend.position = c(0.18,0.8),
    text = element_text(size = letsize),
    legend.text.align = 0) +
  scale_x_continuous(breaks=seq(5,35,5))

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
#----------------------------------------------------------#

#---------------SENSITIVITY ANALYSIS-------------------------#
## Albopictus
# Derivative with respect to Temperature
vec <- seq(0,40,0.01)
hum_cte <- 500
rain_cte <- 8
out_alb <- sapply(vec,R0_func_alb,hum=hum_cte, rain=rain_cte)

df_out_alb <- data.frame(vec, out_alb)
plot_t <- ggplot(df_out_alb) +
  geom_line(aes(vec,out_alb)) + 
  xlab("Temperature(Cº)") + ylab("R0") +
  theme_bw()
plot_t

ind <- df_out_alb[which(df_out_alb$out_alb != 0),"vec"]
df_test <- df_out_alb[ind,]
numd1_t <- grad(function(x){R0_func_alb(x,rain_cte,hum_cte)}, ind)
devf_t_alb <- data.frame(ind,numd1_t)
dev_t <- ggplot(devf_t_alb ) +
  geom_line(aes(ind,numd1_t)) + 
  xlab("Temperature(Cº)") + ylab("dR0/dT") +
  theme_bw()
dev_t

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
numd1_r <- grad(function(x){R0_func_alb(te_cte,x,hum_cte)}, ind)
devf_r_alb <- data.frame(ind,numd1_r)
dev_r <- ggplot(devf_r_alb) +
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
numd1_h <- grad(function(x){R0_func_alb(te_cte, rain_cte,x)}, ind)
devf_h_alb <- data.frame(ind,numd1_h)
dev_h <- ggplot(devf_h_alb) +
  geom_line(aes(ind,numd1_h)) + 
  xlab("Human density(km2)") + ylab("dR0/dH") +
  theme_bw()

library(ggpubr)
ggarrange(dev_t,dev_r,dev_h)
ggarrange(plot_t,plot_r,plot_h)

#####------ Aegypti ------------##
# Derivative with respect to Temperature
vec <- seq(0,40,0.01)
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
numd1_t <- grad(function(x){R0_func_aeg(x,rain_cte,hum_cte)}, ind)
devf_t_aeg <- data.frame(ind,numd1_t)
dev_t <- ggplot(devf_t_aeg) +
  geom_line(aes(ind,numd1_t)) + 
  xlab("Temperature(Cº)") + ylab("dR0/dT") +
  theme_bw()
dev_t

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
numd1_r <- grad(function(x){R0_func_aeg(te_cte,x,hum_cte)}, ind)
devf_r_aeg <- data.frame(ind,numd1_r)
dev_r <- ggplot(devf_r_aeg) +
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
numd1_h <- grad(function(x){R0_func_aeg(te_cte,rain_cte,x)}, ind)
devf_h_aeg <- data.frame(ind,numd1_h)
dev_h <- ggplot(devf_h_aeg) +
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
vec <- seq(0,40,0.01)
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
numd1_t <- grad(function(x){R0_func_jap(x,rain_cte,hum_cte)}, ind[1:2300])
devf_t_jap <- data.frame(ind=ind[1:2300],numd1_t)
dev_t <- ggplot(devf_t_jap) +
  geom_line(aes(ind,numd1_t)) + 
  xlab("Temperature(Cº)") + ylab("dR0/dT") +
  theme_bw()
dev_t

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
numd1_r <- grad(function(x){R0_func_jap(te_cte,x,hum_cte)}, ind)
devf_r_jap <- data.frame(ind,numd1_r)
dev_r <- ggplot(devf_r_jap) +
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
numd1_h <- grad(function(x){R0_func_jap(te_cte,rain_cte,x)}, ind)
devf_h_jap <- data.frame(ind,numd1_h)
dev_h <- ggplot(devf_h_jap) +
  geom_line(aes(ind,numd1_h)) + 
  xlab("Human density(km2)") + ylab("dR0/dH") +
  theme_bw()

library(ggpubr)
ggarrange(dev_t,dev_r,dev_h)
ggarrange(plot_t,plot_r,plot_h)

# Colors to match the other plots in the text
alb_col = pal[2]
aeg_col = pal[1]
jap_col = pal[3]

## Create a plot with temperature derivative for the three species:
devf_t <- devf_t_alb %>% 
  left_join(devf_t_aeg, by = join_by(ind)) %>% 
  left_join(devf_t_jap, by = join_by(ind))

colnames(devf_t) <- c("temperature", "albopictus", "aegypti", "japonicus")
df_plot <- reshape2::melt(devf_t, id.vars = "temperature")
dt <- ggplot(df_plot) + 
  geom_line(aes(temperature, value, color = variable), size = 0.8) + 
  xlab("Temperature") +
  scale_color_manual(name = "", values = c(alb_col,aeg_col,jap_col),
                     labels = c( expression(italic("Ae. albopictus")),
                                 expression(italic("Ae. aegypti")),
                                 expression(italic("Ae. japonicus")))) +
  ylim(c(-5,5)) + theme_bw() + theme(legend.position = c(0.25,0.25),
                                     text = element_text(size = letsize),
                                     legend.text.align = 0)
dt

## Create a plot with rainfall derivative for the three species:
devf_r <- devf_r_alb %>% 
  left_join(devf_r_aeg, by = join_by(ind)) %>% 
  left_join(devf_r_jap, by = join_by(ind))

colnames(devf_r) <- c("rainfall", "albopictus", "aegypti", "japonicus")
df_plot <- reshape2::melt(devf_r, id.vars = "rainfall")
dr <- ggplot(df_plot) + 
  geom_line(aes(rainfall, value, color = variable), size = 0.8) +
  xlab("Rainfall") +
  scale_color_manual(name = "", values = c(alb_col,aeg_col,jap_col)) +
  theme_bw() + theme(legend.position = "none",
                                     text = element_text(size = letsize),
                                     legend.text.align = 0)
dr

## Create a plot with human density derivative for the three species:
devf_h <- devf_h_alb %>% 
  left_join(devf_h_aeg, by = join_by(ind)) %>% 
  left_join(devf_h_jap, by = join_by(ind))

colnames(devf_h) <- c("Human_density", "albopictus", "aegypti", "japonicus")
df_plot <- reshape2::melt(devf_h, id.vars = "Human_density")
dh <- ggplot(df_plot) + 
  geom_line(aes(Human_density, value, color = variable), size = 0.8) +
  xlab("Human density") +
  scale_color_manual(name = "", values = c(alb_col,aeg_col,jap_col)) +
  theme_bw() + theme(legend.position = "none",
                     text = element_text(size = letsize),
                     legend.text.align = 0)

dh

ggarrange(ggarrange(dt,dr + ylab("")),dh, ncol = 1)

#--------------------------------------------------------------------------#
## Plot the three RM together
df_out_tot <- data.frame(temp = df_out_jap$vec, 
                         Albopictus=df_out_alb$out_alb/max(df_out_alb$out_alb),
                         Aegypti=df_out_aeg$out_aeg/max(df_out_aeg$out_aeg),
                         Japonicus=df_out_jap$out_jap/max(df_out_jap$out_jap))


df_plot <- reshape2::melt(df_out_tot,id.vars="temp")
ggplot(df_plot) +
  geom_line(aes(temp,value,color=variable), size = 0.8) +
  xlim(c(8,40)) + theme_bw() + ylab("Relative R0") +
  xlab("Temperature(Cº)")

# dRM/dX*dX/dT; X each param -------------------------------------------
# Main functions 
Briere_df <- function(cte, tmin, tmax, temp){
  outp <- cte*(2*temp - tmin)*(tmax - temp)^(1/2) - ((1/2)*cte*(temp^2-tmin*temp)*(tmax-temp)^(-(1/2)))
  
  return(outp)
}

Quad_df <- function(cte, tmin, tmax, temp){
  outp <- -cte*(2*temp - (tmax + tmin))
  
  return(outp)
}


Lin_df <- function(cte, tmin, tmax, temp){
  outp <- cte
  
  return(outp)
}

QuadN_df <- function(cte, tmin, tmax, temp){
  outp <- 2*cte*temp + tmin
  
  return(outp)
}
# For albopictus ----------------------------------------------
a_f_alb <- function(temp){Briere_func(0.000193,10.25,38.32,temp)} # Biting rate
TFD_f_alb <- function(temp){Briere_func(0.0488,8.02,35.65,temp)} # Fecundity
pEA_f_alb <- function(temp){Quad_func(0.002663,6.668,38.92,temp)} # Survival probability Egg-Adult
lf_f_alb <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span
dE_f_alb <- function(temp){Briere_func(0.00006881,8.869,35.09,temp)} # Adult life span
a_df_alb <- function(temp){Briere_df(0.000193,10.25,38.32,temp)} # Biting rate
TFD_df_alb <- function(temp){Briere_df(0.0488,8.02,35.65,temp)} # Fecundity
pEA_df_alb <- function(temp){Quad_df(0.002663,6.668,38.92,temp)} # Survival probability Egg-Adult
lf_df_alb <- function(temp){Quad_df(1.43,13.41,31.51,temp)} # Adult life span
dE_df_alb <- function(temp){Briere_df(0.00006881,8.869,35.09,temp)} # Mosquito Development Rate

R0_dfunc_alb <- function(rain,hum,Te,var){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  deltaa <- lf_f_alb(Te)
  probla <- pEA_f_alb(Te)
  dE <- dE_f_alb(Te)
  h <- h_f(hum,rain)
  deltE = 0.1
  R0 <- f*deltaa*a*probla*((h*dE)/(h*dE+deltE))
  dffT <- TFD_df_alb(Te)
  dfaT <- a_df_alb(Te)
  dfdeltaAT <- lf_df_alb(Te)
  dfplaT <- pEA_df_alb(Te)
  dfdET <- dE_df_alb(Te)
  dffR0 <- (1/3)*((R0)^(-2/3))*((deltaa*a*h*dE*probla)/(h*dE+deltE))*dffT
  dfaR0 <- (1/3)*((R0)^(-2/3))*((deltaa*f*h*dE*probla)/(h*dE+deltE))*dfaT
  dfdeltAR0 <- (1/3)*((R0)^(-2/3))*((f*a*h*dE*probla)/(h*dE+deltE))*dfdeltaAT
  dfpLAR0 <- (1/3)*((R0)^(-2/3))*((deltaa*a*h*dE*f)/(h*dE+deltE))*dfplaT
  dfdER0 <- (1/3)*((R0)^(-2/3))*((deltaa*a*f*
                                    probla)*((h*(h*dE+deltE)- (h*dE*h))/(h*dE+deltE)^2))*dfdET
  dfR0 <- dffR0 + dfaR0 + dfdeltAR0 + dfpLAR0 + dfdER0
  dfR0 <- ifelse(var == "RM",dfR0,
                 ifelse(var == "a",dfaR0,
                        ifelse(var == "f",dffR0,
                               ifelse(var == "deltaA",dfdeltAR0,
                                      ifelse(var == "pLA",dfpLAR0,dfdER0)))))
  if(var == "deltaA" | var == "RM"){
    
  }else{
    dfR0 <-ifelse(is.na(dfR0),0,dfR0)
  }
  
  return(dfR0)
}

vec = seq(5,35,0.001)
var_list = c("RM","a","f","deltaA","pLA", "dE")
df_dT <- data.frame()
for(i in c(1:length(var_list))){
  R0df_ana <- sapply(vec, function(x){R0_dfunc_alb(rain_cte,hum_cte,
                                                   x,var_list[i])} )
  df_RM <- data.frame(vec= vec, out =R0df_ana)
  df_RM$var <- var_list[i]
  df_dT <- rbind(df_dT,df_RM)
}

# Add artificially to get color in the legend ---------------------
df_out_deltaL <- data.frame(vec = 5,
                            out = 0,
                            var = "deltaL")
df_out_dL <- data.frame(vec = 5,
                        out = 0,
                        var = "dL")

df_dT <- rbind(df_dT,df_out_dL,df_out_deltaL)

# Plot all curves together -----------------------------------------
library(RColorBrewer)
name_pal = "Dark2"
display.brewer.pal(8, name_pal)
pal <- brewer.pal(8, name_pal)

col_a = pal[1]
col_f = pal[2]
col_lf = pal[3]
col_deltaL = pal[4]
col_dL = pal[5]
col_dE = pal[6]
col_pLA = pal[7]
col_deltaA = pal[8]
col_R = "#000000"

# Plot
df_alb <- ggplot(df_dT) +
  geom_line(aes(vec,out, color =var), size = 1) +
  ylim(c(-10,10)) + theme_bw() +
  xlab("Temperature") + ylab(TeX("Derivative, $dR_M/dT$")) +
  scale_color_manual(name = "",
                     values = c(col_a,col_dE,col_deltaA,
                                col_deltaL,col_dL,col_f,col_pLA,col_R),
                     labels = c("a",TeX("$ d_E$"),TeX(" $ lf$"),
                                TeX(" $ \\delta_L$"),TeX(" $ d_L$"),"f", 
                                TeX( " $ p_{LA}$"), TeX( " $ R_M$") )) + 
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'))  

# Aegypti ----------------------------------------------------------
a_f_aeg <- function(temp){Briere_func(0.000202,13.35,40.08,temp)} # Biting rate
pEA_f_aeg <- function(temp){Quad_func(0.004186,9.373,40.26,temp)} # Survival probability Egg-Adult
lf_f_aeg <- function(temp){Quad_func(0.148,9.16,37.73,temp)} # Adult life span
dE_f_aeg <- function(temp){Briere_func(0.0003775 ,14.88,37.42,temp)} # Adult life span
a_df_aeg <- function(temp){Briere_df(0.000202,13.35,40.08,temp)} # Biting rate
pEA_df_aeg <- function(temp){Quad_df(0.004186,9.373,40.26,temp)} # Survival probability Egg-Adult
lf_df_aeg <- function(temp){Quad_df(0.148,9.16,37.73,temp)} # Adult life span
dE_df_aeg <- function(temp){Briere_df(0.0003775 ,14.88,37.42,temp)} # Mosquito Development Rate

R0_dfunc_aeg <- function(rain,hum,Te,var){
  a <- a_f_aeg(Te)
  f <- 40
  deltaa <- lf_f_aeg(Te)
  probla <- pEA_f_aeg(Te)
  dE <- dE_f_aeg(Te)
  h <- h_f(hum,rain)
  deltE = 0.1
  R0 <- f*deltaa*a*probla*((h*dE)/(h*dE+deltE))
  dfaT <- a_df_aeg(Te)
  dfdeltaAT <- lf_df_aeg(Te)
  dfplaT <- pEA_df_aeg(Te)
  dfdET <- dE_df_aeg(Te)
  dfaR0 <- (1/3)*((R0)^(-2/3))*((deltaa*f*h*dE*probla)/(h*dE+deltE))*dfaT
  dfdeltAR0 <- (1/3)*((R0)^(-2/3))*((f*a*h*dE*probla)/(h*dE+deltE))*dfdeltaAT
  dfpLAR0 <- (1/3)*((R0)^(-2/3))*((deltaa*a*h*dE*f)/(h*dE+deltE))*dfplaT
  dfdER0 <- (1/3)*((R0)^(-2/3))*((deltaa*a*f*
                                    probla)*((h*(h*dE+deltE)- (h*dE*h))/(h*dE+deltE)^2))*dfdET
  dfR0 <- dfaR0 + dfdeltAR0 + dfpLAR0 + dfdER0
  dfR0 <- ifelse(var == "RM",dfR0,
                 ifelse(var == "a",dfaR0,
                               ifelse(var == "deltaA",dfdeltAR0,
                                      ifelse(var == "pLA",dfpLAR0,dfdER0))))
  if(var == "deltaA"|var == "a"|var == "RM"|var == "dE"){
    
  }else{
    dfR0 <-ifelse(is.na(dfR0),0,dfR0)
  }
  
  return(dfR0)
}

vec = seq(5,40,0.0001)
var_list = c("RM","a","deltaA","pLA", "dE")
df_dT <- data.frame()
for(i in c(1:length(var_list))){
  R0df_ana <- sapply(vec, function(x){R0_dfunc_aeg(rain_cte,hum_cte,
                                                   x,var_list[i])} )
  df_RM <- data.frame(vec= vec, out =R0df_ana)
  df_RM$var <- var_list[i]
  df_dT <- rbind(df_dT,df_RM)
}

# Plot all curves together -----------------------------------------
ggplot(df_dT[df_dT$var == "dE",]) +
  geom_line(aes(vec,out, color =var), size =1) 
df_aeg <- ggplot(df_dT) +
  geom_line(aes(vec,out, color =var), size =0.8) +
  ylim(c(-6,6)) + theme_bw() +
  xlab("Temperature") + ylab(TeX("Derivative, $dR_M/dT$")) +
  scale_color_manual(name = "",
                     values = c(col_a,col_dE,col_deltaA,col_pLA,col_R),
                     labels = c("a",TeX("$ d_E$"),TeX(" $ \\delta_A$"),
                                TeX( " $ p_{LA}$"), TeX( " $ R_M$") )) + 
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'))  

# Japonicus ----------------------------------------------------------
dE_f_jap <- function(temp){Briere_func(0.0002859,6.360,35.53 ,temp)} # Mosquito Development Rate
dL_f_jap <- function(temp){Briere_func(7.000e-05,9.705e+00,3.410e+01,temp)} # Survival probability Egg-Adult
lf_f_jap <- function(temp){Lin_func(-2.5045,82.6525,temp)} # Adult life span
deltaL_f_jap <- function(temp){QuadN_func(0.0021476,-0.0806067 ,1.0332455,temp)} # Adult life span
dE_df_jap <- function(temp){Briere_df(0.0002859,6.360,35.53,temp)} # Biting rate
dL_df_jap <- function(temp){Briere_df(7.000e-05,9.705e+00,3.410e+01,temp)} # Survival probability Egg-Adult
lf_df_jap <- function(temp){Lin_df(-2.5045,82.6525,temp)} # Adult life span
deltaL_df_jap <- function(temp){QuadN_df(0.0021476,-0.0806067 ,1.0332455,temp)} # Mosquito Development Rate

R0_dfunc_jap <- function(rain,hum,Te,var){
  a <- 0.35
  f <- 40 #183/2
  deltaa <- lf_f_jap(Te)
  deltaL <- deltaL_f_jap(Te)
  deltE = 0.1
  dE <- dE_f_jap(Te)
  dL <- dL_f_jap(Te)
  h <- h_f(hum,rain)
  R0 <- ((f*a*deltaa)*(dL/(dL+deltaL))*((h*dE)/(h*dE+deltE)))
  dfdeltaAT <- lf_df_jap(Te)
  dfdeltaLT <- deltaL_df_jap(Te)
  dfdET <- dE_df_jap(Te)
  dfdLT <- dL_df_jap(Te)
  
  dfdeltaAR0 <- (1/3)*((R0)^(-2/3))*((f*a)*(dL/(dL+deltaL))*((h*dE)/(h*dE+deltE)))*dfdeltaAT
  dfdLR0 <- (1/3)*((R0)^(-2/3))*((f*a*deltaa)*(((dL+deltaL)-dL)/(dL+deltaL)^2)*((h*dE)/(h*dE+deltE)))*dfdLT
  dfdeltaLR0 <- (1/3)*((R0)^(-2/3))*((f*a*deltaa)*(-dL/(dL+deltaL)^2)*((h*dE)/(h*dE+deltE)))*dfdeltaLT
  dfdER0 <- (1/3)*((R0)^(-2/3))*(deltaa*a*f*
                                    (dL/(dL+deltaL))*((h*(h*dE+deltE)- (h*dE*h))/(h*dE+deltE)^2))*dfdET
  dfR0 <- dfdeltaAR0 + dfdeltaLR0 + dfdLR0 + dfdER0
  dfR0 <- ifelse(var == "RM",dfR0,
                 ifelse(var == "deltaA",dfdeltaAR0,
                        ifelse(var == "deltaL",dfdeltaLR0,
                               ifelse(var == "dL",dfdLR0,dfdER0))))
  if(var == "RM" | var == "dL"){
    
  }else{
    dfR0 <-ifelse(is.na(dfR0),0,dfR0)
  }
  return(dfR0)
}

vec = seq(5,40,0.01)
var_list = c("RM","deltaA","dL", "dE","deltaL")
df_dT <- data.frame()
for(i in c(1:length(var_list))){
  R0df_ana <- sapply(vec, function(x){R0_dfunc_jap(rain_cte,hum_cte,
                                                   x,var_list[i])} )
  df_RM <- data.frame(vec= vec, out =R0df_ana)
  df_RM$var <- var_list[i]
  df_dT <- rbind(df_dT,df_RM)
}

# Plot all curves together -----------------------------------------
df_jap <- ggplot(df_dT) +
  geom_line(aes(vec,out, color =var), size = 1) +
  ylim(c(-1,1)) + theme_bw() +
  xlab("Temperature") + ylab(TeX("Derivative, $dR_M/dT$")) +
  scale_color_manual(name = "",
                     values = c(col_dE,col_deltaA,col_deltaL,col_dL,col_R)) + 
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'))  

# Join all plots ---------------------------------------------------
sizelet = 13
legend_only <- get_legend(df_alb +
                            theme(legend.position = "top",
                                  legend.text = element_text(size = sizelet),
                                  text = element_text(size = sizelet)))
ggarrange(df_alb + ylab("dx/dT") + xlab("") +
            ylim(c(-0.8,1)) +
            ggtitle(expression(paste("A         ",italic("Ae. Albopitus"))))+
            theme(legend.position = "none",
                  text = element_text(size = sizelet)),
          df_aeg + ylab("") + xlab("") + 
            ylim(c(-1.5,1)) +
            ggtitle(expression(paste("B           ",
                                     italic("Ae. Aegypti")))) +
            theme(legend.position = "none",
                  text = element_text(size = sizelet)),
          df_jap  + ylab("dx/dT") + 
            ggtitle(expression(paste("C        ",
                                     italic("Ae. Japonicus"))))+
            ylim(c(-1,1)) +
            theme(legend.position = "none",
                  text = element_text(size = sizelet)),
          legend_only,
          ncol=2, nrow = 2)

