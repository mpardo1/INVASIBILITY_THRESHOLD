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
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")
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
