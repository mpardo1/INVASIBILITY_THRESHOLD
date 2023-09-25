## Code that Compare the PA data for albopictus in comparison with
# the number o months in which R0>1 and the avg R0
rm(list=ls())
library(mapSpain)
library(ggplot2)
library("ggpubr")

# general functions ------------------------------------------------
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
# Incorporating rain and human density -------------------------------------------
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

# Albopictus --------------------------------------------------------------------
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

# Aegypti --------------------------------------------------------------------
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


# Japonicus ---------------------------------------------------------------
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

# R_M ---------------------------------------------------------------------
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
plot_temp <- ggplot(df_out) + 
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

# rm as a function of rainfall ------------------------------------------
vec <- seq(0,16,0.001)
rain <- sapply(vec,R0_func_aeg, hum = 0, Te = 15.5)
df_rain <- data.frame(vec, rain)
plot_rain <- ggplot(df_rain) + 
  geom_line(aes(vec,rain)) +
  xlab("Rainfall, mm") + ylab(TeX("$R_M$")) + 
  theme_bw() + theme(text = element_text(size = letsize))

# rm as a function of human density ------------------------------------------
vec <- seq(0,1000,0.1)
hum <- sapply(vec,R0_func_aeg, rain = 0, Te = 15.5)
df_hum <- data.frame(vec, hum)
plot_hum <- ggplot(df_hum) + 
  geom_line(aes(vec,hum)) +
  xlab("Human density") + ylab(TeX("$R_M$")) + 
  theme_bw() + theme(text = element_text(size = letsize))

# join all the plots ------------------------------------------------------
ggarrange(plot_temp + ggtitle("A"),
          plot_rain + rremove("ylab")+ ggtitle("B"),
          plot_hum + rremove("ylab")+ ggtitle("C"),
          ncol = 3,
          widths = c(1,0.7,0.7))

