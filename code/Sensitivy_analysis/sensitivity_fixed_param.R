## Code that Compare the PA data for albopictus in comparison with
# the number o months in which R0>1 and the avg R0
rm(list=ls())
library(mapSpain)
library(ggplot2)
library("ggpubr")
library(data.table)
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")

# Albopictus -----------------------------------------------------
vec <- seq(5,40,0.001)
albopictus <- sapply(vec,R0_func_alb, hum = 500,rain = 8) 


# Fixed bitting rate
cte <- max(sapply(seq(1,40,0.01),a_f_alb))

# R0 function by temperature:
R0_func_alb <- function(Te, rain, hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- cte #a_f_alb(Te)
    f <- (1/2)*TFD_f_alb(Te)
    deltaa <- lf_f_alb(Te)
    dE <- dE_f_alb(Te)
    probla <- pLA_f_alb(Te)
    h <- h_f(hum,rain)
    deltaE = deltaE_f_alb(Te)#0.1
    
    R0 <- ((f*a*deltaa)*probla*((h*dE)/(h*dE+deltaE)))^(1/3)
  }
  return(R0)
}

# Run model with fixed param
vec <- seq(5,40,0.001)
albopictus_a_cte <- sapply(vec,R0_func_alb, hum = 500,rain = 8) 

# Fixed bitting rate
cte <- (max(sapply(seq(1,40,0.01),TFD_f_alb)))

# R0 function by temperature:
R0_func_alb <- function(Te, rain, hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- a_f_alb(Te)
    f <- (1/2)*cte#TFD_f_alb(Te)
    deltaa <- lf_f_alb(Te)
    dE <- dE_f_alb(Te)
    probla <- pLA_f_alb(Te)
    h <- h_f(hum,rain)
    deltaE = deltaE_f_alb(Te)#0.1
    
    R0 <- ((f*a*deltaa)*probla*((h*dE)/(h*dE+deltaE)))^(1/3)
  }
  return(R0)
}

# Run model with fixed param
vec <- seq(5,40,0.001)
albopictus_f_cte <- sapply(vec,R0_func_alb, hum = 500,rain = 8) 

# Fixed adult mortality rate
cte <- (max(sapply(seq(1,40,0.01),lf_f_alb)))

# R0 function by temperature:
R0_func_alb <- function(Te, rain, hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- a_f_alb(Te)
    f <- (1/2)*TFD_f_alb(Te)
    deltaa <- cte#lf_f_alb(Te)
    dE <- dE_f_alb(Te)
    probla <- pLA_f_alb(Te)
    h <- h_f(hum,rain)
    deltaE = deltaE_f_alb(Te)#0.1
    
    R0 <- ((f*a*deltaa)*probla*((h*dE)/(h*dE+deltaE)))^(1/3)
  }
  return(R0)
}

# Run model with fixed param
vec <- seq(5,40,0.001)
albopictus_lf_cte <- sapply(vec,R0_func_alb, hum = 500,rain = 8) 

# Fixed Egg development rate
cte <- (max(sapply(seq(1,40,0.01),dE_f_alb)))

# R0 function by temperature:
R0_func_alb <- function(Te, rain, hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- a_f_alb(Te)
    f <- (1/2)*TFD_f_alb(Te)
    deltaa <- lf_f_alb(Te)
    dE <- cte#dE_f_alb(Te)
    probla <- pLA_f_alb(Te)
    h <- h_f(hum,rain)
    deltaE = deltaE_f_alb(Te)#0.1
    
    R0 <- ((f*a*deltaa)*probla*((h*dE)/(h*dE+deltaE)))^(1/3)
  }
  return(R0)
}

# Run model with fixed param
vec <- seq(5,40,0.001)
albopictus_dE_cte <- sapply(vec,R0_func_alb, hum = 500,rain = 8) 

# Fixed adult probability from Larvae to adult
cte <- (max(sapply(seq(1,40,0.01),pLA_f_alb)))

# R0 function by temperature:
R0_func_alb <- function(Te, rain, hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- a_f_alb(Te)
    f <- (1/2)*TFD_f_alb(Te)
    deltaa <- lf_f_alb(Te)
    dE <- dE_f_alb(Te)
    probla <- cte#pLA_f_alb(Te)
    h <- h_f(hum,rain)
    deltaE = deltaE_f_alb(Te)#0.1
    
    R0 <- ((f*a*deltaa)*probla*((h*dE)/(h*dE+deltaE)))^(1/3)
  }
  return(R0)
}

# Run model with fixed param
vec <- seq(5,40,0.001)
albopictus_pLA_cte <- sapply(vec,R0_func_alb, hum = 500,rain = 8) 

# Fixed adult Egg mortality rate
cte <- (min(sapply(seq(1,40,0.01),deltaE_f_alb)))

# R0 function by temperature:
R0_func_alb <- function(Te, rain, hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- a_f_alb(Te)
    f <- (1/2)*TFD_f_alb(Te)
    deltaa <- lf_f_alb(Te)
    dE <- dE_f_alb(Te)
    probla <- pLA_f_alb(Te)
    h <- h_f(hum,rain)
    deltaE = cte#deltaE_f_alb(Te)#0.1
    
    R0 <- ((f*a*deltaa)*probla*((h*dE)/(h*dE+deltaE)))^(1/3)
  }
  return(R0)
}

# Run model with fixed param
vec <- seq(5,40,0.001)
albopictus_deltaE_cte <- sapply(vec,R0_func_alb, hum = 500,rain = 8) 

# Create data frame with all vecs
df_cte <- data.frame(vec,  
           albopictus_a_cte, albopictus_dE_cte,
           albopictus_f_cte, albopictus_lf_cte,
           albopictus_pLA_cte, albopictus_deltaE_cte,
           albopictus)
colnames(df_cte) <- c("Temperature",  "a", "dE",
                      "f", "deltaA", "pLA", "deltaE", "No cte")
df_cte <- reshape2::melt( df_cte, id.vars = "Temperature")
library(RColorBrewer)
name_pal = "Set1"
display.brewer.pal(7, name_pal)
pal <- brewer.pal(7, name_pal)
pal <- c("#00798c","#d1495b", "#edae49", "#66a182",
         "#1C0da2", "#8d96a3", "#000000")
letsize = 16
library("latex2exp")
plot_temp <- ggplot(df_cte) + 
  geom_line(aes(Temperature,value, color=variable), size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  ylab(TeX("$R_M$")) + 
  scale_color_manual(name = "", values =pal) +
                     # ,
                     # labels = c(expression(italic("Ae. aegypti")),
                     #            expression(italic("Ae. albopictus")))) +
  # ,
  # expression(italic("Ae. japonicus")))) +
  xlab("Temperature (Cº)") +
  scale_x_continuous(breaks = seq(5,41,4)) +
  theme_bw() + theme(legend.position = c(0.18,0.75),
                     text = element_text(size = letsize),
                     legend.text.align = 0)

plot_temp

# Aegypti ---------------------------------------------------------------
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R") # Reload
vec <- seq(5,40,0.001)
aegypti <- sapply(vec,R0_func_aeg, hum = 500,rain = 8) 

# Fixed bitting rate
cte <- max(sapply(seq(1,40,0.01),a_f_aeg))

# R0 function by temperature:
R0_func_aeg <- function(Te, rain,hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- cte#a_f_aeg(Te)
    f <- 40#EFD_f_aeg(Te) #40
    deltaa <- lf_f_aeg(Te)
    dE <- dE_f_aeg(Te)
    probla <- pLA_f_aeg(Te)
    h <- h_f(hum,rain)
    deltaE = deltaE_f_aeg(Te)
    R0 <- ((f*a*deltaa)*probla*((h*dE)/(h*dE+deltaE)))^(1/3)
  }
  return(R0)
}

# Run model with fixed param
vec <- seq(5,40,0.001)
aegypti_a_cte <- sapply(vec,R0_func_aeg, hum = 500,rain = 8) 

# Fixed adult mortality rate
cte <- (max(sapply(seq(1,40,0.01),lf_f_aeg)))

# R0 function by temperature:
R0_func_aeg <- function(Te, rain,hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- a_f_aeg(Te)
    f <- 40#EFD_f_aeg(Te) #40
    deltaa <- cte#lf_f_aeg(Te)
    dE <- dE_f_aeg(Te)
    probla <- pLA_f_aeg(Te)
    h <- h_f(hum,rain)
    deltaE = deltaE_f_aeg(Te)
    R0 <- ((f*a*deltaa)*probla*((h*dE)/(h*dE+deltaE)))^(1/3)
  }
  return(R0)
}

# Run model with fixed param
vec <- seq(5,40,0.001)
aegypti_lf_cte <- sapply(vec,R0_func_aeg, hum = 500,rain = 8) 

# Fixed Egg development rate
cte <- (max(sapply(seq(1,40,0.01),dE_f_aeg)))

# R0 function by temperature:
R0_func_aeg <- function(Te, rain,hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- a_f_aeg(Te)
    f <- 40#EFD_f_aeg(Te) #40
    deltaa <- lf_f_aeg(Te)
    dE <- cte#dE_f_aeg(Te)
    probla <- pLA_f_aeg(Te)
    h <- h_f(hum,rain)
    deltaE = deltaE_f_aeg(Te)
    R0 <- ((f*a*deltaa)*probla*((h*dE)/(h*dE+deltaE)))^(1/3)
  }
  return(R0)
}

# Run model with fixed param
vec <- seq(5,40,0.001)
aegypti_dE_cte <- sapply(vec,R0_func_aeg, hum = 500,rain = 8) 

# Fixed adult probability from Larvae to adult
cte <- (max(sapply(seq(1,40,0.01),pLA_f_aeg)))

# R0 function by temperature:
R0_func_aeg <- function(Te, rain,hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- a_f_aeg(Te)
    f <- 40#EFD_f_aeg(Te) #40
    deltaa <- lf_f_aeg(Te)
    dE <- dE_f_aeg(Te)
    probla <- cte#pLA_f_aeg(Te)
    h <- h_f(hum,rain)
    deltaE = deltaE_f_aeg(Te)
    R0 <- ((f*a*deltaa)*probla*((h*dE)/(h*dE+deltaE)))^(1/3)
  }
  return(R0)
}

# Run model with fixed param
vec <- seq(5,40,0.001)
aegypti_pLA_cte <- sapply(vec,R0_func_aeg, hum = 500,rain = 8) 

# Fixed adult Egg mortality rate
cte <- (min(sapply(seq(1,40,0.01),deltaE_f_aeg)))

# R0 function by temperature:
R0_func_aeg <- function(Te, rain,hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- a_f_aeg(Te)
    f <- 40#EFD_f_aeg(Te) #40
    deltaa <- lf_f_aeg(Te)
    dE <- dE_f_aeg(Te)
    probla <- pLA_f_aeg(Te)
    h <- h_f(hum,rain)
    deltaE = cte#deltaE_f_aeg(Te)
    R0 <- ((f*a*deltaa)*probla*((h*dE)/(h*dE+deltaE)))^(1/3)
  }
  return(R0)
}

# Run model with fixed param
vec <- seq(5,40,0.001)
aegypti_deltaE_cte <- sapply(vec,R0_func_aeg, hum = 500,rain = 8) 

# Create data frame with all vecs
df_cte <- data.frame(vec,  
                     aegypti_a_cte, aegypti_dE_cte,
                    aegypti_lf_cte,
                     aegypti_pLA_cte, aegypti_deltaE_cte,
                     aegypti)
colnames(df_cte) <- c("Temperature",  "a", "dE",
                       "deltaA", "pLA", "deltaE", "No cte")
df_cte <- reshape2::melt( df_cte, id.vars = "Temperature")
library(RColorBrewer)
# name_pal = "Set1"
# display.brewer.pal(7, name_pal)
# pal <- c("#00798c","#d1495b", "#edae49", "#66a182",
#          "#2e4057", "#8d96a3", "#000000")
# pal <- brewer.pal(7, name_pal)[c(1:5,7)]
pal <- pal[c(1:5,7)]

letsize = 16
library("latex2exp")
plot_temp_aeg <- ggplot(df_cte) + 
  geom_line(aes(Temperature,value, color=variable), size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  ylab(TeX("$R_M$")) + 
  scale_color_manual(name = "", values =pal) +
  # ,
  # labels = c(expression(italic("Ae. aegypti")),
  #            expression(italic("Ae. aegopictus")))) +
  # ,
  # expression(italic("Ae. japonicus")))) +
  xlab("Temperature (Cº)") +
  scale_x_continuous(breaks = seq(5,41,4)) +
  theme_bw() + theme(legend.position = c(0.18,0.75),
                     text = element_text(size = letsize),
                     legend.text.align = 0)

plot_temp_aeg


# Check difference when fecundity is EFD, not a*cte
# R0 function by temperature:
R0_func_aeg <- function(Te, rain,hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- 1#a_f_aeg(Te)
    f <- EFD_f_aeg(Te) #40
    deltaa <- lf_f_aeg(Te)
    dE <- dE_f_aeg(Te)
    probla <- pLA_f_aeg(Te)
    h <- h_f(hum,rain)
    deltaE = cte#deltaE_f_aeg(Te)
    R0 <- ((f*a*deltaa)*probla*((h*dE)/(h*dE+deltaE)))^(1/3)
  }
  return(R0)
}

# Run model with fixed param
vec <- seq(5,40,0.001)
aegypti_EFD <- sapply(vec,R0_func_aeg, hum = 500,rain = 8) 

# Create data frame with all vecs
df_cte1 <- data.frame(vec, aegypti, aegypti_EFD)
df_cte1 <- reshape2::melt( df_cte1, id.vars = "vec")
ggplot(df_cte1) + 
  geom_line(aes(vec,value, color=variable), size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  ylab(TeX("$R_M$")) + theme_bw() +  xlab("Temperature") + 
  scale_color_manual(name= "", values = c("#edae49", "#66a182"),
                     labels = c(expression(italic("Cte fecundity")),
                                expression(italic("Fecundity depending on temp")))) +
  theme(legend.position = c(0.18,0.75),
        text = element_text(size = letsize),
        legend.text.align = 0)

