theme_set(ggplot2::theme_bw() +
            ggplot2::theme(axis.title = element_text(size  = 15, color = 'black'),
                           axis.text = element_text(size = 15, color = 'black'),
                           legend.text = element_text(size = 15, color = 'black'),
                           legend.title = element_text(size = 15, color = 'black')))
rm(list=ls())
library(ggplot2)
library(latex2exp)

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

########-------------------Albopictus-----------------------########
# Thermal responses #
min_temp = 10
max_temp = 40
time_step <- 0.001

# Biting rate:
a_f_albo <- function(temp){Briere_func(0.000193,10.25,38.32,temp)}
vec <- seq(min_temp,max_temp,time_step)
a_albo  <- sapply(vec, a_f_albo )
a_df_albo <- data.frame( x = vec, a = a_albo )

a_plot_albo <- ggplot(a_df_albo) + 
  geom_line(aes(x, a), color ="blue") +
  xlab("Temperature (Cº)") +
  ylab("a, Biting rate") 
a_plot_albo

# Fecundity:
TFD_f_albo <- function(temp){Briere_func(0.0488,8.02,35.65,temp)}
vec <- seq(min_temp,max_temp,time_step)
TFD_albo <- sapply(vec, TFD_f_albo)
TFD_df_albo <- data.frame( x = vec, TFD = TFD_albo)

TFD_plot_albo <- ggplot(TFD_df_albo) + 
  geom_line(aes(x, TFD), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("TFD, Fecundity rate") 
TFD_plot_albo

# Survival probability Egg-Adult:
pEA_f_albo <- function(temp){Quad_func(0.00361,9.04,39.33,temp)}
vec <- seq(min_temp,max_temp,time_step)
pEA_albo <- sapply(vec, pEA_f_albo)
pEA_df_albo <- data.frame( x = vec, pEA = pEA_albo)

pEA_plot_albo <- ggplot(pEA_df_albo) + 
  geom_line(aes(x, pEA), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("pEA, Edd-adult survival probability") 
pEA_plot_albo

# Mosquito Development Rate:
MDR_f_albo <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)}
vec <- seq(min_temp,max_temp,time_step)
MDR_albo <- sapply(vec, MDR_f_albo)
MDR_df_albo <- data.frame( x = vec, MDR = MDR_albo)

MDR_plot_albo <- ggplot(MDR_df_albo) + 
  geom_line(aes(x, MDR), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("MDR, mosquito Development Rate") 
MDR_plot_albo

# Adult life span:
lf_f_albo <- function(temp){Quad_func(1.43,13.41,31.51,temp)}
vec <- seq(min_temp,max_temp,time_step)
lf_albo <- sapply(vec, lf_f_albo)
lf_df_albo <- data.frame( x = vec, lf = lf_albo)

lf_plot_albo <- ggplot(lf_df_albo) + 
  geom_line(aes(x, lf), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("lf, adult life span") 
lf_plot_albo

# R0 depending on Temperature (Cº):
R0_func_albo <- function(Te){
  a <- a_f_albo(Te)
  f <- TFD_f_albo(Te)
  deltaa <- 1/lf_f_albo(Te)
  probla <- pEA_f_albo(Te)
  R0 <- sqrt(f*(a/deltaa)*probla)
  return(R0)
}

vec <- seq(min_temp,max_temp,time_step)
R0_albo <- sapply(vec, R0_func_albo)
r0_df_albo <- data.frame( x = vec, a = R0_albo)

R0_plot_albo <- ggplot(r0_df_albo) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab(TeX("$R_0 (Suitability index)$")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red")
R0_plot_albo


library("cowplot")
albo <- plot_grid(a_plot_albo,TFD_plot_albo,pEA_plot_albo,
                  MDR_plot_albo,lf_plot_albo, R0_plot_albo)
albo
######## RACHEL FORMULA MT ##########
# # Egg viability:
# egg_f <- function(temp){Quad_func(0.00265,-0.91,34.26,temp)}
# vec <- seq(0,max_temp,0.1)
# egg <- sapply(vec, egg_f)
# egg_df <- data.frame( x = vec, a = egg)
# 
# egg_plot <- ggplot(egg_df) + 
#   geom_line(aes(x, a), color ="blue")  +
#   xlab("Temperature (Cº)") +
#   ylab("Egg viability") 
# egg_plot
# 
# #### Rachel culex pipiens ####
# # # Adult longevity:
# al_f <- function(temp){
#   -5.24*temp + 178.32
# }
# #
# vec <- seq(0,max_temp,0.1)
# al <- sapply(vec, al_f)
# al_df <- data.frame( x = vec, a = al)
# #
# al_plot <- ggplot(al_df) +
#   geom_line(aes(x, a), color ="blue")  +
#   xlab("Temperature (Cº)") +
#   ylab("Egg viability")
# al_plot


########
## Mosquito density
MT_func <- function(Te){
 (TFD_f(Te)*a_f(Te)*pEA_f(Te)*MDR_f(Te))*((lf_f(Te))^2)
}

vec <- seq(14,31,0.01)
Mt <- sapply(vec, MT_func)
Mt_df <- data.frame( x = vec, a = Mt )

Mt_plot <- ggplot(Mt_df) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab(TeX("$M_T(Mosquito density)$")) 
Mt_plot

########-------------------Aegypti-----------------------########
# Thermal responses #
min_temp = 10
max_temp = 40
time_step <- 0.001
# Biting rate:
a_f_aeg <- function(temp){Briere_func(0.000202,13.35,40.08,temp)}
vec <- seq(min_temp,max_temp,time_step)
a_aeg  <- sapply(vec, a_f_aeg )
a_df_aeg <- data.frame( x = vec, a = a_aeg )

a_plot_aeg <- ggplot(a_df_aeg) + 
  geom_line(aes(x, a), color ="blue") +
  xlab("Temperature (Cº)") +
  ylab("a, Biting rate") 
a_plot_aeg

# Fecundity:
TFD_f_aeg <- function(temp){Briere_func(0.00856,14.58,34.61,temp)}
vec <- seq(min_temp,max_temp,time_step)
TFD_aeg <- sapply(vec, TFD_f_aeg)
TFD_df_aeg <- data.frame( x = vec, TFD = TFD_aeg)

TFD_plot_aeg <- ggplot(TFD_df_aeg) + 
  geom_line(aes(x, TFD), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("TFD, Fecundity rate") 
TFD_plot_aeg

# Survival probability Egg-Adult:
pEA_f_aeg <- function(temp){Quad_func(0.00599,13.56,38.29,temp)}
vec <- seq(min_temp,max_temp,time_step)
pEA_aeg <- sapply(vec, pEA_f_aeg)
pEA_df_aeg <- data.frame( x = vec, pEA = pEA_aeg)

pEA_plot_aeg <- ggplot(pEA_df_aeg) + 
  geom_line(aes(x, pEA), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("pEA, Edd-adult survival probability") 
pEA_plot_aeg

# Mosquito Development Rate:
MDR_f_aeg <- function(temp){Briere_func(0.0000786,11.36,39.17,temp)}
vec <- seq(min_temp,max_temp,time_step)
MDR_aeg <- sapply(vec, MDR_f_aeg)
MDR_df_aeg <- data.frame( x = vec, MDR = MDR_aeg)

MDR_plot_aeg <- ggplot(MDR_df_aeg) + 
  geom_line(aes(x, MDR), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("MDR, mosquito Development Rate") 
MDR_plot_aeg

# Adult life span:
lf_f_aeg <- function(temp){Quad_func(0.148,9.16,37.73,temp)}
vec <- seq(min_temp,max_temp,time_step)
lf_aeg <- sapply(vec, lf_f_aeg)
lf_df_aeg <- data.frame( x = vec, lf = lf_aeg)

lf_plot_aeg <- ggplot(lf_df_aeg) + 
  geom_line(aes(x, lf), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("lf, adult life span") 
lf_plot_aeg

# R0 depending on Temperature (Cº):
R0_func_aeg <- function(Te){
  a <- a_f_aeg(Te)
  f <- TFD_f_aeg(Te)
  deltaa <- 1/lf_f_aeg(Te)
  probla <- pEA_f_aeg(Te)
  R0 <- sqrt((f/deltaa)*probla)
  return(R0)
}

vec <- seq(min_temp,max_temp,time_step)
R0_aeg <- sapply(vec, R0_func_aeg)
r0_df_aeg <- data.frame( x = vec, a = R0_aeg)

R0_plot_aeg <- ggplot(r0_df_aeg) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab(TeX("$R_0 (Suitability index)$")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red")
R0_plot_aeg


library("cowplot")
aegyp <- plot_grid(a_plot_aeg,TFD_plot_aeg,pEA_plot_aeg,
          MDR_plot_aeg,lf_plot_aeg, R0_plot_aeg)

albo
aegyp
