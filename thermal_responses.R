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

R0_func <- function(Te){
  a <- a_f(Te)
  f <- TFD_f(Te)
  deltaa <- 1/lf_f(Te)
  probla <- pEA_f(Te)
  R0 <- sqrt(f*(a/deltaa)*probla)
  return(R0)
}

# Thermal responses #
min_temp = 10
max_temp = 40
time_step <- 0.001
# Biting rate:
a_f <- function(temp){Briere_func(0.000193,10.25,38.32,temp)}
vec <- seq(min_temp,max_temp,time_step)
a  <- sapply(vec, a_f )
a_df <- data.frame( x = vec, a = a )

a_plot <- ggplot(a_df) + 
  geom_line(aes(x, a), color ="blue") +
  xlab("Temperature (Cº)") +
  ylab("a, Biting rate") 
a_plot

# Fecundity:
TFD_f <- function(temp){Briere_func(0.0488,8.02,35.65,temp)}
vec <- seq(min_temp,max_temp,time_step)
TFD <- sapply(vec, TFD_f)
TFD_df <- data.frame( x = vec, TFD = TFD)

TFD_plot <- ggplot(TFD_df) + 
  geom_line(aes(x, TFD), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("TFD, Fecundity rate") 
TFD_plot

# Survival probability Egg-Adult:
pEA_f <- function(temp){Quad_func(0.00361,9.04,39.33,temp)}
vec <- seq(min_temp,max_temp,time_step)
pEA <- sapply(vec, pEA_f)
pEA_df <- data.frame( x = vec, pEA = pEA)

pEA_plot <- ggplot(pEA_df) + 
  geom_line(aes(x, pEA), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("pEA, Edd-adult survival probability") 
pEA_plot

# Mosquito Development Rate:
MDR_f <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)}
vec <- seq(min_temp,max_temp,time_step)
MDR <- sapply(vec, MDR_f)
MDR_df <- data.frame( x = vec, MDR = MDR)

MDR_plot <- ggplot(MDR_df) + 
  geom_line(aes(x, MDR), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("MDR, mosquito Development Rate") 
MDR_plot

# Adult life span:
lf_f <- function(temp){Quad_func(1.43,13.41,31.51,temp)}
vec <- seq(min_temp,max_temp,time_step)
lf <- sapply(vec, lf_f)
lf_df <- data.frame( x = vec, lf = lf)

lf_plot <- ggplot(lf_df) + 
  geom_line(aes(x, lf), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("lf, adult life span") 
lf_plot

# R0 depending on Temperature (Cº):
vec <- seq(min_temp,max_temp,time_step)
R0 <- sapply(vec, R0_func)
r0_df <- data.frame( x = vec, a = R0)

R0_plot <- ggplot(r0_df) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab(TeX("$R_0 (Suitability index)$")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red")
R0_plot


library("cowplot")
plot_grid(a_plot,TFD_plot,pEA_plot,MDR_plot,lf_plot, R0_plot)

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
