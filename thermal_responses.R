theme_set(ggplot2::theme_bw() +
            ggplot2::theme(axis.title = element_text(size  = 15, color = 'black'),
                           axis.text = element_text(size = 15, color = 'black'),
                           legend.text = element_text(size = 15, color = 'black'),
                           legend.title = element_text(size = 15, color = 'black')))

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
  a <- gono_f(Te)
  f <- fec_f(Te)
  deltaa <- 1/adult_f(Te)
  probla <- surv_f(Te)
  R0 <- sqrt(f*(a/deltaa)*probla)
  return(R0)
  }



# Thermal responses #
min_temp = 10
max_temp = 40
time_step <- 0.001
# Gonotrophic cycle:
gono_f <- function(temp){Briere_func(0.000193,10.25,38.32,temp)}
vec <- seq(min_temp,max_temp,time_step)
gono <- sapply(vec, gono_f)
gono_df <- data.frame( x = vec, a = gono)

gono_plot <- ggplot(gono_df) + 
  geom_line(aes(x, a), color ="blue") +
  xlab("Temperature (Cº)") +
  ylab("a, Biting rate") 
gono_plot

# Fecundity:
fec_f <- function(temp){Briere_func(0.0482,8.02,36.65,temp)}
vec <- seq(min_temp,max_temp,time_step)
fec <- sapply(vec, fec_f)
fec_df <- data.frame( x = vec, a = fec)

fec_plot <- ggplot(fec_df) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("f, Fecundity rate") 
fec_plot

# Survival probability Larva:
surv_f <- function(temp){Quad_func(0.00361,9.04,39.33,temp)}
vec <- seq(min_temp,max_temp,time_step)
surv <- sapply(vec, surv_f)
surv_df <- data.frame( x = vec, a = surv)

surv_plot <- ggplot(surv_df) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("Survival probability Larva") 
surv_plot

# Mosquito Development Rate:
devep_f <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)}
vec <- seq(min_temp,max_temp,time_step)
devep <- sapply(vec, devep_f)
devep_df <- data.frame( x = vec, a = devep)

devep_plot <- ggplot(devep_df) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("Mosquito Development Rate") 
devep_plot

# Adult life span:
adult_f <- function(temp){Quad_func(1.43,13.41,31.51,temp)}
vec <- seq(min_temp,max_temp,time_step)
adult <- sapply(vec, adult_f)
adult_df <- data.frame( x = vec, a = adult)

adult_plot <- ggplot(adult_df) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("Adult life span") 
adult_plot

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
plot_grid(gono_plot,fec_plot,surv_plot,devep_plot,adult_plot, R0_plot)

######## RACHEL FORMULA MT ##########
# Egg viability:
egg_f <- function(temp){Quad_func(0.00265,-0.91,34.26,temp)}
vec <- seq(0,max_temp,0.1)
egg <- sapply(vec, egg_f)
egg_df <- data.frame( x = vec, a = egg)

egg_plot <- ggplot(egg_df) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab("Egg viability") 
egg_plot

#### Rachel culex pipiens ####
# # Adult longevity:
# al_f <- function(temp){
#   -5.24*temp + 178.32
# }
# 
# vec <- seq(0,max_temp,0.1)
# al <- sapply(vec, al_f)
# al_df <- data.frame( x = vec, a = al)
# 
# al_plot <- ggplot(al_df) + 
#   geom_line(aes(x, a), color ="blue")  +
#   xlab("Temperature (Cº)") +
#   ylab("Egg viability") 
# al_plot

# Mosquito abundance:
# MT_func <- function(Te){
#   Mt = (egg_f(Te)*surv_f(Te)*devep_f(Te)*gono_f(Te))*(al_f(Te))^2
#   return(Mt)
# }
########
## Mosquito density
MT_func <- function(Te){
 (egg_f(Te)*surv_f(Te)*devep_f(Te)*gono_f(Te))*(1/(adult_f(Te))^2)
}

vec <- seq(14,31,0.01)
Mt <- sapply(vec, MT_func)
Mt_df <- data.frame( x = vec, a = Mt )

Mt_plot <- ggplot(Mt_df) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature (Cº)") +
  ylab(TeX("$M_T(Mosquito density)$")) 
Mt_plot
