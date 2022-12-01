theme_set(ggplot2::theme_bw() +
            ggplot2::theme(axis.title = element_text(size  = 15, color = 'black'),
                           axis.text = element_text(size = 15, color = 'black'),
                           legend.text = element_text(size = 15, color = 'black'),
                           legend.title = element_text(size = 15, color = 'black')))

library(ggplot2)
# Main functions 
Briere_func <- function(cte, tmin, tmax, temp){
  temp*cte*(temp - tmin)*(tmax - temp)^(1/2)
}

Quad_func <- function(cte, tmin, tmax, temp){
  -cte*(temp - tmin)*(temp - tmax)
}

# Thermal responses #

# Gonotrophic cycle:
gono_f <- function(temp){Briere_func(0.000193,10.25,38.32,temp)}
vec <- seq(10,38,0.1)
gono <- sapply(vec, gono_f)
gono_df <- data.frame( x = vec, a = gono)
gono_df <- gono_df[-which(gono_df[,2] < 0 | is.na(gono_df[,2])),]

gono_plot <- ggplot(gono_df) + 
  geom_line(aes(x, a), color ="blue") +
  xlab("Temperature") +
  ylab("a, Biting rate") 
gono_plot

# Fecundity:
fec_f <- function(temp){Briere_func(0.00482,8.02,36.65,temp)}
vec <- seq(10,40,0.11)
fec <- sapply(vec, fec_f)
fec_df <- data.frame( x = vec, a = fec)
rec_df <- fec_df[-which(fec_df[,2] < 0 | is.na(fec_df[,2])),]

fec_plot <- ggplot(fec_df) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature") +
  ylab("f, Fecundity rate") 
fec_plot

# Survival probability Larva:
surv_f <- function(temp){Quad_func(-0.00361,9.04,39.33,temp)}
vec <- seq(1,50,0.11)
surv <- sapply(vec, surv_f)
surv_df <- data.frame( x = vec, a = surv)
surv_df <- surv_df[-which(surv_df[,2] < 0 | is.na(surv_df[,2])),]

surv_plot <- ggplot(surv_df) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature") +
  ylab("Survival probability") 
surv_plot

# Mosquito Development Rate:
devep_f <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)}
vec <- seq(1,50,0.11)
devep <- sapply(vec, devep_f)
devep_df <- data.frame( x = vec, a = devep)
devep_df <- surv_df[-which(devep_df[,2] < 0 | is.na(devep_df[,2])),]

devep_plot <- ggplot(devep_df) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature") +
  ylab("Mosquito Development Rate") 
devep_plot

# Adult life span:
adult_f <- function(temp){Quad_func(-1.43,13.41,31.51,temp)}
vec <- seq(1,50,0.11)
adult <- sapply(vec, adult_f)
adult_df <- data.frame( x = vec, a = adult)
adult_df <- surv_df[-which(adult_df[,2] < 0 | is.na(adult_df[,2])),]

adult_plot <- ggplot(adult_df) + 
  geom_line(aes(x, a), color ="blue")  +
  xlab("Temperature") +
  ylab("Adult life span") 
adult_plot

library("cowplot")
plot_grid(gono_plot,fec_plot,surv_plot,devep_plot,adult_plot)


