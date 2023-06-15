rm(list=ls())
library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)

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

QuadN_func <- function(cte, c1, c2, temp){
  outp <- cte*temp^2 + c1*temp + c2
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

Lin_func <- function(cte, c1, temp){
  outp <- cte*temp + c1
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
R0_func_alb <- function(Te){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  deltaa <- lf_f_alb(Te)
  dE <- dE_f_alb(Te)
  probla <- pLA_f_alb(Te)
  deltaE = 0.1
  R0 <- (f*(a*deltaa)*probla)^(1/3)
  return(R0)
}

vec <- seq(0,40,0.01)
out <- sapply(vec,R0_func_alb)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

letsize <- 13
# Function for temp mean the plot is the output:
func_mean <- function(file_path){
  df_temp <- setDT(read.csv(file_path, sep = ","))
  # head(df_temp)
  df_temp$date <- as.Date(df_temp$date)
  df_temp$daymin <- df_temp$daymean - df_temp$daysd
  df_temp$daymax <- df_temp$daymean + df_temp$daysd
  
  # Compute the R0 for tmin, tmax
  df_temp[, R0_tmean := mapply(R0_func_alb, tmean)]
  df_temp[, R0_mean := mapply(R0_func_alb, daymean)]
  df_temp[, R0_meanisd := mapply(R0_func_alb, daymin)]
  df_temp[, R0_meanasd := mapply(R0_func_alb, daymax)]
  
  df_plot <- df_temp[, c("date", "R0_tmean", "R0_mean")]
  df_plotsd <- df_temp[, c("date", "R0_meanisd", "R0_meanasd")]
  colnames(df_plot) <- c("date", "Mean temperature 2022", "Mean temperature previous years")
  df_plot <- reshape2::melt(df_plot, id.vars = "date")
  
  if(grepl("HNMS",file_path) == TRUE){
    plot <- ggplot(df_plot) + 
      geom_ribbon(data = df_temp, aes(date, ymin = R0_meanisd,
                                      ymax = R0_meanasd),
                  alpha = 0.2, color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanasd),
                color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanisd),
                color = "#D0CCD0") +
      geom_line(aes(date,value, color = variable)) +
      scale_color_manual(values=c('#EE8434','#1B998B')) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
      ylab("R0") +
      xlab("Date") + ggtitle("Atica 2022") +
      theme_bw() + theme(legend.position = c(0.15, 0.8),
                         text = element_text(size = letsize)) + 
      labs(color = "")
  }else{
    plot <- ggplot(df_plot) + 
      geom_ribbon(data = df_temp, aes(date, ymin = R0_meanisd,
                                      ymax = R0_meanasd),
                  alpha = 0.2, color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanasd),
                color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanisd),
                color = "#D0CCD0") +
      geom_line(aes(date,value, color = variable)) +
      scale_color_manual(values=c('#EE8434','#1B998B')) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
      ylab("R0") +
      xlab("Date") + ggtitle("Barcelona 2022") +
      theme_bw() + theme(legend.position = c(0.15, 0.8),
                         text = element_text(size = letsize)) +
      labs(color = "")
  }
  
  return(plot)
}

lf <- setwd("~/Documentos/PHD/2023/BSC/20230614_data_mean_var_sd_2022/")

## Mean temperature
### Barcelona
File <- "HNMS_anomalies_16715_tmean_2022.csv"
func_mean(File)
File <- "HNMS_anomalies_16716_tmean_2022.csv"
func_mean(File)
File <- "HNMS_anomalies_16718_tmean_2022.csv"
func_mean(File)
File <- "HNMS_anomalies_16741_tmean_2022.csv"
func_mean(File)

### Grecia
File <- "XEMA_anomalies_D4_tmean_2022.csv"
func_mean(File)
File <- "XEMA_anomalies_D5_tmean_2022.csv"
func_mean(File)
File <- "XEMA_anomalies_DF_tmean_2022.csv"
func_mean(File)
File <- "XEMA_anomalies_DJ_tmean_2022.csv"
func_mean(File)
File <- "XEMA_anomalies_DJ_tmean_2022.csv"
func_mean(File)
File <- "XEMA_anomalies_DJ_tmean_2022.csv"
func_mean(File)
File <- "XEMA_anomalies_DO_tmean_2022.csv"
func_mean(File)
File <- "XEMA_anomalies_J5_tmean_2022.csv"
func_mean(File)
File <- "XEMA_anomalies_KP_tmean_2022.csv"
func_mean(File)
File <- "XEMA_anomalies_UB_tmean_2022.csv"
func_mean(File)

# Function for temp mean the plot is the output:
func_min <- function(file_path){
  df_temp <- setDT(read.csv(file_path, sep = ","))
  # head(df_temp)
  df_temp$date <- as.Date(df_temp$date)
  df_temp$daymin <- df_temp$daymean - df_temp$daysd
  df_temp$daymax <- df_temp$daymean + df_temp$daysd
  
  # Compute the R0 for tmin, tmax
  df_temp[, R0_tmean := mapply(R0_func_alb, tmin)]
  df_temp[, R0_mean := mapply(R0_func_alb, daymean)]
  df_temp[, R0_meanisd := mapply(R0_func_alb, daymin)]
  df_temp[, R0_meanasd := mapply(R0_func_alb, daymax)]
  
  df_plot <- df_temp[, c("date", "R0_tmean", "R0_mean")]
  df_plotsd <- df_temp[, c("date", "R0_meanisd", "R0_meanasd")]
  colnames(df_plot) <- c("date", "Min temperature 2022", 
                         "Avg minimum temperature previous years")
  df_plot <- reshape2::melt(df_plot, id.vars = "date")
  
  if(grepl("HNMS",file_path) == TRUE){
    plot <- ggplot(df_plot) + 
      geom_ribbon(data = df_temp, aes(date, ymin = R0_meanisd,
                                      ymax = R0_meanasd),
                  alpha = 0.2, color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanasd),
                color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanisd),
                color = "#D0CCD0") +
      geom_line(aes(date,value, color = variable)) +
      scale_color_manual(values=c('#EE8434','#1B998B')) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
      ylab("R0") +
      xlab("Date") + ggtitle("Atica 2022") +
      theme_bw() + theme(legend.position = c(0.15, 0.8),
                         text = element_text(size = letsize)) + 
      labs(color = "")
  }else{
    plot <- ggplot(df_plot) + 
      geom_ribbon(data = df_temp, aes(date, ymin = R0_meanisd,
                                      ymax = R0_meanasd),
                  alpha = 0.2, color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanasd),
                color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanisd),
                color = "#D0CCD0") +
      geom_line(aes(date,value, color = variable)) +
      scale_color_manual(values=c('#EE8434','#1B998B')) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
      ylab("R0") +
      xlab("Date") + ggtitle("Barcelona 2022") +
      theme_bw() + theme(legend.position = c(0.15, 0.8),
                         text = element_text(size = letsize)) + 
      labs(color = "")
  }
  
  return(plot)
}

## Min temperature
### Barcelona
File <- "HNMS_anomalies_16715_tmin_2022.csv"
func_min(File)
File <- "HNMS_anomalies_16716_tmin_2022.csv"
func_min(File)
File <- "HNMS_anomalies_16718_tmin_2022.csv"
func_min(File)
File <- "HNMS_anomalies_16741_tmin_2022.csv"
func_min(File)

### Vietnam
File <- "XEMA_anomalies_D4_tmin_2022.csv"
func_min(File)
File <- "XEMA_anomalies_D5_tmin_2022.csv"
func_min(File)
File <- "XEMA_anomalies_DF_tmin_2022.csv"
func_min(File)
File <- "XEMA_anomalies_DJ_tmin_2022.csv"
func_min(File)
File <- "XEMA_anomalies_DJ_tmin_2022.csv"
func_min(File)
File <- "XEMA_anomalies_DJ_tmin_2022.csv"
func_min(File)
File <- "XEMA_anomalies_DO_tmin_2022.csv"
func_min(File)
File <- "XEMA_anomalies_J5_tmin_2022.csv"
func_min(File)
File <- "XEMA_anomalies_KP_tmin_2022.csv"
func_min(File)
File <- "XEMA_anomalies_UB_tmin_2022.csv"
func_min(File)
