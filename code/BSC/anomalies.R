rm(list=ls())
library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)
library(latex2exp)

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
  f <- (1/2)*TFD_f_alb(Te)
  deltaa <- lf_f_alb(Te)
  dE <- dE_f_alb(Te)
  probla <- pLA_f_alb(Te)
  deltaE = 0.1
  R0 <- (f*(a*deltaa)*probla)^(1/2)
  return(R0)
}

vec <- seq(0,40,0.01)
out <- sapply(vec,R0_func_alb)
letsize = 14
df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out)) + xlab("Temperature, Cº") + 
  ylab(TeX("$R_M$")) + xlim(c(5,35)) + coord_fixed() +
  theme_bw()+ theme(legend.position = "bottom",
                    text = element_text(size = letsize),
                    legend.text.align = 0)

vec <- seq(0,40,0.01)
out <- sapply(vec,TFD_f_alb)

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
  
  print(paste0("days year:",nrow(df_temp)))
  df_plot <- df_temp[, c("date", "R0_tmean", "R0_mean")]
  df_plotsd <- df_temp[, c("date", "R0_meanisd", "R0_meanasd")]
  colnames(df_plot) <- c("date", "Mean temperature 2022", "Mean temperature previous years")
  df_plot <- reshape2::melt(df_plot, id.vars = "date")
  
  if(grepl("HNMS",file_path) == TRUE){
    stat <- substr(file_path, 16,20)
    plot <- ggplot(df_plot) + 
      geom_ribbon(data = df_temp, aes(date, ymin = R0_meanisd,
                                      ymax = R0_meanasd),
                  alpha = 0.2, color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanasd),
                color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanisd),
                color = "#D0CCD0") +
      geom_line(aes(date,value, color = variable)) +
      scale_color_manual(values=c('#EE8434','#1B998B'), 
                         labels = c(TeX("$R_M$ with daily temperature 2022"),
                                    TeX("$R_M$ with mean temperature from 1993 to 2022"))) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
      ylab(TeX("$R_M$")) +
      xlab("Date") + ggtitle(paste0("HNMS Station:", stat)) +
      theme_bw() + theme(legend.position = "bottom",
                         text = element_text(size = letsize),
                         legend.text.align = 0) + 
      labs(color = "")
  }else{
    stat <- substr(file_path, 16,17)
    plot <- ggplot(df_plot) + 
      geom_ribbon(data = df_temp, aes(date, ymin = R0_meanisd,
                                      ymax = R0_meanasd),
                  alpha = 0.2, color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanasd),
                color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanisd),
                color = "#D0CCD0") +
      geom_line(aes(date,value, color = variable)) +
      scale_color_manual(values=c('#EE8434','#1B998B'), 
                         labels = c(TeX("$R_M$ with daily temperature 2022"),
                                    TeX("$R_M$ with mean temperature from 1993 to 2022"))) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
      ylab(TeX("$R_M$")) +
      xlab("Date") + ggtitle(paste0("XEMA Station: ", stat)) +
      theme_bw() + theme(legend.position = "bottom",
                         text = element_text(size = letsize),
                         legend.text.align = 0) +
      labs(color = "")
  }
  
  return(plot)
}

setwd("~/Documentos/PHD/2023/BSC/20230614_data_mean_var_sd_2022/")

## Mean temperature
### Greece
File <- "HNMS_anomalies_16715_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/HNMS_1675_mean.pdf",plot1 )
File <- "HNMS_anomalies_16716_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/HNMS_16716_mean.pdf",plot1 )
File <- "HNMS_anomalies_16718_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/HNMS_16718_mean.pdf",plot1 )
File <- "HNMS_anomalies_16741_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/HNMS_16741_mean.pdf",plot1 )

### Catalunya
File <- "XEMA_anomalies_D4_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_D4_mean.pdf",plot1 )
File <- "XEMA_anomalies_D5_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_D5_mean.pdf",plot1 )
File <- "XEMA_anomalies_DF_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_DF_mean.pdf",plot1 )
File <- "XEMA_anomalies_DJ_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_DJ_mean.pdf",plot1 )
File <- "XEMA_anomalies_DO_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_D0_mean.pdf",plot1 )
File <- "XEMA_anomalies_J5_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_J5_mean.pdf",plot1 )
File <- "XEMA_anomalies_KP_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_KP_mean.pdf",plot1 )
File <- "XEMA_anomalies_UB_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_UB_mean.pdf",plot1 )
File <- "XEMA_anomalies_UE_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_UE_mean.pdf",plot1 )
File <- "XEMA_anomalies_UN_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_UN_mean.pdf",plot1 )
File <- "XEMA_anomalies_W1_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_W1_mean.pdf",plot1 )
File <- "XEMA_anomalies_WT_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_WT_mean.pdf",plot1 )
File <- "XEMA_anomalies_WU_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_WU_mean.pdf",plot1 )
File <- "XEMA_anomalies_X2_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_X2_mean.pdf",plot1 )
File <- "XEMA_anomalies_X4_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_X4_mean.pdf",plot1 )
File <- "XEMA_anomalies_X8_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_X8_mean.pdf",plot1 )
File <- "XEMA_anomalies_XJ_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_XJ_mean.pdf",plot1 )
File <- "XEMA_anomalies_XZ_tmean_2022.csv"
plot1 <- func_mean(File)
plot1
ggsave(file = "Plots/XEMA_XZ_mean.pdf",plot1 )

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
  print(paste0("days year:",nrow(df_temp)))
  
  df_plot <- df_temp[, c("date", "R0_tmean", "R0_mean")]
  df_plotsd <- df_temp[, c("date", "R0_meanisd", "R0_meanasd")]
  colnames(df_plot) <- c("date", "Min temperature 2022", 
                         "Avg minimum temperature previous years")
  df_plot <- reshape2::melt(df_plot, id.vars = "date")
  
  if(grepl("HNMS",file_path) == TRUE){
    stat <- substr(file_path, 16,20)
    plot <- ggplot(df_plot) + 
      geom_ribbon(data = df_temp, aes(date, ymin = R0_meanisd,
                                      ymax = R0_meanasd),
                  alpha = 0.2, color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanasd),
                color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanisd),
                color = "#D0CCD0") +
      geom_line(aes(date,value, color = variable)) +
      scale_color_manual(values=c('#EE8434','#1B998B'), 
                         labels = c(TeX("$R_M$ with min daily temperature 2022"),
                                    TeX("$R_M$ with mean minimum temperature from 1993 to 2022"))) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b ") +
      ylab(TeX("$R_M$"))+
      xlab("Date") + ggtitle(paste0("HNMS Station: ", stat)) +
      theme_bw() + theme(legend.position = "bottom",
                         text = element_text(size = letsize)) + 
      labs(color = "")
  }else{
    stat <- substr(file_path, 16,17)
    plot <- ggplot(df_plot) + 
      geom_ribbon(data = df_temp, aes(date, ymin = R0_meanisd,
                                      ymax = R0_meanasd),
                  alpha = 0.2, color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanasd),
                color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanisd),
                color = "#D0CCD0") +
      geom_line(aes(date,value, color = variable)) +
      scale_color_manual(values=c('#EE8434','#1B998B'), 
                         labels = c(TeX("$R_M$ with min daily temperature 2022"),
                                    TeX("$R_M$ with mean minimum temperature from 1993 to 2022"))) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
      ylab(TeX("$R_M$"))+
      xlab("Date") + ggtitle(paste0("XEMA Station: ", stat)) +
      theme_bw() + theme(legend.position = "bottom",
                         text = element_text(size = letsize)) + 
      labs(color = "")
  }
  
  return(plot)
}

## Min temperature
### Greece
File <- "HNMS_anomalies_16715_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/HNMS_1675_min.pdf",plot1 )
File <- "HNMS_anomalies_16716_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/HNMS_16716_min.pdf",plot1 )
File <- "HNMS_anomalies_16718_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/HNMS_16718_min.pdf",plot1 )
File <- "HNMS_anomalies_16741_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/HNMS_16741_min.pdf",plot1 )

### Catalunya
File <- "XEMA_anomalies_D4_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_D4_min.pdf",plot1 )
File <- "XEMA_anomalies_D5_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_D5_min.pdf",plot1 )
File <- "XEMA_anomalies_DF_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_DF_min.pdf",plot1 )
File <- "XEMA_anomalies_DJ_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_DJ_min.pdf",plot1 )
File <- "XEMA_anomalies_DO_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_D0_min.pdf",plot1 )
File <- "XEMA_anomalies_J5_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_J5_min.pdf",plot1 )
File <- "XEMA_anomalies_KP_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_KP_min.pdf",plot1 )
File <- "XEMA_anomalies_UB_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_UB_min.pdf",plot1 )
File <- "XEMA_anomalies_UE_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_UE_min.pdf",plot1 )
File <- "XEMA_anomalies_UN_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_UN_min.pdf",plot1 )
File <- "XEMA_anomalies_W1_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_W1_min.pdf",plot1 )
File <- "XEMA_anomalies_WT_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_WT_min.pdf",plot1 )
File <- "XEMA_anomalies_WU_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_WU_min.pdf",plot1 )
File <- "XEMA_anomalies_X2_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_X2_min.pdf",plot1 )
File <- "XEMA_anomalies_X4_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_X4_min.pdf",plot1 )
File <- "XEMA_anomalies_X8_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_X8_min.pdf",plot1 )
File <- "XEMA_anomalies_XJ_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_XJ_min.pdf",plot1 )
File <- "XEMA_anomalies_XZ_tmin_2022.csv"
plot1 <- func_min(File)
plot1
ggsave(file = "Plots/XEMA_XZ_min.pdf",plot1 )

#--------------------GROUP all stations---------------------#
func_mean <- function(df_temp,stat){
  
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
  
  if(stat == "HNMS"){
    plot <- ggplot(df_plot) + 
      geom_ribbon(data = df_temp, aes(date, ymin = R0_meanisd,
                                      ymax = R0_meanasd),
                  alpha = 0.2, color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanasd),
                color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanisd),
                color = "#D0CCD0") +
      geom_line(aes(date,value, color = variable)) +
      scale_color_manual(values=c('#EE8434','#1B998B'), 
                         labels = c(TeX("$R_M$ with daily temperature 2022"),
                                    TeX("$R_M$ with mean temperature from 1993 to 2022"))) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
      ylab(TeX("$R_M$")) +
      xlab("Date") + ggtitle("HNMS") +
      theme_bw() + theme(legend.position = "bottom",
                         text = element_text(size = letsize),
                         legend.text.align = 0) + 
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
    scale_color_manual(values=c('#EE8434','#1B998B'), 
                       labels = c(TeX("$R_M$ with daily temperature 2022"),
                                  TeX("$R_M$ with mean temperature from 1993 to 2022"))) +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
    ylab(TeX("$R_M$")) +
    xlab("Date") + ggtitle("XEMA") +
    theme_bw() + theme(legend.position = "bottom",
                       text = element_text(size = letsize),
                       legend.text.align = 0) +
    labs(color = "")
  }
  
  return(plot)
}


func_min <- function(df_temp, stat){
  
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
  
  if(stat == "HNMS"){
    plot <- ggplot(df_plot) + 
      geom_ribbon(data = df_temp, aes(date, ymin = R0_meanisd,
                                      ymax = R0_meanasd),
                  alpha = 0.2, color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanasd),
                color = "#D0CCD0") +
      geom_line(data = df_plotsd, aes(date,R0_meanisd),
                color = "#D0CCD0") +
      geom_line(aes(date,value, color = variable)) +
      scale_color_manual(values=c('#EE8434','#1B998B'), 
                         labels = c(TeX("$R_M$ with min daily temperature 2022"),
                                    TeX("$R_M$ with mean minimum temperature from 1993 to 2022"))) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b ") +
      ylab(TeX("$R_M$"))+
      xlab("Date") + ggtitle("HNMS") +
      theme_bw() + theme(legend.position = "bottom",
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
      scale_color_manual(values=c('#EE8434','#1B998B'), 
                         labels = c(TeX("$R_M$ with min daily temperature 2022"),
                                    TeX("$R_M$ with mean minimum temperature from 1993 to 2022"))) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
      ylab(TeX("$R_M$"))+
      xlab("Date") + ggtitle("XEMA") +
      theme_bw() + theme(legend.position = "bottom",
                         text = element_text(size = letsize)) + 
      labs(color = "")
  }
  
  return(plot)
}

#-------------------------------------------------------------------
### Mean temperature
#### HNMS
setwd("~/Documentos/PHD/2023/BSC/20230614_data_mean_var_sd_2022/HNMS/tmean/")
lf <- list.files()
dt <- data.table()
for(i in c(1:length(lf))){
  file <- setDT(read.csv(lf[i], sep = ","))
  dt <- rbind(file,dt)
}
dt$date <- as.Date(dt$date)

dt <- dt[, .(tmean=mean(tmean),
       daymean =mean(daymean),
       daysd =mean(daysd)), 
       by=list(date)]
plot1 <- func_mean(dt, "HNMS")
plot1
setwd("~/Documentos/PHD/2023/BSC/20230614_data_mean_var_sd_2022/")
ggsave(file = "Plots/HNMS_AverageStations_mean.pdf",plot1 )

### XEMA
setwd("~/Documentos/PHD/2023/BSC/20230614_data_mean_var_sd_2022/XEMA/tmean/")
lf <- list.files()
dt <- data.table()
for(i in c(1:length(lf))){
  file <- setDT(read.csv(lf[i], sep = ","))
  dt <- rbind(file,dt)
}
dt$date <- as.Date(dt$date)

dt <- dt[, .(tmean=mean(tmean),
             daymean =mean(daymean),
             daysd =mean(daysd)), 
         by=list(date)]
plot1 <- func_mean(dt, "XEMA")
plot1
setwd("~/Documentos/PHD/2023/BSC/20230614_data_mean_var_sd_2022/")
ggsave(file = "Plots/XEMA_AverageStations_mean.pdf",plot1 )

#### Minimum temperatura
### HNMS
setwd("~/Documentos/PHD/2023/BSC/20230614_data_mean_var_sd_2022/HNMS/tmin/")
lf <- list.files()
dt <- data.table()
for(i in c(1:length(lf))){
  file <- setDT(read.csv(lf[i], sep = ","))
  dt <- rbind(file,dt)
}
dt$date <- as.Date(dt$date)

dt <- dt[, .(tmin=mean(tmin),
             daymean =mean(daymean),
             daysd =mean(daysd)), 
         by=list(date)]
plot1 <- func_min(dt, "HNMS")
plot1
setwd("~/Documentos/PHD/2023/BSC/20230614_data_mean_var_sd_2022/")
ggsave(file = "Plots/HNMS_AverageStations_min.pdf",plot1 )

### XEMA
setwd("~/Documentos/PHD/2023/BSC/20230614_data_mean_var_sd_2022/XEMA/tmin/")
lf <- list.files()
dt <- data.table()
for(i in c(1:length(lf))){
  file <- setDT(read.csv(lf[i], sep = ","))
  dt <- rbind(file,dt)
}
dt$date <- as.Date(dt$date)

dt <- dt[, .(tmin=mean(tmin),
             daymean =mean(daymean),
             daysd =mean(daysd)), 
         by=list(date)]
plot1 <- func_min(dt, "XEMA")
plot1
setwd("~/Documentos/PHD/2023/BSC/20230614_data_mean_var_sd_2022/")
ggsave(file = "Plots/XEMA_AverageStations_min.pdf",plot1 )
