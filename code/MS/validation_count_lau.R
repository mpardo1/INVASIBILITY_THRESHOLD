### Code to do a validation between female traps counts and R0
rm(list=ls())
library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)
library("viridis")
library("gganimate")
library(RColorBrewer)
library("latex2exp")

source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")

# Load Data Laura
Path <- '/home/marta/Documentos/PHD/2024/R_M/data/Abundancias_2021_2.csv'
abun <- read.csv(Path)
abun <- abun[is.na(abun$Start.date) == FALSE & is.na(abun$trapping.effort) == FALSE ,]
filt_esp <- c(unique(abun$species)[c(1,4,7,14)],NA)
abun <- abun[abun$species %in% filt_esp,]
Path <- '/home/marta/Documentos/PHD/2024/R_M/data/Malgrat-de-mar-2020-2022.csv'
weath <- read.csv(Path)
weath$timestamp <- as.Date(weath$timestamp)

# Extract year month and week of the month to add weather
abun$year <- year(as.Date(abun$Start.date))
abun$month <- month(as.Date(abun$Start.date))
abun$day <- day(as.Date(abun$Start.date))
unique(abun$trapping.effort)

# Plot abundances
unique(abun[,c("city", "Location.name", "trap.name")])
unique(abun[ ,"trapping.effort"])
ggplot(abun[abun$city == "Blanes" ,]) +
  geom_point(aes(Start.date,nrperspecies))

# Compute average temperature  and prec for the different trapping efforts
rolling_mean_temp <- function(x, n) {
  if(max(weath$timestamp) < x + n){
    mean_mov_temp <- 0
    mean_mov_prec <- 0
  }
  weath_aux <- weath[weath$timestamp >= x &
                       weath$timestamp <= (x + n),]
  mean_mov_temp <- mean(weath_aux$mean_temperature)
  mean_mov_prec <- mean(weath_aux$precipitation)
  return(mean_mov_temp)
}

rolling_mean_prec <- function(x, n) {
  if(max(weath$timestamp) < x + n){
    mean_mov_temp <- 0
    mean_mov_prec <- 0
  }
  weath_aux <- weath[weath$timestamp >= x &
                       weath$timestamp <= (x + n),]
  mean_mov_temp <- mean(weath_aux$mean_temperature)
  mean_mov_prec <- mean(weath_aux$precipitation)
  return(mean_mov_prec)
}

weath <- setDT(weath[, c("timestamp", "mean_temperature", "precipitation")]) 
# Test if function working
x <- weath$timestamp[1]
n <- 3
rolling_mean(weath$timestamp[1],3)

# Compute for each trapping effort
weath$trap_0 <- 0
weath[, temp_0 := mapply(rolling_mean_temp,timestamp,trap_0)]
weath[, prec_0 := mapply(rolling_mean_prec,timestamp,trap_0)]
weath$trap_0 <- NULL
weath$trap_1 <- 1
weath[, temp_1 := mapply(rolling_mean_temp,timestamp,trap_1)]
weath[, prec_1 := mapply(rolling_mean_prec,timestamp,trap_1)]
weath$trap_1 <- NULL
weath$trap_2 <- 2
weath[, temp_2 := mapply(rolling_mean_temp,timestamp,trap_2)]
weath[, prec_2 := mapply(rolling_mean_prec,timestamp,trap_2)]
weath$trap_2 <- NULL
weath$trap_4 <- 4
weath[, temp_4 := mapply(rolling_mean_temp,timestamp,trap_4)]
weath[, prec_4 := mapply(rolling_mean_prec,timestamp,trap_4)]
weath$trap_4 <- NULL
weath$trap_5 <- 5
weath[, temp_5 := mapply(rolling_mean_temp,timestamp,trap_5)]
weath[, prec_5 := mapply(rolling_mean_prec,timestamp,trap_5)]
weath$trap_5 <- NULL
weath$trap_6 <- 6
weath[, temp_6 := mapply(rolling_mean_temp,timestamp,trap_6)]
weath[, prec_6 := mapply(rolling_mean_prec,timestamp,trap_6)]
weath$trap_6 <- NULL
weath$trap_7 <- 7
weath[, temp_7 := mapply(rolling_mean_temp,timestamp,trap_7)]
weath[, prec_7 := mapply(rolling_mean_prec,timestamp,trap_7)]
weath$trap_7 <- NULL
weath$trap_8 <- 8
weath[, temp_8 := mapply(rolling_mean_temp,timestamp,trap_8)]
weath[, prec_8 := mapply(rolling_mean_prec,timestamp,trap_8)]
weath$trap_8 <- NULL
weath$trap_9 <- 9
weath[, temp_9 := mapply(rolling_mean_temp,timestamp,trap_9)]
weath[, prec_9 := mapply(rolling_mean_prec,timestamp,trap_9)]
weath$trap_9 <- NULL
weath$trap_11 <- 11
weath[, temp_11 := mapply(rolling_mean_temp,timestamp,trap_11)]
weath[, prec_11 := mapply(rolling_mean_prec,timestamp,trap_11)]
weath$trap_11 <- NULL
weath$trap_14 <- 14
weath[, temp_14 := mapply(rolling_mean_temp,timestamp,trap_14)]
weath[, prec_14 := mapply(rolling_mean_prec,timestamp,trap_14)]
weath$trap_14 <- NULL

# Remove not needed columns and reshape long format
weath$mean_temperature <- NULL
weath$precipitation <- NULL
weath <- reshape2::melt(weath, id.vars = "timestamp")
weath$var <- substr(weath$variable,1,4)
weath$trapping.effort <- substr(weath$variable,6,7)

# Create long format with column temp and prec
weath_temp <- weath[weath$var == "temp",]
weath_temp$temp <- weath_temp$value
weath_prec <- weath[weath$var == "prec",]
weath_prec$prec <- weath_prec$value
weath <- weath_prec[, c("timestamp", "prec", "trapping.effort")] %>% 
  left_join(weath_temp[, c("timestamp", "temp", "trapping.effort")])

# Compute R_M
weath$dens <- 2169.54 # taken from wikipedia
weath <- setDT(weath)
weath[, R0_alb := mapply(R0_func_alb,temp,prec,dens)]

# Check computation
ggplot(weath) +
  geom_point(aes(temp, R0_alb))

# Join with abundance data
abun <- abun[, c("city", "Start.date", "trapping.effort", "Females", "Males", "nrperspecies")]
abun$timestamp <- as.Date(abun$Start.date)
weath$trapping.effort <- as.numeric(weath$trapping.effort)
weath_abun <- weath %>% left_join(abun)

# Plot R_M against abundance
ggplot(weath_abun) +
  geom_point(aes(R0_alb, log10(nrperspecies), color = city))

ggplot(weath_abun) +
  geom_point(aes(R0_alb, log10(Females), color = city))

# Check correlation
list_cit <- unique(weath_abun$city)
i = 2
df_aux1 <- weath_abun[which(weath_abun$city == list_cit[i]),]
model <- lm(Females~ R0_alb, data =df_aux1)
plot(model)
vec <- seq(0,max(df_aux1$R0_alb),0.01)
df_out <- data.frame(vec = vec, 
                     # out =  exp(coef(model)[1])*(exp(df_aux1$R0_alb*coef(model)[2])))
                     out =  coef(model)[1] + vec*coef(model)[2])
df_out$cit <- list_cit[[i]]
df_out$rsq <- summary(model)$adj.r.squared
ggplot(df_out) +
  geom_line(aes(vec, out)) +
  geom_point(data = df_aux1, aes(R0_alb,log10(Females)))

# Plot with geom_smooth 
list_cit_1 <- list_cit[c(2,5,9)]
ggplot(weath_abun[weath_abun$city %in% list_cit_1,],
       aes(R0_alb, log10(Females), color = city))+
  geom_point() + geom_smooth(method = "lm") + theme_bw()
