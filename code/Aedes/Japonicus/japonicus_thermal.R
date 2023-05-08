rm(list= ls())
library(thermPerf)
library(ggplot2)
library(tidyverse)

## Compute the thermal responses for Aedes Japonicus
## Data taken from https://parasitesandvectors.biomedcentral.com/articles/10.1186/s13071-018-2659-1
## https://www.researchgate.net/publication/235430511_The_ecology_of_the_exotic_mosquito_Ochlerotatus_Finlaya_japonicus_japonicus_Theobald_1901_Diptera_Culicidae_and_an_examination_of_its_role_in_the_West_Nile_virus_cycle_in_New_Jersey
Path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/adult_larva_lifespan.csv"
Japonicus <- read.csv(Path)
head(Japonicus)

plot_deltaA <- ggplot(Japonicus) + 
  geom_point(aes(Temp,1/Age_adult_death_mean)) + theme_bw()
plot_deltaA

Fitting_deltaA <- nls(1/Age_adult_death_mean ~ cont*Temp + cont1,
                   data = Japonicus,
                   start = list(cont = 0.001, cont1 = 0.0))

summary(Fitting_deltaA)

mod <- function(te){
  c <- as.numeric(Fitting_deltaA$m$getPars()[1])
  c1 <- as.numeric(Fitting_deltaA$m$getPars()[2])
  c*te+c1
}

vec <- seq(0,45,0.001)
df_out_deltaA <- data.frame(temp_ae = vec,
                            deltaA_jap <- sapply(vec, mod))
plotdeltaA <- ggplot(df_out_deltaA) +
  geom_point(aes(temp_ae,deltaA_jap), size = 0.01) +
  geom_point(data = Japonicus,aes(Temp,1/Age_adult_death_mean), size = 0.7, color = "red") +
  xlim(c(0,45)) + 
  ylab("Adult mortality rate") + xlab("Temperature (Cº)") +
  theme_bw()
plotdeltaA

###----------------------------------------------
Path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/japonicus_temp_developmenttime.csv"
developL <- read.csv(Path)
head(developL)
developL$First_instar_mean <- 1/as.numeric(gsub(",", ".",developL$First_instar_mean))

plot_dE <- ggplot(developL) + 
  geom_point(aes(Temp,First_instar_mean)) + theme_bw()
plot_dE

Fitting_dE <- nls(First_instar_mean ~ cont*Temp^2 + cont1*Temp + cont2,
                      data = developL,
                      start = list(cont = 0.001, cont1 = 0, cont2 = 0))

summary(Fitting_dE)

mod <- function(te){
  c <- as.numeric(Fitting_dE$m$getPars()[1])
  c1 <- as.numeric(Fitting_dE$m$getPars()[2])
  c2 <- as.numeric(Fitting_dE$m$getPars()[3])
  c*te^2+c1*te+c2
}

vec <- seq(0,45,0.001)
df_out_dE  <- data.frame(temp_ae = vec,
                            dE_jap <- sapply(vec, mod))
plotdE <- ggplot(df_out_dE) +
  geom_point(aes(temp_ae,dE_jap), size = 0.01) +
  geom_point(data = developL,aes(Temp,First_instar_mean), size = 0.7, color = "red") +
  xlim(c(0,45)) + 
  ylab("Develop rate from Egg to Larva") + xlab("Temperature (Cº)") +
  theme_bw()
plotdE

#--------------------------------------------------------
developL$Pupa_Female_mean <- 1/as.numeric(gsub(",", ".",developL$Pupa_Female_mean))

plot_dL <- ggplot(developL) + 
  geom_point(aes(Temp,Pupa_Female_mean)) + theme_bw()
plot_dL

Fitting_dL <- nls(Pupa_Female_mean ~ cont*Temp + cont1,
                  data = developL,
                  start = list(cont = 0.001, cont1 = 0))

summary(Fitting_dL)

mod <- function(te){
  c <- as.numeric(Fitting_dL$m$getPars()[1])
  c1 <- as.numeric(Fitting_dL$m$getPars()[2])
  c*te+c1
}

vec <- seq(0,45,0.001)
df_out_dL  <- data.frame(temp_ae = vec,
                         dL_jap <- sapply(vec, mod))
plotdL <- ggplot(df_out_dL) +
  geom_point(aes(temp_ae,dL_jap), size = 0.01) +
  geom_point(data = developL,aes(Temp,Pupa_Female_mean), size = 0.7, color = "red") +
  xlim(c(0,45)) + 
  ylab("Develop rate from Larva to Adult") + xlab("Temperature (Cº)") +
  theme_bw()
plotdL

###----------------------------------------------
Lmortality <- data.frame(Temp = c(0,5,10,12,14,15,17,19,20,23,25,26,27,28,29,31),
                         mean_mort_perc = c(100,99.5,16,38.5,18,15,19,29.5,11.3,48.5,13.8,6,41.5,12.5,70.5,87.5),
                         sd_mort_perc = c(0,1.1,5.5,14.2,4.8,7.9,9.6,12.4,6.5,27.6,8.4,5.2,31.1,7.7,22.2,6.4))
head(Lmortality)
Lmortality$mean_mort_perc = Lmortality$mean_mort_perc/100
plot_deltaL <- ggplot(Lmortality) + 
  geom_point(aes(Temp,mean_mort_perc)) + theme_bw()
plot_deltaL

Fitting_deltaL <- nls(mean_mort_perc ~ cont*Temp^2 + cont1*Temp + cont2,
                      data = Lmortality,
                      start = list(cont = 15, cont1 = -20,
                                   cont2 = 30))

summary(Fitting_deltaL)

mod <- function(te){
  c <- as.numeric(Fitting_deltaL$m$getPars()[1])
  c1 <- as.numeric(Fitting_deltaL$m$getPars()[2])
  c2 <- as.numeric(Fitting_deltaL$m$getPars()[3])
  c*te^2+c1*te+c2
}

vec <- seq(0,45,0.001)
df_out_deltaL <- data.frame(temp_ae = vec,
                            deltaL_jap <- sapply(vec, mod))
plotdeltaL <- ggplot(df_out_deltaL) +
  geom_point(aes(temp_ae,deltaL_jap), size = 0.01) +
  geom_point(data = Lmortality,aes(Temp,mean_mort_perc), size = 0.7, color = "red") +
  ylab("Larva mortality rate") + xlab("Temperature (Cº)") +
  theme_bw()
plotdeltaL

###----------------------------------------------
library(ggpubr)
ggarrange(plotdE,plotdL,
          plotdeltaL,plotdeltaA)
