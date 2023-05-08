## Written using R 4.0.3
rm(list=ls())

####Dependencies####
library(ggplot2)
library(tidyverse)
### Survival curve Laura:
Path <- "~/Documentos/PHD/2023/Laura proj/DATA_LARVA_TEM.csv"
Path <- "~/Documents/PHD/2023/Laura proj/DATA_LARVA_TEM.csv"
df <- read.csv(file = Path)
df <- df[which( df$group == "control"),]
df <- df[which(df$stage_at_end == "Adult" & df$group == "control"),]
ggplot(df) + geom_point(aes(temp_chamber,time_to_Adult))

# Linear model:
linear.model <-lm(df$time_to_Adult ~ df$temp_chamber)
plot(df$temp_chamber, df$time_to_Adult, pch=16, ylab = "Counts ", cex.lab = 1.3, col = "red" )
abline(lm(df$time_to_Adult ~ df$temp_chamber), col = "blue")

# Example of non linear least squares:
require(graphics)
DNase1 <- subset(DNase, Run == 1)
fm1DNase1 <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
summary(fm1DNase1)

# My data nls:
Fitting <- nls(time_to_Adult ~ c*temp_chamber^2 + c1*temp_chamber + c2,
                 data = df,
                 start = list(c = 0, c1= 0, c2 = 0))

summary(Fitting)


mod <- function(te){
  c <- as.numeric(Fitting$m$getPars()[1])
  c1 <- as.numeric(Fitting$m$getPars()[2])
  c2 <- as.numeric(Fitting$m$getPars()[3])
  c*te^2+c1*te+c2
}

vec <- seq(0,40,0.001)
df_out <- data.frame(temp = vec, life_span <- sapply(vec, mod))
colnames(df_out) <- c("temp", "life_span")
ggplot(df_out) +
  geom_point(aes(temp,life_span), size = 0.01) +
  geom_point(data = df,aes(temp_chamber,time_to_Adult), size = 0.7, color = "red") +
  xlim(c(10,35)) + ylim(c(0,75)) +
  ylab("Time to adult") + xlab("Temperature (Cº)") +
  theme_bw()

### Probability survival
Path <- "~/Documents/PHD/2023/Laura proj/DATA_LARVA_TEM.csv"
df <- read.csv(file = Path)
df <- df[which( df$group == "control"),]
df_group <- df %>%  group_by(temp_chamber,stage_at_end) %>% 
     summarise( n = n())

df_group$stagef <- ifelse(df_group$stage_at_end != "Adult", "LP", "Adult")
df_group <- df_group %>%  group_by(temp_chamber,stagef) %>% 
  summarise( nsum= sum(n), n = n())

df_group_l <- spread(df_group, stagef, nsum)
df_group_l[which(is.na(df_group_l$Adult )), "Adult"] = 0
df_group_l[which(is.na(df_group_l$LP )), "LP"] = 0

df_group <- df_group_l %>%  group_by(temp_chamber) %>% 
  summarise( Asum= sum(Adult), LPsum = sum(LP))
df_group$tot <- df_group$Asum + df_group$LPsum
df_group$perc_surv <- df_group$Asum/df_group$tot

ggplot(df_group) + 
  geom_point(aes(temp_chamber,perc_surv ))

Fitting_pla <- nls(perc_surv ~ (-cont*(temp_chamber-Tmin)*(temp_chamber - Tmax)),
                   data = df_group,
                   start = list(cont = 0.001, Tmin = 5, Tmax = 20))

summary(Fitting_pla)

mod <- function(te){
  t0 <- as.numeric(Fitting_pla$m$getPars()[2])
  tm <- as.numeric(Fitting_pla$m$getPars()[3])
  c <- as.numeric(Fitting_pla$m$getPars()[1])
  (-c*(te-t0)*(te - tm))
}

vec <- seq(0,45,0.001)
df_out <- data.frame(temp = vec, pla <- sapply(vec, mod))
plotaeg <- ggplot(df_out) +
  geom_point(aes(temp,pla), size = 0.01) +
  geom_point(data = df_group,aes(temp_chamber,perc_surv), size = 0.7, color = "red") +
  ylim(c(0,1)) + xlim(c(0,45)) + 
  ylab("Prob from Larvae to Adult") + xlab("Temperature (Cº)") +
  theme_bw()
plotaeg
