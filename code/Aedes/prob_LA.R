#Compute function Albopictus y Aegypti

####Dependencies####
library(ggplot2)
require(graphics)

# Data frame data taken from Delatte et al 2009. 
# https://academic.oup.com/jme/article/46/1/33/902827?login=false
df_albo <- data.frame(temp = c(5,10,15,20,25,30,35,40),
          proportion_surv = c(0,0,4/8,62/80,61/80,54/80,3.5/120,0))

ggplot(df_albo) + 
  geom_point(aes(temp,proportion_surv)) + theme_bw()

### Survival curve Laura:
Path <- "~/Documentos/PHD/2023/Laura proj/DATA_LARVA_TEM.csv"
df <- read.csv(file = Path)
plot_alb <- ggplot(df) + geom_point(aes(temp_chamber,total_lived))

# Linear model:
linear.model <-lm(df_albo$proportion_surv ~ df_albo$temp)
plot(df_albo$temp, df_albo$proportion_surv, pch=6, ylab = "Counts ", cex.lab = 1.3, col = "red" )
abline(lm(df_albo$proportion_surv ~ df_albo$temp), col = "blue")

# My data nls:
Fitting <- nls(proportion_surv ~ (-cont*(temp-Tmin)*(temp - Tmax)),
               data = df_albo,
               start = list(cont = 0.001, Tmin = 5, Tmax = 20))

summary(Fitting)


mod <- function(te){
  t0 <- as.numeric(Fitting$m$getPars()[2])
  tm <- as.numeric(Fitting$m$getPars()[3])
  c <- as.numeric(Fitting$m$getPars()[1])
  (-c*(te-t0)*(te - tm))
}

vec <- seq(0,40,0.001)
df_alb <- data.frame(temp = vec, life_span <- sapply(vec, mod))
plotalb <- ggplot(df_alb) +
  geom_point(aes(temp,life_span), size = 0.01) +
  geom_point(data = df_albo,aes(temp,proportion_surv), size = 0.7, color = "red") +
  ylim(c(0,1)) + xlim(c(0,40)) + 
  ylab("Total day lived") + xlab("Temperature (Cº)") +
  theme_bw()

# Data frame data taken from Tun-Lin et al 2001 
# https://resjournals.onlinelibrary.wiley.com/doi/full/10.1046/j.1365-2915.2000.00207.x
# https://academic.oup.com/jme/article/56/6/1661/5505308

df_aeg <- data.frame(temp = c(10,15,20,25,27,30,35,40,12,14,16),
      proportion_surv = c(0,0.235,0.9,0.88,0.93,0.88,0.67,0,0.23,0.87,0.93))

plot_aeg <- ggplot(df_aeg) + 
  geom_point(aes(temp,proportion_surv)) + theme_bw()

Fitting_aeg <- nls(proportion_surv ~ (-cont*(temp-Tmin)*(temp - Tmax)),
               data = df_aeg,
               start = list(cont = 0.001, Tmin = 5, Tmax = 20))

summary(Fitting_aeg)

mod <- function(te){
  t0 <- as.numeric(Fitting_aeg$m$getPars()[2])
  tm <- as.numeric(Fitting_aeg$m$getPars()[3])
  c <- as.numeric(Fitting_aeg$m$getPars()[1])
  (-c*(te-t0)*(te - tm))
}

vec <- seq(0,45,0.001)
df_out <- data.frame(temp = vec, life_span <- sapply(vec, mod))
plotaeg <- ggplot(df_out) +
  geom_point(aes(temp,life_span), size = 0.01) +
  geom_point(data = df_albo,aes(temp,proportion_surv), size = 0.7, color = "red") +
  ylim(c(0,1)) + xlim(c(0,45)) + 
  ylab("Total day lived") + xlab("Temperature (Cº)") +
  theme_bw()

library(ggpubr)
ggarrange(plotalb,plotaeg)
