#Compute function Albopictus y Aegypti
rm(list =ls())
####Dependencies####
library(ggplot2)
require(graphics)

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
df_out_aeg <- data.frame(temp_ae = vec, life_span_ae <- sapply(vec, mod))
colnames(df_out_aeg) <- c("temp_ae", "life_span_ae")
df_out_aeg[which(df_out_aeg$life_span_ae < 0),2] <- 0
plotaeg <- ggplot(df_out_aeg) +
  geom_line(aes(temp_ae,life_span_ae), size = 0.7) +
  geom_point(data = df_aeg,aes(temp,proportion_surv), size = 0.9, color = "red") +
  ylim(c(0,1)) + xlim(c(0,45)) + 
  ylab("Total day lived") + xlab("Temperature (Cº)") +
  theme_bw()
plotaeg

# Data frame data taken from Delatte et al 2009. 
# https://academic.oup.com/jme/article/46/1/33/902827?login=false
df_albo <- data.frame(temp = c(5,10,15,20,25,30,35,40),
                      proportion_surv = c(0,0,4/8,62/80,61/80,54/80,3.5/120,0))

ggplot(df_albo) + 
  geom_point(aes(temp,proportion_surv)) + theme_bw()

### Survival curve Laura:
# Path <- "~/Documentos/PHD/2023/Laura proj/DATA_LARVA_TEM.csv"
# df <- read.csv(file = Path)
# plot_alb <- ggplot(df) + geom_point(aes(temp_chamber,total_lived))

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
colnames(df_alb) <- c("temp", "life_span")
df_alb[which(df_alb$life_span < 0),2] <- 0

plotalb <- ggplot(df_alb) +
  geom_line(aes(temp,life_span), size = 0.7) +
  geom_point(data = df_albo,aes(temp,proportion_surv),
             size = 0.9, color = "red") +
  ylim(c(0,1)) + xlim(c(0,40)) + 
  ylab("Prob from Larva to Adult") + xlab("Temperature (Cº)") +
  theme_bw()
plotalb

library(ggpubr)
ggarrange(plotalb  + ggtitle("Aedes Albopictus") + ylab("Probability from Larvae to Adult"),
          plotaeg + ggtitle("Aedes Aegypti") + ylab(""))

#--------------------Egg Development Aedes Albopictus--------------------#
## Info taken https://www.scielo.br/j/rsp/a/dvPQ8QMr7Y687hPJxsTxjDg/abstract/?lang=en
df_deltaE <- data.frame(temp = c(15,20,25,30),
                     develop_rate = c(1/38.38,1/19.09,1/13.10,1/10.44))

plot_deltaE <- ggplot(df_deltaE) + 
  geom_point(aes(temp,develop_rate)) + theme_bw()
plot_deltaE

Fitting_deltaE <- nls(develop_rate ~ c*temp + c1,
                   data = df_deltaE,
                   start = list(c = 0.001, c1 = 0 ))

summary(Fitting_deltaE)

mod <- function(te){
  c <- as.numeric(Fitting_deltaE$m$getPars()[1])
  c1 <- as.numeric(Fitting_deltaE$m$getPars()[2])
  c*te+c1
}

vec <- seq(0,45,0.001)
df_out <- data.frame(temp = vec, devep_rate <- sapply(vec, mod))
plot_deltaE <- ggplot(df_out) +
  geom_line(aes(temp,devep_rate), size = 0.7) +
  geom_point(data = df_deltaE, aes(temp,develop_rate), size = 0.9, color = "red") +
  ylab("Egg development time") + xlab("Temperature (Cº)") +
  theme_bw()
plot_deltaE


### --------------Development Egg Aegypti -----------------------#
## https://pubmed.ncbi.nlm.nih.gov/19274388/
df_dE_aeg <- data.frame(temp=c(16,22,25,28,31,35,36),
           days = c(1/(482/24),1/(95/24),1/(74/24),1/(59.5/24),1/(47.5/24),1/(49.5/24),1/(600/24)))
ggplot(df_dE_aeg) + 
  geom_point(aes(temp,days))

library(devRate)
fitcurve <- devRateModel(eq = lactin1_95, 
                        temp = df_dE_aeg$temp, 
                        devRate = df_dE_aeg$days,
                        startValues =  list(aa = 0.02, Tmax = 24, deltaT = 5))
resultNLS <- devRatePrint(myNLS = fitcurve)

mod <- function(te){
  c <- as.numeric(resultNLS$sumNLS$coefficients[1])
  c1 <- as.numeric(resultNLS$sumNLS$coefficients[2])
  c2 <- as.numeric(resultNLS$sumNLS$coefficients[3])
  c*exp(-(1/2)*((te-c1)/c2)^2)
}

vec <- seq(0,45,0.001)
df_out <- data.frame(temp = vec, devep_rate <- sapply(vec, mod))
plot_deltaE <- ggplot(df_out) +
  geom_point(aes(temp,devep_rate), size = 0.01) +
  geom_point(data = df_dE_aeg, aes(temp,days), size = 0.7, color = "red") +
  ylab("Egg development time") + xlab("Temperature (Cº)") +
  theme_bw()
plot_deltaE

devRatePlot(eq = taylor_81, 
            nlsDR = fitcurve, 
            pch = 16, ylim = c(0, 1))

