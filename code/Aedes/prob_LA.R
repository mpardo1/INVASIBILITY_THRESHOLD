# Compute function Albopictus y Aegypti
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

Fitting_aeg_lin <- nls(proportion_surv ~ cont*temp+cont1,
                   data = df_aeg,
                   start = list(cont = 0.001, cont1 = 0))

summary(Fitting_aeg_lin)

AIC(Fitting_aeg,Fitting_aeg_lin)

mod <- function(te){
  t0 <- as.numeric(Fitting_aeg$m$getPars()[2])
  tm <- as.numeric(Fitting_aeg$m$getPars()[3])
  c <- as.numeric(Fitting_aeg$m$getPars()[1])
  (-c*(te-t0)*(te - tm))
}

vec <- seq(0,45,0.01)
df_out_aeg <- data.frame(temp_ae = vec, life_span_ae <- sapply(vec, mod))
colnames(df_out_aeg) <- c("temp_ae", "life_span_ae")
df_out_aeg[which(df_out_aeg$life_span_ae < 0),2] <- 0
df_out_aeg$group <- "mean"

###---------------+/- SD---------------------######
## Mean - SD
mod_min <- function(te){
  t0 <- as.numeric(Fitting_aeg$m$getPars()[2]) - 
    summary(Fitting_aeg)$coefficients[2,2]
  tm <- as.numeric(Fitting_aeg$m$getPars()[3]) - 
    summary(Fitting_aeg)$coefficients[3,2]
  c <- as.numeric(Fitting_aeg$m$getPars()[1]) - 
    summary(Fitting_aeg)$coefficients[1,2]
  (-c*(te-t0)*(te - tm))
}

vec <- seq(0,45,0.01)
df_out_aeg_min <- data.frame(temp_ae = vec,
                             life_span_ae <- sapply(vec, mod_min))
colnames(df_out_aeg_min) <- c("temp_ae", "life_span_ae")
df_out_aeg_min[which(df_out_aeg_min$life_span_ae < 0),2] <- 0
df_out_aeg_min$group <- "min"

plot(df_out_aeg_min$temp_ae,df_out_aeg_min$life_span_ae)
## Mean - SD
mod_max <- function(te){
  t0 <- as.numeric(Fitting_aeg$m$getPars()[2]) + 
    summary(Fitting_aeg)$coefficients[2,2]
  tm <- as.numeric(Fitting_aeg$m$getPars()[3]) + 
    summary(Fitting_aeg)$coefficients[3,2]
  c <- as.numeric(Fitting_aeg$m$getPars()[1]) + 
    summary(Fitting_aeg)$coefficients[1,2]
  (-c*(te-t0)*(te - tm))
}

vec <- seq(0,45,0.01)
df_out_aeg_max <- data.frame(temp_ae = vec,
                             life_span_ae <- sapply(vec, mod_max))
colnames(df_out_aeg_max) <- c("temp_ae", "life_span_ae")
df_out_aeg_max[which(df_out_aeg_max$life_span_ae < 0),2] <- 0
df_out_aeg_max$group <- "max"
plot(df_out_aeg_max$temp_ae,df_out_aeg_max$life_span_ae)

# Plot all three curves together
df_out_aeg <- rbind(df_out_aeg_max,
                       df_out_aeg_min,
                       df_out_aeg)

plotaeg <- ggplot(df_out_aeg) +
  geom_line(aes(temp_ae,life_span_ae,
                color = group,
                group = group, 
                alpha = group), size = 0.7) +
  geom_point(data = df_aeg,aes(temp,proportion_surv),
             size = 0.9, color = "black") +
  scale_color_manual(values=c("red", "blue", "red")) + 
  scale_alpha_manual(values = c(0.5,1,0.5)) +
  xlim(c(5,42)) + ylim(c(0,1.3)) +
  guides( color =FALSE, alpha = FALSE) +
  ylab("Development rate from Larvae to Adult") + xlab("Temperature (Cº)") +
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
# Path <- "~/INVASIBILITY_THRESHOLD/data/pLA_Laura.Rds"
# albo_lau <- readRDS(Path)[,c(1,5)]
# colnames(albo_lau) <- colnames(df_albo)
# df_albo <- rbind(df_albo, albo_lau)

ggplot(df_albo) + geom_point(aes(temp,proportion_surv))

# My data nls:
Fitting <- nls(proportion_surv ~ (-cont*(temp-Tmin)*(temp - Tmax)),
               data = df_albo,
               start = list(cont = 0.001, Tmin = 5, Tmax = 20))

summary(Fitting)

# Fitting <- nls(proportion_surv ~ cont*temp+cont1,
#                data = df_albo,
#                start = list(cont = 0.001, cont1 = 0))
# 
# summary(Fitting)

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

###------------ Mean +/- SD -------------###
### Mean+SD
mod_max <- function(te){
  t0 <- as.numeric(Fitting$m$getPars()[2]) +
    summary(Fitting)$coefficients[2,2]
  tm <- as.numeric(Fitting$m$getPars()[3]) +
    summary(Fitting)$coefficients[3,2]
  c <- as.numeric(Fitting$m$getPars()[1]) +
    summary(Fitting)$coefficients[1,2]
  (-c*(te-t0)*(te - tm))
}

vec <- seq(0,40,0.001)
df_alb_max <- data.frame(temp = vec, life_span <- sapply(vec, mod_max))
colnames(df_alb_max) <- c("temp", "life_span")
df_alb_max[which(df_alb_max$life_span < 0),2] <- 0
df_alb_max$group <- "max"

### Mean-SD
mod_min <- function(te){
  t0 <- as.numeric(Fitting$m$getPars()[2]) -
    summary(Fitting)$coefficients[2,2]
  tm <- as.numeric(Fitting$m$getPars()[3]) -
    summary(Fitting)$coefficients[3,2]
  c <- as.numeric(Fitting$m$getPars()[1]) -
    summary(Fitting)$coefficients[1,2]
  (-c*(te-t0)*(te - tm))
}

vec <- seq(0,40,0.001)
df_alb_min <- data.frame(temp = vec, life_span <- sapply(vec, mod_min))
colnames(df_alb_min) <- c("temp", "life_span")
df_alb_min[which(df_alb_min$life_span < 0),2] <- 0
df_alb_min$group <- "min"
df_alb$group <- "mean"

# Plot all three curves together
df_alb <- rbind(df_alb_max,
                df_alb_min,
                df_alb)

plotalb <- ggplot(df_alb) +
  geom_line(aes(temp,life_span,
                color = group,
                group = group, 
                alpha = group), size = 0.7) +
  geom_point(data = df_albo,aes(temp,proportion_surv),
             size = 0.9, color = "black") +
  scale_color_manual(values=c("red", "blue", "red")) + 
  scale_alpha_manual(values = c(0.5,1,0.5)) +
  xlim(c(5,40)) +
  guides( color =FALSE, alpha = FALSE) +
  ylab("Prob from Larva to Adult") + xlab("Temperature (Cº)") +
  theme_bw() 
plotalb

# Join all the plots in one figure
library(ggpubr)
ggarrange(plotalb  + ggtitle("Aedes Albopictus") +
            theme(text = element_text(size = 15)) +
            ylab("Probability from Larvae to Adult"),
          plotaeg + ggtitle("Aedes Aegypti") +
            theme(text = element_text(size = 15)) + 
            ylab(""))

#--------------------Egg Development Aedes Albopictus--------------------#
# ## Info taken Table1: https://www.scielo.br/j/rsp/a/dvPQ8QMr7Y687hPJxsTxjDg/abstract/?lang=en
# df_deltaE <- data.frame(temp = c(15,20,25,30),
#                      develop_rate_mu = c(38.38,19.09,
#                                       13.10,10.44),
#                      develop_rate_sd = c(6.25,5.7,8.37,6.18)
#                      )
# 
# n = 8
# r1 <- rnorm(n, df_deltaE$develop_rate_mu[1],
#             df_deltaE$develop_rate_sd[1] )
# r2 <- rnorm(n, df_deltaE$develop_rate_mu[2],
#             df_deltaE$develop_rate_sd[2] )
# r3 <- rnorm(n, df_deltaE$develop_rate_mu[3],
#             df_deltaE$develop_rate_sd[3] )
# r4 <- rnorm(n, df_deltaE$develop_rate_mu[4],
#             df_deltaE$develop_rate_sd[4] )
# 
# df_deltaE <- data.frame(temp = sort(rep(df_deltaE[,1],n)),
#                         develop_rate = c(r1,r2,r3,r4))
# 
# df_deltaE$develop_rate <- 1/df_deltaE$develop_rate
# saveRDS(df_dE, "~/INVASIBILITY_THRESHOLD/data/df_dE.Rds")

###### Random sample from a gaussian distribution, since if we take the mean
# thre are only 4 points in the data, and the fit it is overfitting
# We do it once since each iteration the curves are different. So we save the random sample.
df_dE <- readRDS( "~/INVASIBILITY_THRESHOLD/data/df_dE.Rds")

plot_dE <- ggplot(df_dE) + 
  geom_point(aes(temp,develop_rate)) + theme_bw()
plot_dE

Fitting_dE <- nls(develop_rate ~ c*temp*(temp-c1)*(c2-temp)^(1/2),
                   data = df_dE,
                   start = list(c = 0.001, c1 = 5 , c2 = 30))

summary(Fitting_dE)

Fitting_dE_lin <- nls(develop_rate ~ c*temp+c1,
                  data = df_dE,
                  start = list(c = 0.001, c1 = 0))

summary(Fitting_dE_lin)

AIC(Fitting_dE,Fitting_dE_lin)

mod <- function(te){
  c <- as.numeric(Fitting_dE$m$getPars()[1])
  c1 <- as.numeric(Fitting_dE$m$getPars()[2])
  c2 <- as.numeric(Fitting_dE$m$getPars()[3])
  c*te*(te-c1)*(c2-te)^(1/2)
}

vec <- seq(5,40,0.01)
df_out <- data.frame(temp = vec,
                     devep_rate <- sapply(vec, mod))
df_out[which(df_out$devep_rate < 0),2] <- 0
df_out$group <- "mean"

##--------- Mean - SD ----------###
mod_min <- function(te){
  c <- as.numeric(Fitting_dE$m$getPars()[1]) - 
    summary(Fitting_dE)$coefficients[1,2]
  c1 <- as.numeric(Fitting_dE$m$getPars()[2]) - 
    summary(Fitting_dE)$coefficients[2,2]
  c2 <- as.numeric(Fitting_dE$m$getPars()[3]) - 
    summary(Fitting_dE)$coefficients[3,2]
  c*te*(te-c1)*(c2-te)^(1/2)
}

df_out_min <- data.frame(temp = vec,
                         devep_rate = sapply(vec, mod_min))
df_out_min[which(df_out_min$devep_rate < 0),2] <- 0
df_out_min$group <- "min"

##--------- Mean + SD ----------###
mod_max <- function(te){
  c <- as.numeric(Fitting_dE$m$getPars()[1]) + 
    summary(Fitting_dE)$coefficients[1,2]
  c1 <- as.numeric(Fitting_dE$m$getPars()[2]) + 
    summary(Fitting_dE)$coefficients[2,2]
  c2 <- as.numeric(Fitting_dE$m$getPars()[3]) + 
    summary(Fitting_dE)$coefficients[3,2]
  c*te*(te-c1)*(c2-te)^(1/2)
}

df_out_max <- data.frame(temp = vec, 
                         devep_rate = sapply(vec, mod_max))
df_out_max[which(df_out_max$devep_rate < 0),2] <- 0
df_out_max$group <- "max"
colnames(df_out) <- colnames(df_out_min)
# Plot all three curves together
df_out <- rbind(df_out_max,
                 df_out_min,
                  df_out)

plotdE <- ggplot(df_out) +
  geom_line(aes(temp,devep_rate,
                color = group,
                group = group, 
                alpha = group), size = 0.7) +
  geom_point(data = df_dE, aes(temp,develop_rate),
             size = 0.9, color = "black") +
  scale_color_manual(values=c("red", "blue", "red")) + 
  scale_alpha_manual(values = c(0.5,1,0.5)) +
  xlim(c(5,40)) +
  guides( color =FALSE, alpha = FALSE) +
  ylab("Egg development time") + xlab("Temperature (Cº)") +
  theme_bw() 

plotdE 

### --------------Development Egg Aegypti -----------------------#
## https://pubmed.ncbi.nlm.nih.gov/19274388/
# df_dE_aeg <- data.frame(temp=c(16,22,25,28,31,35),
#                         hours_mean = c(489.3,98.3,77.4,61.6,48.4,50.3),
#                         days_sd = c(0.6,0.7,0.8,1.2,0.5,0.3))
# 
# n = 8
# r1 <- rnorm(n, df_dE_aeg$hours_mean[1],
#             df_dE_aeg$days_sd[1] )
# r2 <- rnorm(n, df_dE_aeg$hours_mean[2],
#             df_dE_aeg$days_sd[2] )
# r3 <- rnorm(n, df_dE_aeg$hours_mean[3],
#             df_dE_aeg$days_sd[3] )
# r4 <- rnorm(n, df_dE_aeg$hours_mean[4],
#             df_dE_aeg$days_sd[4] )
# r5 <- rnorm(n, df_dE_aeg$hours_mean[5],
#             df_dE_aeg$days_sd[5] )
# r6 <- rnorm(n, df_dE_aeg$hours_mean[6],
#             df_dE_aeg$days_sd[6] )
# 
# df_dE_aeg <- data.frame(temp = sort(rep(df_dE_aeg[,1],n)),
#                         hour_devep = c(r1,r2,r3,r4,r5,r6))
# 
# df_dE_aeg$develop_rate <- 1/(df_dE_aeg$hour_devep/24)
# df_dE_aeg <- rbind(df_dE_aeg, c(36,0,0,0.001))

# saveRDS(df_dE_aeg, "~/INVASIBILITY_THRESHOLD/data/df_dE_aeg.Rds")

## I use the random sample fixed, other wise the param would change.
df_dE_aeg <- readRDS("~/INVASIBILITY_THRESHOLD/data/df_dE_aeg.Rds")


ggplot(df_dE_aeg) + 
  geom_point(aes(temp,develop_rate))

Fitting_dE_aeg <- nls(develop_rate ~ c*temp*(temp-c1)*(c2-temp)^(1/2),
                      data = df_dE_aeg, algorithm = "port",
                      start = list(c = 0.0003, c1 = 14, c2 = 40), 
                      lower=c(0.00001,7,30), upper = c(0.04,15,50))

summary(Fitting_dE_aeg)

Fitting_dE_aeg_lin <- nls(develop_rate ~ c*temp + c1,
                      data = df_dE_aeg,
                      start = list(c = 0.0003, c1 = 0))

summary(Fitting_dE_aeg_lin)

AIC(Fitting_dE_aeg, Fitting_dE_aeg_lin)

mod <- function(te){
  c <- as.numeric(Fitting_dE_aeg$m$getPars()[1])
  c1 <- as.numeric(Fitting_dE_aeg$m$getPars()[2])
  c2 <- as.numeric(Fitting_dE_aeg$m$getPars()[3])
  c*te*(te-c1)*(c2-te)^(1/2)
}

vec <- seq(0,45,0.001)
df_out_aeg <- data.frame(temp = vec,
                     devep_rate = sapply(vec, mod))
df_out_aeg[which(df_out_aeg$devep_rate < 0),2] <- 0

####------Mean +/- SD -----------###
# Mean + SD
mod_max <- function(te){
  c <- as.numeric(Fitting_dE_aeg$m$getPars()[1])  + 
    summary(Fitting_dE_aeg)$coefficients[1,2]
  c1 <- as.numeric(Fitting_dE_aeg$m$getPars()[2])  + 
    summary(Fitting_dE_aeg)$coefficients[2,2]
  c2 <- as.numeric(Fitting_dE_aeg$m$getPars()[3])  + 
    summary(Fitting_dE_aeg)$coefficients[3,2]
  c*te*(te-c1)*(c2-te)^(1/2)
}

df_out_aeg_max <- data.frame(temp = vec,
                     devep_rate = sapply(vec, mod_max))
df_out_aeg_max[which(df_out_aeg_max$devep_rate < 0),2] <- 0
df_out_aeg_max$group = "max"

## MEan - SD
mod_min <- function(te){
  c <- as.numeric(Fitting_dE_aeg$m$getPars()[1])  - 
    summary(Fitting_dE_aeg)$coefficients[1,2]
  c1 <- as.numeric(Fitting_dE_aeg$m$getPars()[2])  - 
    summary(Fitting_dE_aeg)$coefficients[2,2]
  c2 <- as.numeric(Fitting_dE_aeg$m$getPars()[3])  - 
    summary(Fitting_dE_aeg)$coefficients[3,2]
  c*te*(te-c1)*(c2-te)^(1/2)
}

df_out_aeg_min <- data.frame(temp = vec,
                     devep_rate = sapply(vec, mod_min))
df_out_aeg_min[which(df_out_aeg_min$devep_rate < 0),2] <- 0
df_out_aeg_min$group = "min"
df_out_aeg$group = "mean"


df_out_aeg <- rbind(df_out_aeg_max,
                    df_out_aeg_min,
                    df_out_aeg)

plotdE_aeg <- ggplot(df_out_aeg) +
  geom_line(aes(temp,devep_rate,
                color = group,
                group = group, 
                alpha = group), size = 0.7) +
  geom_point(data =  df_dE_aeg, aes(temp,develop_rate),
             size = 0.9, color = "black") +
  scale_color_manual(values=c("red", "blue", "red")) + 
  scale_alpha_manual(values = c(0.5,1,0.5)) +
  xlim(c(5,40)) +
  guides( color =FALSE, alpha = FALSE) +
  ylab("Egg development time") + xlab("Temperature (Cº)") +
  theme_bw()
plotdE_aeg

############################################################
# Join plots for each specie
# Albopictus
ggarrange(plotdE_aeg  + 
            theme(text = element_text(size = 15)) ,
          plotaeg +
            theme(text = element_text(size = 15))+ ylim(c(0,1.3)))

# Aegypti
ggarrange( plotdE +
             theme(text = element_text(size = 15)),
           plotalb  +
            theme(text = element_text(size = 15)) +
            ylab("Probability from Larvae to Adult")
         )


## Eggs per femae paper: https://onlinelibrary.wiley.com/doi/full/10.1111/jvec.12187
fec <- data.frame(Temp = c(16,22,28,33,36,16,22,28,33,36,16,22,28,33,36, 20,26,30,35), 
           Number.eggs  = c(3.5,79.22,64.42,40.4,16.12,2.75,161.5,262.3,51.3,30.57,1.46,126,181.3,52.92,22.27,43,80,70,22))

ggplot(fec) + geom_point(aes(Temp, Number.eggs))

Fitting_Egg_aeg <- nls(Number.eggs ~ -c*(Temp - c1)*(Temp - c2),
                      data = fec, algorithm = "port",
                      start = list(c = 0.0001, c1 = 14, c2 = 20), 
                      lower=c(0.00001,14,10), upper = c(0.5,25,38))

summary(Fitting_Egg_aeg)

mod <- function(te){
  c <- as.numeric(Fitting_Egg_aeg$m$getPars()[1])
  c1 <- as.numeric(Fitting_Egg_aeg$m$getPars()[2])
  c2 <- as.numeric(Fitting_Egg_aeg$m$getPars()[3])
  -c*(te-c1)*(te-c2)
}

vec <- seq(0,45,0.001)
df_out_aeg <- data.frame(temp = vec,
                         egg = sapply(vec, mod))

ggplot(fec) + geom_point(aes(Temp, Number.eggs)) + 
  geom_line(data =df_out_aeg,aes(temp, egg)) + ylim (c(0,100))

### Linear function
Fitting_Egg_aeg_lin <- nls(Number.eggs ~ -c*Temp + c1,
                       data = fec,
                       start = list(c = 0.0001, c1 = 0))

summary(Fitting_Egg_aeg_lin)

AIC(Fitting_Egg_aeg, Fitting_Egg_aeg_lin)
mod <- function(te){
  c <- as.numeric(Fitting_Egg_aeg$m$getPars()[1])
  c1 <- as.numeric(Fitting_Egg_aeg$m$getPars()[2])
  -c*te+c1
}

vec <- seq(0,45,0.001)
df_out_aeg <- data.frame(temp = vec,
                         egg = sapply(vec, mod))

ggplot(fec) + geom_point(aes(Temp, Number.eggs)) + 
  geom_line(data =df_out_aeg,aes(temp, egg))
# 
# ### Gonotrophic cycle
# Path <- "~/INVASIBILITY_THRESHOLD/data/Aedes/gono_aegypti.csv"
# gono <- read.csv(Path, sep = ",")
# gono[nrow(gono)+1,] <- c(20,8)
# gono[nrow(gono)+1,] <- c(26,3)
# gono[nrow(gono)+1,] <- c(30,2.5)
# gono[nrow(gono)+1,] <- c(35,4.3)
# gono$Dur_gono <- as.numeric(gsub(pattern = ",",
#                                  replacement = ".",
#                                  gono$Dur_gono))
# head(gono)
# ggplot(gono) + geom_point(aes(Temp, Dur_gono))
# 
# Fitting_gono_aeg <- nls(Dur_gono ~ -c*(Temp - c1)*(Temp - c2),
#                        data = gono, 
#                        start = list(c = 0.0001, c1 = 10, c2 = 40))
# 
# summary(Fitting_gono_aeg)
# 
# mod <- function(te){
#   c <- as.numeric(Fitting_gono_aeg$m$getPars()[1])
#   c1 <- as.numeric(Fitting_gono_aeg$m$getPars()[2])
#   c2 <- as.numeric(Fitting_gono_aeg$m$getPars()[3])
#   -c*(te-c1)*(te-c2)
# }
# 
# vec <- seq(0,45,0.001)
# df_out_aeg <- data.frame(temp = vec,
#                          egg = sapply(vec, mod))
# 
# ggplot(gono) + geom_point(aes(Temp, Dur_gono)) + 
#   geom_line(data =df_out_aeg,aes(temp, egg)) + ylim (c(0,100))
# 
# ## Briere function
# Fitting_gono_aeg <- nls(Dur_gono ~ c*Temp*(Temp - c1)*(c2-Temp)^(1/2),
#                         data = gono, 
#                         start = list(c = 0.0001, c1 = 13, c2 = 40))
# 
# summary(Fitting_gono_aeg)
# 
# mod <- function(te){
#   c <- as.numeric(Fitting_gono_aeg$m$getPars()[1])
#   c1 <- as.numeric(Fitting_gono_aeg$m$getPars()[2])
#   c2 <- as.numeric(Fitting_gono_aeg$m$getPars()[3])
#   -c*(te-c1)*(te-c2)
# }
# 
# vec <- seq(0,45,0.001)
# df_out_aeg <- data.frame(temp = vec,
#                          egg = sapply(vec, mod))
# 
# ggplot(gono) + geom_point(aes(Temp, Dur_gono)) + 
#   geom_line(data =df_out_aeg,aes(temp, egg)) + ylim (c(0,100))
