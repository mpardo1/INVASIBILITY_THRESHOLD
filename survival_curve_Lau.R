## Written using R 4.0.3
rm(list=ls())

####Dependencies####
library(ggplot2)

### Survival curve Laura:
Path <- "~/Documentos/PHD/2023/Laura proj/DATA_LARVA_TEM.csv"
df <- read.csv(file = Path)
ggplot(df) + geom_point(aes(temp_chamber,total_lived))

# Linear model:
linear.model <-lm(df$total_lived ~ df$temp_chamber)
plot(df$temp_chamber, df$total_lived, pch=16, ylab = "Counts ", cex.lab = 1.3, col = "red" )
abline(lm(df$total_lived ~ df$temp_chamber), col = "blue")

# Example of non linear least squares:
require(graphics)
DNase1 <- subset(DNase, Run == 1)
fm1DNase1 <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
summary(fm1DNase1)

# My data nls:
Fitting <- nls(total_lived ~ (-cont*(temp_chamber-Tmin)*(temp_chamber - Tmax)),
                 data = df,
                 start = list(cont = 0.001, Tmin = 5, Tmax = 20))

summary(Fitting)


mod <- function(te){
  t0 <- as.numeric(Fitting$m$getPars()[2])
  tm <- as.numeric(Fitting$m$getPars()[3])
  c <- as.numeric(Fitting$m$getPars()[1])
  (-c*(te-t0)*(te - tm))
}

vec <- seq(0,40,0.001)
df_out <- data.frame(temp = vec, life_span <- sapply(vec, mod))
ggplot(df_out) +
  geom_point(aes(temp,life_span), size = 0.01) +
  geom_point(data = df,aes(temp_chamber,total_lived), size = 0.7, color = "red") +
  ylim(c(0,30)) + xlim(c(10,35)) + 
  ylab("Total day lived") + xlab("Temperature (Cº)") +
  theme_bw()
