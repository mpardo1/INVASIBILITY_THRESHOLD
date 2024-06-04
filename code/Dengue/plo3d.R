rm(list=ls())
library("mapSpain")
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library("ggpubr")
library(viridis)
library(stringr)
library(gdata)
library("data.table")
library(parallel)
library(gganimate)
library(plto3d)

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$R0_test <- runif(length(esp_can$codauto),0,5)
can_box <- esp_get_can_box()

#Add source file
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")

# R0 with temp cte
vec <- seq(0,1000,10)
te_cte <- 20
out <- sapply(vec,R0_func_alb,Te=te_cte)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

# R0 with hum density cte
vec <- seq(0,40,0.1)
hum_cte <- 20
out <- sapply(vec,R0_func_alb,hum=hum_cte)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

## 3D plot R0
library(plotly)

vec <- seq(0,500, length = 500)
vec1 <- seq(0,18, length = 500)
dt_z <- setDT(expand.grid(vec,vec1))
dt_z[, h := mapply(h_f, Var1, Var2)]
ggplot(dt_z) + 
  geom_point(aes(Var1,Var2,color = h)) + 
  scale_color_viridis()

library(rgl)
library(RColorBrewer)
# Create a color palette using the color_brewer package
my_colors <- c("yellow","red", "black")
my_palette <- colorRampPalette(my_colors)(8000)

library("plot3D")
plot3d(x=dt_z$Var1,y=dt_z$Var2,z=dt_z$h, 
       col = my_palette[dt_z$h+1],
       cex.lab=10,cex.axis=5,
       xlab = "Temperature", ylab = "Human density",
       zlab = "R0 Dengue") 

####-------------Hatching rate-----------------#
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
# Plot 3D function hatching rates
y <- seq(0, 16, length= 200)
x <- seq(0, 1000, length= 1000)
df_xy <- expand.grid(x,y)
df_xy$z <- 0
for(i in c(1:nrow(df_xy))){
  print(i)
  df_xy[i,3] <- h_f(df_xy$Var1[i], df_xy$Var2[i])
}
# 
Path <- "~/Documentos/PHD/2023/outputhatching.Rds"
saveRDS(df_xy,Path)
 df_xy <- readRDS(Path)

library(rgl)
library(RColorBrewer)
# Create a color palette using the color_brewer package
my_colors <- c("yellow","blue", "black")
my_palette <- colorRampPalette(my_colors)(100)

plot3d(x=df_xy$Var1,y=df_xy$Var2,z=df_xy$z,
       col = my_palette[df_xy$z * 100],
       cex.lab=10,cex.axis=5,
       xlab = "Human density (km2)", ylab = "Rainfall(mm day)",
       zlab = "Haching rate") 
