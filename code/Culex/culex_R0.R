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
  outp <- -cte*temp + c1
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
  
  # hatch <- (1-erat)*(((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) + e0)) +
  #   erat*(edens/(edens + exp(-efac*hum)))
  hatch <- ((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) )
  return(hatch)
}

#### -------------------------- CULEX ------------------------- ####
## Thermal responses Culex Rachel Taylor:
a_f_cul <- function(temp){Quad_func(0.0000674,8.72,250.60,temp)} # Biting rate
TFD_f_cul <- function(temp){Quad_func(0.64,5.99,38.94,temp)} # Fecundity
deltaL_f_cul <- function(temp){1-Quad_func(0.00435,7.54,36.45,temp)} # Larva mortality
dL_f_cul <- function(temp){Briere_func(0.0000608,5.41,36.55,temp)} # larva development rate
lf_f_cul <- function(temp){Lin_func(5.24,178.32,temp)} # Adult life span
dE_f_cul <- function(temp){Lin_func(-0.031,-0.21,temp)} # Egg development rate
deltaE_f_cul <- function(temp){1-Quad_func(0.00265,-0.91,34.26,temp)} # Egg morality rate

# R0 function by temperature:
R0_func_cul <- function(Te, hum, rain){
  a <- a_f_cul(Te)
  f <- (1/2)*TFD_f_cul(Te)
  deltaa <- lf_f_cul(Te)
  dE <- dE_f_cul(Te)
  deltaE <- deltaE_f_cul(Te)
  h <- h_f(hum,rain)
  dL <- dL_f_cul(Te)
  deltaL <- deltaL_f_cul(Te)
  R0 <- sqrt(f*(a*deltaa)*(dL/(dL+deltaL))*((dE*h)/(dE*h + deltaE)))
  if(is.na(hum)){R0 <- NA}
  return(R0)
}

### Plot bitting rate, a
vec <- seq(8,30,0.01)
out <- sapply(vec,a_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))


### Plot bitting rate, a
vec <- seq(8,30,0.01)
out <- sapply(vec,deltaE_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

### Plot fecundity, F
vec <- seq(0,40,0.01)
out <- sapply(vec,TFD_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

### Plot Larva morality
vec <- seq(0,40,0.01)
out <- sapply(vec,deltaL_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

### Plot Larva development rate
vec <- seq(0,40,0.01)
out <- sapply(vec,dL_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

### Plot development rate egg, dE
vec <- seq(0,40,0.01)
out <- sapply(vec,dE_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

### Plot adult longevity, lf
vec <- seq(0,40,0.01)
out <- sapply(vec,lf_f_cul)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

## R0
vec <- seq(0,40,0.01)
out <- sapply(vec,R0_func_cul, rain = 6, hum = 500)

letsize = 14
df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out)) + 
  geom_hline(aes(yintercept = 1), linetype = "dashed", color = "red") +
  xlab("Temperature, Cº") + ylab(TeX("$R_M$")) + xlim(c(5,35)) +
  theme_bw() + theme(text = element_text(size = letsize)) 

df_out[which(df_out$out == max(df_out$out)),"vec"]

# Compute R0 for culex
clim_pop <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_clim_",2020,".Rds"))
clim_pop[, R0_cul := mapply(R0_func_cul, tmean, pop, prec)]

# Save lon lat
grid_points <- unique(clim_pop[,c("id", "lon", "lat")])

# test 
ggplot(clim_pop[clim_pop$month == 1,]) +
  geom_raster(aes(x = lon, y = lat, 
                  fill = R0_cul),alpha = 1) +
  scale_fill_viridis_c() + theme_minimal()

# Create bool and aggregate
clim_pop$bool_cul <- ifelse(clim_pop$R0_cul>1,1,0)
clim_pop <- clim_pop[,.(sum_cul = sum(bool_cul)), by = list(id)]

# add the lon lat -------------------------------------
clim_pop <- clim_pop %>% left_join(grid_points)

# Plot R0_cul
library(RColorBrewer)
name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal[11]
pal[12] = "#74011C"
pal[13] = "#4B0011"
letsize = 16
ggplot(clim_pop) +
  geom_raster(aes(x = lon, y = lat, 
                  fill = as.factor(sum_cul)),alpha = 1) +
  scale_fill_manual(values = pal,
                    name = "Nº suitable \n months",
                    limits = factor(seq(0,12,1)),
                    na.value = "#FCFCFC") +
  xlab("") + ylab("") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks.length = unit(0, "null"),
        axis.ticks.margin = unit(0, "null"), 
        # legend.position = "none",
        legend.box = "horizontal") +
  coord_equal()
