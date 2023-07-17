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

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$R0_test <- runif(length(esp_can$codauto),0,5)
can_box <- esp_get_can_box()

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


#### -------------------------- Albopictus ------------------------- ####
## Thermal responses Aedes Albopictus from Mordecai 2017:
a_f_alb <- function(temp){Briere_func(0.000193,10.25,38.32,temp)} # Biting rate
TFD_f_alb <- function(temp){Briere_func(0.0488,8.02,35.65,temp)} # Fecundity
pEA_f_alb <- function(temp){Quad_func(0.00361,9.04,39.33,temp)} # Survival probability Egg-Adult
MDR_f_alb <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_f_alb <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span
b_f_alb <- function(temp){Briere_func(0.000735,15.84,36.4,temp)} # Adult life span
c_f_alb <- function(temp){Briere_func(0.000439,3.62,36.82,temp)} # Adult life span
pdr_f_alb <- function(temp){Briere_func(0.000109,10.39,43.05,temp)} # Adult life span

# R0 function by temperature:
R0_func_alb <- function(Te, hum){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  mu <- 1/lf_f_alb(Te)
  mdr <- MDR_f_alb(Te)
  probla <- pEA_f_alb(Te)
  b <- b_f_alb(Te)
  c <- c_f_alb(Te)
  pdr <- pdr_f_alb(Te)
  N <- hum
  r = 0.1
  R0 <- sqrt((a^2*b*c*exp(-mu/pdr)*f*probla*mdr)/(N*r*mu^3))
  
  return(R0)
}

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

vec <- seq(0,40,0.1)
vec1 <- seq(0.5,30,0.5)
dt_z <- setDT(expand.grid(vec,vec1))
dt_z[, R0 := mapply(R0_func_alb, Var1, Var2)]
ggplot(dt_z) + 
  geom_point(aes(Var1,Var2,color = R0)) + 
  xlab("Temperature") + ylab("Human density") +
  scale_color_viridis()

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))


###--------------PA---------------#
# Presence absence data Albopictus Spain:
Path = "~/INVASIBILITY_THRESHOLD/data/PA/PresenceAbsence_MA_BG2.Rds"
df_pa <- readRDS(Path)
df_pa <- df_pa %>% group_by(NATCODE) %>%
  summarise(sum = sum(A_PRIM_DET_OFICIAL),
            sum_MA = sum(A_PRIM_DET_CITSCI))
df_pa$PA <- ifelse(df_pa$sum == 0 & df_pa$sum_MA == 0,0,1 )
df_pa <- df_pa[,c("NATCODE", "PA")]
df_pa_new <- "~/INVASIBILITY_THRESHOLD/data/PA/NewPAalbopictus.csv"
df_pa_new <- read.csv(df_pa_new, sep = ",", header = FALSE)
df_pa_new$PA <- 1
df_pa_new <- df_pa_new[,c("V1","PA")]
colnames(df_pa_new) <- c("NATCODE", "PA")
df_pa <- df_pa[-which(df_pa$NATCODE %in% df_pa_new$NATCODE),]
df_pa <- rbind(df_pa,df_pa_new)

df_pa <- esp_can %>% left_join(df_pa)
df_pa[which(is.na(df_pa$PA)),"PA"] <- 0

ggplot(df_pa) + 
  geom_sf(aes(fill=as.factor(PA)))

df_pa <- df_pa[,c("NATCODE","PA")]
df_pa$geometry <- NULL
# write.csv(df_pa, "~/INVASIBILITY_THRESHOLD/data/PA/Albopictus_Spain_Pa.csv")

###--------------WEATHER---------------#
# Data table with tmed and R0 computed for albopictus and dengue
#####
year = "2019"
Path = paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/",year,"/")
listfile <- list.files(Path)
weather_t <- data.table()
for(file in listfile){
  Path1 <- paste0(Path,file)
  weather <- readRDS(Path1)
  weather_df <- as.data.frame(do.call(rbind, weather))
  weather_dt <- setDT(weather_df) # Convert data.frame to data.table
  rm(weather,weather_df)
  weather_t <- rbind(weather_t,weather_dt)
  
}

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

weather <- esp_can %>% 
  left_join(weather_t[which(weather_t$date == "2019-06-26"),])
ggplot(weather) + geom_sf(aes(fill=tmean))

# Extract the population density for 2019
census <- mapSpain::pobmun19
esp_can <- esp_can %>% 
  left_join(census, by = c("cmun" = "cmun","cpro" = "cpro"))
ggplot(esp_can) + geom_sf(aes(fill=pob19))
esp_can$area <- as.numeric(st_area(esp_can))/1000000
esp_can_pop <- setDT(esp_can[which(is.na(esp_can$pob19) == FALSE),c("cpro", "pob19")])
esp_can_pop$geometry <- NULL
esp_can_pop <- esp_can_pop[,.(pop=mean(pob19)), 
                           by = list(cpro)]
esp_can <- esp_can %>% left_join(esp_can_pop)
esp_can$pob19 <- ifelse(is.na(esp_can$pob19),
                        esp_can$pop,
                        esp_can$pob19)
ggplot(esp_can) + geom_sf(aes(fill=pob19), lwd = 0)
esp_can$pop_km <- esp_can$pob19/esp_can$area
## Check population density
range(esp_can$pop_km)
esp_can$dens_range <- ifelse(esp_can$pop_km < 1, 0,
                      ifelse(esp_can$pop_km < 10, 1,
                      ifelse(esp_can$pop_km < 100,2,
                      ifelse(esp_can$pop_km < 500,3,
                      ifelse(esp_can$pop_km < 1000,4,
                      ifelse(esp_can$pop_km < 5000,5,
                      ifelse(esp_can$pop_km < 10000,6,7 ) ) )))))

ggplot(esp_can) +
  geom_sf(aes(fill=as.factor(dens_range)), lwd = 0) +
  scale_fill_viridis_d(name = "Poputaion \n density")
# hist(esp_can$pop_km)
# 
## Join with weather data
esp_can_pro <- esp_can[,c("NATCODE", "cpro", "pob19", "area", "ine.prov.name")]
esp_can_pro[which(is.na(esp_can_pro$pob19)),"pob19"] <- 0
esp_can <- esp_can[,c("NATCODE","pop_km", "cpro", "pob19", "area", "name.x")]
dengue_df <- esp_can %>% left_join(weather_t)
dengue_df <- setDT(dengue_df)
# ggplot(dengue_df[which(dengue_df$date == "2019-06-26"),]) +
#   geom_sf(aes(fill=tmean))

# dengue_df$R0_daily_muni <- apply(dengue_df,1,R0_func_alb)
# ggplot(dengue_df[which(dengue_df$date == "2019-06-26"),]) +
#   geom_sf(aes(fill=R0_daily_muni), lwd = 0)

## File with data from David with imported cases
Path <- "~/INVASIBILITY_THRESHOLD/code/Dengue/importation_risk_gadm.gpkg"
import <- st_read(Path)
import <- unique(import[, c("provincia", "dengue_cases",
                            "dengue_prob", "malaria_cases",
                            "malaria_prob")])
import$geom <- NULL
import <- unique(import)

#Compute R0 for Spain for 2019
dengue_df[, R0_mean := mapply(R0_func_alb, tmean, pop_km)]
dengue_p <- esp_can %>%
  left_join(dengue_df[,c("NATCODE","R0_mean", "date" )])
ggplot(dengue_p[which(dengue_p$date == "2019-07-26"),]) +
  geom_sf(aes(fill=R0_mean), lwd = 0) + 
  scale_fill_viridis_c(option = "magma")

rm(census,df_out,esp_can)
dengue_df <- dengue_df[,c("NATCODE","R0_mean","date", 
                          "tmean","pop_km","cpro", "area", "pob19")]
dengue_df$month <- lubridate::month(dengue_df$date)
import$cpro <- ifelse(import$provincia < 10,
                      paste0("0", as.character(import$provincia)),
                      as.character(import$provincia))

dengue_df <- dengue_df %>% left_join(import)
dengue_df$R0_rel <- dengue_df$R0_mean/max(dengue_df$R0_mean)
dengue_df$risk <- dengue_df$R0_rel*dengue_df$dengue_prob
dengue_df <- dengue_df %>% left_join(df_pa)
dengue_df$risk_PA <- dengue_df$risk*dengue_df$PA

#### Check and write csv:
# R0 Dengue and risk daily municipalities daily:
head(dengue_df)
dengue_df$R0_daily_muni <- dengue_df$R0_mean
dengue_df$R0_daily_muni_rel <- dengue_df$R0_rel
dengue_df$cpro <- as.numeric(dengue_df$cpro)
dengue_R0 <- dengue_df[,c("NATCODE","cpro","R0_daily_muni",
                          "R0_daily_muni_rel",
                          "date", "month", "risk", "risk_PA",
                          "dengue_prob", "PA")]
write.csv(dengue_R0,
          "~/Dengue/output/risk_dengue_2019_daily_muni.csv")

##----------------- Check daily muni Dengue--------------------###
# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

### Dengue data frame
dengue_R0$cpro <- NULL
day_R0 <- esp_can %>% left_join(dengue_R0)
# day_R0$cpro <- as.numeric(day_R0$cpro)
day_R0 <- day_R0[which(day_R0$date == "2019-07-01"),]
ggplot(day_R0) + scale_fill_viridis_c(option="magma") +
  geom_sf(aes(fill = risk_PA), linewidth = 0.01)

####-------------------MONTHLY MUNI--------------------###
dengue_df_mon <- dengue_df[,.(tmean = mean(tmean),
                              area = min(area),
                              pop = min(pob19),
                              dens =  min(pob19)/min(area),
                              PA = min(PA),
                              PA1 = max(PA),
                              dengue_prob = min(dengue_prob),
                              dengue_prob1 = max(dengue_prob)),
                           by = list(NATCODE,month)]
dengue_df_mon$dens[which(is.na(dengue_df_mon$dens))] <- 0.1
dengue_df_mon[, R0_mean := mapply(R0_func_alb, tmean, dens)]
max_R0 <- max(dengue_df_mon$R0_mean[which(is.na(dengue_df_mon$R0_mean)==FALSE)])
dengue_df_mon$R0_rel <- dengue_df_mon$R0_mean/max_R0
dengue_df_mon$risk <- dengue_df_mon$R0_rel*dengue_df_mon$dengue_prob
dengue_df_mon$risk_PA <- dengue_df_mon$risk*dengue_df_mon$PA

####----------------MONTHLY------------------##
# R0 Dengue daily municipalities monthly:
head(dengue_df_mon)
dengue_df_mon$R0_mon_muni <- dengue_df_mon$R0_mean
dengue_df_mon$R0_mon_muni_rel <- dengue_df_mon$R0_rel
dengue_df_mon$cpro <- as.numeric(dengue_df_mon$cpro)
dengue_df_mon$date <- paste0(dengue_df_mon$month, "-2019")
dengue_R0_mon <- dengue_df_mon[,c("NATCODE","cpro","R0_mon_muni","R0_mon_muni_rel",
                                  "date", "month", "risk", "risk_PA",
                                  "dengue_prob", "PA")]
write.csv(dengue_R0_mon,
          "~/Dengue/output/risk_dengue_2019_monthly_muni.csv")

### Season Map Dengue R0 #####
dengue_df_y_s <- dengue_R0_mon[which(dengue_R0_mon$month > 2 &
                                       dengue_R0_mon$month < 12),]
dengue_df_y_s <- dengue_df_y_s[,.( meanR0 = mean(R0_mon_muni)),by = list(NATCODE)]
# dengue_df_y_s <- dengue_R0_mon[,.( meanR0 = mean(R0_mon_muni)),by = list(NATCODE)]
dengue_df_y_s$R0_rel <- dengue_df_y_s$meanR0/max(dengue_df_y_s$meanR0)
dengue_df_y_s <- esp_can %>% left_join(dengue_df_y_s)
library("latex2exp")
ggplot(dengue_df_y_s) +
  geom_sf(aes(fill = R0_rel), colour = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral",
                       name = TeX("$R0_{DENV}$")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## Check higher R0 if less dens
ggplot(dengue_df_y_s) + 
  geom_point(aes(pop_km,R0_rel)) +
  xlab("Population density") +
  ylab("R0 DENV") +
  theme_bw()

## Check results map:
# Whether the municipalities with less human density has higher R0
ggplot(dengue_df_mon) + 
  geom_point(aes(dens,R0_rel), size = 0.5) + 
  xlab("Population density") + ylab("R0 DENV") +
  theme_bw()

test <- dengue_df_mon[,c("dens","R0_rel",
                         "pop", "area", "tmean")]

##### Range R0
dengue_R0_mon_plot <- dengue_R0_mon
dengue_R0_mon_plot$cpro <- NULL
dengue_R0_mon_plot <- esp_can %>% left_join(dengue_R0_mon_plot)
range(dengue_R0_mon_plot$risk_PA[which(is.na(dengue_R0_mon_plot$risk_PA)==FALSE)])
dengue_R0_mon_plot$risk_range_PA <- ifelse(dengue_R0_mon_plot$risk_PA<6.92E-05,0,
                                        ifelse(dengue_R0_mon_plot$risk_PA<0.01,1,
                                               ifelse(dengue_R0_mon_plot$risk_PA<0.02,2,3)))

dengue_R0_mon_plot$risk_range <- ifelse(dengue_R0_mon_plot$risk<6.92E-05,0,
                                        ifelse(dengue_R0_mon_plot$risk<0.01,1,
                                               ifelse(dengue_R0_mon_plot$risk<0.02,2,3)))

ggplot(dengue_R0_mon_plot) +
  geom_sf(aes(fill = as.factor(risk_range)), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_viridis_d(option = "magma") +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month), duration = rep(1000,12))

# plot Risk specific months
## Sin PA
month_n = 5
plot_5 <- ggplot(dengue_R0_mon_plot[which(dengue_R0_mon_plot$month == month_n &
                                  is.na(dengue_R0_mon_plot$risk)==FALSE),]) +
  geom_sf(aes(fill = as.factor(risk_range)), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_viridis_d(option = "magma",limits = c(0:3), name="risk") +
  ggtitle(month_n) + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))

# Con PA
plot_5_PA <- ggplot(dengue_R0_mon_plot[which(dengue_R0_mon_plot$month == month_n &
                                            is.na(dengue_R0_mon_plot$risk)==FALSE),]) +
  geom_sf(aes(fill = as.factor(risk_range_PA)), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_viridis_d(option = "magma",limits = c(0:3),
                       name="risk") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))

### Join all the plots:
library(ggpubr)
ggarrange(plot_5,plot_7,  plot_9, 
          plot_5_PA, plot_7_PA,plot_9_PA,
          common.legend = TRUE) 

# Plot R0 specific months
dengue_R0_mon_plot$rR0_range <- ifelse(dengue_R0_mon_plot$risk<1,0,
                                        ifelse(dengue_R0_mon_plot$risk<100,1,
                                               ifelse(dengue_R0_mon_plot$risk<500,2,
                                                      ifelse(dengue_R0_mon_plot$risk<1000,3,4))))
range(dengue_R0_mon_plot$R0_mon_muni)
df_aux <-dengue_R0_mon_plot[which(is.na(dengue_R0_mon_plot$risk)==FALSE),"R0_mon_muni"]
df_aux$geometry <- NULL
min_R0 <- min(df_aux$R0_mon_muni)
max_R0 <- max(df_aux$R0_mon_muni)
month_n = 3
### RO normal absoluto
plot_3 <- ggplot(dengue_R0_mon_plot[which(dengue_R0_mon_plot$month == month_n &
                                  is.na(dengue_R0_mon_plot$R0_mon_muni)==FALSE),]) +
  geom_sf(aes(fill = R0_mon_muni_rel), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral",
                       limits = c(min_R0,max_R0),
                       name = "R0 Dengue",
                       breaks = c(0,4000,8000)) +
  ggtitle(month_n) +  theme(plot.title = element_text(hjust = 0.5))

library("latex2exp")
## R0 relativo
month_n = 2
plot_2 <- ggplot(dengue_R0_mon_plot[which(dengue_R0_mon_plot$month == month_n &
                                             is.na(dengue_R0_mon_plot$R0_mon_muni)==FALSE),]) +
  geom_sf(aes(fill = R0_mon_muni_rel), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral",
                       limits = c(0,1),
                       name = TeX("$R0_{DENV}$"),
                       breaks = c(0,0.5,1)) +
  ggtitle(month_n) + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## Join all the plots
ggarr <- ggarrange(plot_3,plot_4,plot_5,
          plot_6,plot_7,plot_8,
          plot_9,plot_10,plot_11,
          nrow=3,ncol = 3, common.legend = TRUE)
ggarr

test <- dengue_R0_mon_plot[which(dengue_R0_mon_plot$month == month_n &
                           is.na(dengue_R0_mon_plot$R0_mon_muni)==FALSE),]
ggplot(test) + geom_point(aes(pop_km,R0_mon_muni_rel))

### Plot R0>1 or R0_rel>0
dengue_R0_mon_plot$geometry <- NULL
dengue_R0_mon_plot$bool <- ifelse(dengue_R0_mon_plot$R0_mon_muni>1,1,0)
dengue_R0_mon_plot$bool_rel <- ifelse(dengue_R0_mon_plot$R0_mon_muni_rel>0,1,0)
dengue_R0_mon_plot <- setDT(dengue_R0_mon_plot)
dengue_R0_mon_plot_a <- dengue_R0_mon_plot[,.(sum_R0 = sum(bool),
                                              sum_R0_rel = sum(bool_rel)),
                                           by=list(NATCODE)]

dengue_R0_mon_plot_a <- esp_can %>% left_join(dengue_R0_mon_plot_a)
colors <- c("#43A2CA", "#7BCCC4", "#BAE4BC", "#F0F9E8",
            "#FFF7EC","#FEE8C8","#FDD49E","#FDBB84",
            "#FC8D59","#EF6548","#D7301F", "#B30000",
            "#7F0000")

ggplot(dengue_R0_mon_plot_a) +
  geom_sf(aes(fill = as.factor(sum_R0_rel)), colour = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_manual(values = colors,
                    name = "Nº months\n suitable",
                    limits = factor(seq(0,12,1))) +
  theme_bw() 

## Check Zivko map:
Path <- "~/Documentos/PHD/2023/Dengue/risk_dengue_2019_daily_muni.csv"
df_denv <- read.csv(Path)
hist(df_denv$R0_daily_muni_rel)
hist(df_denv$R0_daily_muni_rel[which(df_denv$R0_daily_muni_rel>0.5)])
####--------------------DAILY PROVINCES---------------------####
# # test = dengue_df %>% group_by(NATCODE, date) %>% summarise(n=n())
# dengue_df1 <- dengue_df[which(dengue_df$cpro != 51 & dengue_df$cpro != 52),]
# dengue_df_d_p <- dengue_df1[, .(tmean = mean(tmean),
#                                    pop = sum(pob19),
#                                    area = sum(area),
#                                    dens = (sum(pob19)/sum(area))), 
#                                by=list(cpro,date)]
# dengue_df_d_p$dens[which(is.finite(dengue_df_d_p$dens)==FALSE)] <- 10
# dengue_df_d_p[, R0 := mapply(R0_func_alb, tmean, dens)]
# dengue_df_d_p[which(is.finite(dengue_df_d_p$R0)==FALSE),]
# dengue_df_d_p$cpro <- as.numeric(dengue_df_d_p$cpro)
# sum(is.na(dengue_df_d_p))
# 
# import <- unique(import[which(is.na(import$provincia)==FALSE | is.na(import$dengue_prob)==FALSE),])
# # sum(is.na(import))
# # sum(is.na(df_den))
# dengue_df_d_p <- dengue_df_d_p %>%
#   left_join(unique(import), by = c("cpro" = "provincia"))
# 
# sum(is.na(dengue_df_d_p))
# dengue_df_d_p <- dengue_df_d_p[which(is.na(dengue_df_d_p$cpro) == FALSE),]
# dengue_df_d_p$R0_rel <- dengue_df_d_p$R0/max(dengue_df_d_p$R0)
# dengue_df_d_p$R0_bool <- ifelse(dengue_df_d_p$R0>1,1,0 )
# 
# ## Compute the relative risk : Dengue proability * R0 Dengue relative
# dengue_df_d_p$risk <- dengue_df_d_p$dengue_prob*dengue_df_d_p$R0_rel
# 
# # step <- range(dengue_df_d_p$risk)/3
# dengue_df_d_p$risk_range <- ifelse((dengue_df_d_p$risk < 0.000357),
#                                    0,
#                                    ifelse(dengue_df_d_p$risk < 0.01, 1,
#                                           ifelse(dengue_df_d_p$risk < 0.02, 2,3)))
# 
# esp_can <- esp_get_prov_siane(moveCAN = TRUE)
# # ## Delete Ceuta and Melilla from the analysis
# # esp_can <- esp_can[which(esp_can$cpro != 51 & esp_can$cpro != 52 ),"cpro",
# #                    "ine.prov.name"]
# esp_can$cpro <- as.numeric(esp_can$cpro)
# dengue_df_d_p <- esp_can %>% left_join(dengue_df_d_p)
# 
# test <- dengue_df_d_p %>% group_by(date) %>% summarize(n=n())
# test$geometry <- NULL
# dengue_df_d_p[which(is.na(dengue_df_d_p$risk)),]
# dengue_df_d_p <- dengue_df_d_p[-which(is.na(dengue_df_d_p$risk_range)),]
# 
# ## Create plots:
# ggplot(dengue_df_d_p) +
#   geom_sf(aes(fill = as.factor(risk_range)), linewidth = 0.01) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   scale_fill_brewer(palette = "YlOrRd",
#                     name = "Dengue risk") +
#   theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
#   theme_void() +
#   labs(title = "Day: {current_frame}") +
#   transition_manual(as.factor(date))
# 
# ## Dengue prob David
# ggplot(dengue_df_d_p[which(dengue_df_d_p$date == "2019-01-01"),]) +
#   geom_sf(aes(fill = dengue_prob), linewidth = 0.01) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
#   scale_fill_viridis_c(name = "Dengue prob") +
#   theme_void() 
# 
# #---------------------------province and month--------------------------#
# # Agrupate by month
# dengue_df_g <- dengue_df[, .(R0_tmed=mean(R0_mean),
#                              tmean = mean(tmean),
#                              pop = min(pop_km)), 
#                          by=list(NATCODE,month)]
# 
# dengue_df_g[, R0_mon := mapply(R0_func_alb, tmean, pop)]
# dengue_df_g[which(is.na(dengue_df_g$R0_mon)),]
# 
# dengue_df_g_p <- dengue_df_g %>%
#   left_join(esp_can_pro)
# dengue_df_g_p[which(is.na(dengue_df_g_p$R0_mon)),]
# 
# dengue_df_g_p <- dengue_df_g_p[, .(tmean = mean(tmean),
#                                    pop = sum(pob19),
#                                    area = sum(area),
#                                    dens = (sum(pob19)/sum(area))), 
#                                by=list(cpro,month)]
# 
# dengue_df_g_p[, R0_mon_prov := mapply(R0_func_alb, tmean, dens)]
# 
# esp_can <- esp_get_prov_siane(moveCAN = TRUE)
# ## Delete Ceuta and Melilla from the analysis
# esp_can <- esp_can[which(esp_can$cpro != 51 & esp_can$cpro != 52 ),"cpro",
#                    "ine.prov.name"]
# dengue_df_g_p <- esp_can %>% left_join(dengue_df_g_p) 
# 
# ## August
# ggplot(dengue_df_g_p[which(dengue_df_g_p$month == 6),]) +
#   geom_sf(aes(fill = R0_mon_prov), linewidth = 0.01) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
#   scale_fill_viridis_c(name = "R0 Dengue") +
#   theme_void() 
# 
# ## August
# ggplot(dengue_df_g_p[which(dengue_df_g_p$month == 6),]) +
#   geom_sf(aes(fill = R0_rel), linewidth = 0.01) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
#   scale_fill_viridis_c(name = "R0 Dengue") +
#   theme_void() 
# 
# # # Create plots:
# ggplot(dengue_df_g_p) +
#   geom_sf(aes(fill = R0_mon_prov), linewidth = 0.01) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
#   scale_fill_viridis_c() +
#   theme_void() +
#   labs(title = "Month: {current_frame}") +
#   transition_manual(as.factor(month), duration = rep(1000,12))
# 
# ## File with data province and monthly
# dengue_df_g_p$cpro <- as.numeric(dengue_df_g_p$cpro)
# dengue_df_g_p <- dengue_df_g_p %>%
#   left_join(unique(import), by = c("cpro" = "provincia"))
# dengue_df_g_p$R0_rel <- dengue_df_g_p$R0_mon_prov/max(dengue_df_g_p$R0_mon_prov)
# dengue_df_g_p$R0_bool <- ifelse(dengue_df_g_p$R0_mon_prov>1,1,0 )
# 
# ## Compute the relative risk : Dengue proability * R0 Dengue relative
# dengue_df_g_p$risk <- dengue_df_g_p$dengue_prob*dengue_df_g_p$R0_rel
# dengue_df_g_p <- dengue_df_g_p[which(is.na(dengue_df_g_p$cpro ) == FALSE),]
# dengue_df_g_p$risk_range <- ifelse((dengue_df_g_p$risk < 6.92879e-05),0,
#                                    ifelse(dengue_df_g_p$risk < 0.01, 1,
#                                           ifelse(dengue_df_g_p$risk < 0.02, 2,3)))
# 
# max(dengue_df_g_p$risk[which(dengue_df_g_p$dengue_prob<0.000357)])
# 
# range(dengue_df_g_p$risk) 
# plot(dengue_df_g_p$dengue_prob, dengue_df_g_p$R0_rel)
# 
# ## Create plots:
# ggplot(dengue_df_g_p) +
#   geom_sf(aes(fill = as.factor(risk_range)), linewidth = 0.01) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
#   theme_void() +
#   scale_fill_viridis_d() +
#   labs(title = "Month: {current_frame}") +
#   transition_manual(as.factor(month), duration = rep(1000,12))
# 
# ggplot(dengue_df_g_p) +
#   geom_sf(aes(fill = risk), linewidth = 0.01) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
#   theme_void() +
#   scale_fill_viridis_c(option = "magma") +
#   labs(title = "Month: {current_frame}") +
#   transition_manual(as.factor(month), duration = rep(1000,12))
# 
# # Plot August:
# ggplot(dengue_df_g_p[which(dengue_df_g_p$month == 1),]) +
#   geom_sf(aes(fill = as.factor(risk_range)), linewidth = 0.01) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
#   scale_fill_viridis_d(name = "R0 Dengue") +
#   theme_void() 
# 
# # hist(dengue_df_g_p$risk)
# # plot(dengue_df_g_p$risk, dengue_df_g_p$R0_bool)
# # plot(dengue_df_g_p$dengue_prob, dengue_df_g_p$R0_bool)
# # plot(dengue_df_g_p$dengue_prob, dengue_df_g_p$R0_mon_prov)
# # range(dengue_df_g_p$risk)
# #-----------------------------------------------------------------------#
# ##Data frames:
# # Risk daily provinces:
# head(dengue_df_d_p)
# dengue_df_d_p$R0_daily_prov <- dengue_df_d_p$R0
# dengue_df_d_p$R0_daily_prov_rel <- dengue_df_d_p$R0_rel
# dengue_df_d_p$risk_daily_prov <- dengue_df_d_p$risk
# dengue_df_d_p$risk_range_daily_prov <- dengue_df_d_p$risk_range
# risk_d_p <- dengue_df_d_p[, c("cpro", "date", "R0_daily_prov_rel",
#                               "R0_daily_prov","risk_daily_prov")]
# risk_d_p$geometry <- NULL
# write.csv(risk_d_p,
#           "~/INVASIBILITY_THRESHOLD/code/Dengue/output/risk_dengue_2019_daily_prov.csv")
# 
# # risk_d_p <- read.csv("~/INVASIBILITY_THRESHOLD/code/Dengue/output/risk_dengue_2019_daily_prov.csv")
# 
# # Risk monthly provinces:
# head(dengue_df_g_p)
# dengue_df_g_p$R0_mon_prov_rel <- dengue_df_g_p$R0_rel
# dengue_df_g_p$risk_mon_prov <- dengue_df_g_p$risk
# dengue_df_g_p$risk_range_mon_prov <- dengue_df_g_p$risk_range
# risk_m_p <- dengue_df_g_p[, c("cpro", "month", "R0_rel","R0_mon_prov","dengue_prob",
#                               "risk_mon_prov")]
# risk_m_p$geometry <- NULL
# risk_m_p$month1 <- ifelse(risk_m_p$month <10, paste0("0", risk_m_p$month),
#                           as.character(risk_m_p$month))
# risk_m_p$date <- paste0(risk_m_p$month1,"-2019")
# risk_m_p$month1 <- NULL
# colnames(risk_m_p)[4] <- "R0_rel_mon_prov"
# write.csv(risk_m_p,
#           "~/Dengue/output/risk_dengue_2019_monthly_prov.csv")

# # ## Join all the data frames: 
# risk_d_p <- read.csv("~/Dengue/output/risk_dengue_2019_daily_prov.csv")
# risk_m_p <- read.csv("~/Dengue/output/risk_dengue_2019_monthly_prov.csv")
# dengue_R0 <- read.csv("~/Dengue/output/risk_dengue_2019.csv")
# 
# risk_dengue <- dengue_R0 %>% left_join(risk_d_p)
# risk_dengue <- risk_dengue %>% left_join(risk_m_p)
# write.csv(risk_dengue,
#           "~/INVASIBILITY_THRESHOLD/code/Dengue/output/risk_dengue_2019.csv")

###### SHAPE FILES #####
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
## Delete Ceuta and Melilla from the analysis
esp_can <- esp_can[which(esp_can$cpro != 51 & esp_can$cpro != 52 ),c("cpro",
                   "ine.prov.name", "ine.ccaa.name", "name", "NATCODE")]
esp_can$cpro <- as.numeric(esp_can$cpro)
str(esp_can)
st_write(esp_can,
         "~/INVASIBILITY_THRESHOLD/code/Dengue/output/spain_muni.gpkg", "nc")

## Provinces map Spain
esp_can <- esp_get_prov_siane(moveCAN = TRUE)
## Delete Ceuta and Melilla from the analysis
esp_can <- esp_can[,
                   c("cpro", "ine.prov.name", "nuts2.name")]
colnames(esp_can) <- c("cpro", "ine.prov.name", "ine.ccaa.name", "geometry")
esp_can$cpro <- as.numeric(esp_can$cpro)

st_write(esp_can,
         "~/INVASIBILITY_THRESHOLD/code/Dengue/output/spain_prov.gpkg", "nc")

## Checks:
can_box <- esp_get_can_box()
dengue_R0_mon_plot <- dengue_R0_mon
dengue_R0_mon_plot$cpro <- NULL
dengue_R0_mon_plot <- esp_can %>% left_join(dengue_R0_mon_plot)
ggplot(dengue_R0_mon_plot) +
  geom_sf(aes(fill = risk_PA), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month), duration = rep(1000,12))

# risk_d_p$X <- NULL
# import <- unique(import[, c("provincia", "dengue_prob")])
# colnames(import) <- c("cpro", "dengue_prob")
# risk_d_p <- risk_d_p %>% left_join(import)
# day_prov <- esp_can %>% left_join(risk_d_p)
# ggplot(day_prov) +
#   geom_sf(aes(fill = R0_daily_prov_rel), linewidth = 0.01) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
#   theme_void() +
#   labs(title = "Day: {current_frame}") +
#   transition_manual(as.factor(date))


# 
# risk_m_p$month1 <- ifelse(risk_m_p$month <10, paste0("0", risk_m_p$month),
#                           as.character(risk_m_p$month))
# risk_m_p$date <- paste0(risk_m_p$month1,"-2019")
# risk_m_p$month1 <- NULL
