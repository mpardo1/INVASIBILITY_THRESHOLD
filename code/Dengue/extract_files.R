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
b_f_alb <- function(temp){Briere_func(0.000723,16,36.3,temp)} # Adult life span
c_f_alb <- function(temp){Briere_func(0.00433,2.8,36.7,temp)} # Adult life span
pdr_f_alb <- function(temp){Briere_func(0.00433,2.8,36.7,temp)} # Adult life span

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
  scale_color_viridis()

library("plot3D")
plot3d(x=dt_z$Var1,y=dt_z$Var2,z=dt_z$R0, col = my_palette[dt_z$R0 * 100],
       cex.lab=10,cex.axis=5,
       xlab = "Human density (km2)", ylab = "Rainfall(mm day)",
       zlab = "Haching rate") 

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
esp_can$area <- as.numeric(st_area(esp_can))/1000000
esp_can$pop_km <- esp_can$pob19/esp_can$area
esp_can[which(is.na(esp_can$pop_km)),"pop_km"] <- 1
# hist(esp_can$pop_km)
# ggplot(esp_can) + geom_sf(aes(fill=pop_km), lwd = 0)

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

#Compute R0 for Spain for 2019
dengue_df[, R0_mean := mapply(R0_func_alb, tmean, pop_km)]
dengue_p <- esp_can %>% left_join(dengue_df[,c("NATCODE","R0_mean", "date" )])
ggplot(dengue_p[which(dengue_p$date == "2019-07-26"),]) +
  geom_sf(aes(fill=R0_mean)) + 
  scale_fill_viridis_c(option = "magma")

rm(census,df_out,esp_can)
dengue_df <- dengue_df[,c("NATCODE","R0_mean","date", 
                          "tmean","pop_km","cpro", "area", "pob19")]
dengue_df$month <- lubridate::month(dengue_df$date)
# 51 code for Ceuta


###--------------------DAILY PROVINCES---------------------####
# test = dengue_df %>% group_by(NATCODE, date) %>% summarise(n=n())
dengue_df1 <- dengue_df[which(dengue_df$cpro != 51 & dengue_df$cpro != 52),]
dengue_df_d_p <- dengue_df1[, .(tmean = mean(tmean),
                                   pop = sum(pob19),
                                   area = sum(area),
                                   dens = (sum(pob19)/sum(area))), 
                               by=list(cpro,date)]
dengue_df_d_p$dens[which(is.finite(dengue_df_d_p$dens)==FALSE)] <- 10
dengue_df_d_p[, R0 := mapply(R0_func_alb, tmean, dens)]
dengue_df_d_p[which(is.finite(dengue_df_d_p$R0)==FALSE),]
dengue_df_d_p$cpro <- as.numeric(dengue_df_d_p$cpro)
sum(is.na(dengue_df_d_p))

import <- unique(import[which(is.na(import$provincia)==FALSE | is.na(import$dengue_prob)==FALSE),])
# sum(is.na(import))
# sum(is.na(df_den))
dengue_df_d_p <- dengue_df_d_p %>%
  left_join(unique(import), by = c("cpro" = "provincia"))

sum(is.na(dengue_df_d_p))
dengue_df_d_p <- dengue_df_d_p[which(is.na(dengue_df_d_p$cpro) == FALSE),]
dengue_df_d_p$R0_rel <- dengue_df_d_p$R0/max(dengue_df_d_p$R0)
dengue_df_d_p$R0_bool <- ifelse(dengue_df_d_p$R0>1,1,0 )

## Compute the relative risk : Dengue proability * R0 Dengue relative
dengue_df_d_p$risk <- dengue_df_d_p$dengue_prob*dengue_df_d_p$R0_rel

# step <- range(dengue_df_d_p$risk)/3
dengue_df_d_p$risk_range <- ifelse((dengue_df_d_p$risk < max(dengue_df_d_p$risk[dengue_df_d_p$R0<1])),
                                   "No risk",
                                   ifelse(dengue_df_d_p$risk < 0.01, "Low risk",
                                          ifelse(dengue_df_d_p$risk < 0.02, "Medium risk","High risk")))

esp_can <- esp_get_prov_siane(moveCAN = TRUE)
# ## Delete Ceuta and Melilla from the analysis
# esp_can <- esp_can[which(esp_can$cpro != 51 & esp_can$cpro != 52 ),"cpro",
#                    "ine.prov.name"]
esp_can$cpro <- as.numeric(esp_can$cpro)
dengue_df_d_p <- esp_can %>% left_join(dengue_df_d_p)

test <- dengue_df_d_p %>% group_by(date) %>% summarize(n=n())
test$geometry <- NULL
dengue_df_d_p[which(is.na(dengue_df_d_p$risk)),]

## Create plots:
ggplot(dengue_df_d_p) +
  geom_sf(aes(fill = risk_range), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Day: {current_frame}") +
  transition_manual(as.factor(date))

#---------------------------province and month--------------------------#
# Agrupate by month
dengue_df_g <- dengue_df[, .(R0_tmed=mean(R0_mean),
                             tmean = mean(tmean),
                             pop = min(pop_km)), 
                         by=list(NATCODE,month)]

dengue_df_g[, R0_mon := mapply(R0_func_alb, tmean, pop)]
dengue_df_g[which(is.na(dengue_df_g$R0_mon)),]

dengue_df_g_p <- dengue_df_g %>%
  left_join(esp_can_pro)
dengue_df_g_p[which(is.na(dengue_df_g_p$R0_mon)),]

dengue_df_g_p <- dengue_df_g_p[, .(tmean = mean(tmean),
                                   pop = sum(pob19),
                                   area = sum(area),
                                   dens = (sum(pob19)/sum(area))), 
                               by=list(cpro,month)]

dengue_df_g_p[, R0_mon_prov := mapply(R0_func_alb, tmean, dens)]

esp_can <- esp_get_prov_siane(moveCAN = TRUE)
## Delete Ceuta and Melilla from the analysis
esp_can <- esp_can[which(esp_can$cpro != 51 & esp_can$cpro != 52 ),"cpro",
                   "ine.prov.name"]
dengue_df_g_p <- esp_can %>% left_join(dengue_df_g_p) 

# # Create plots:
ggplot(dengue_df_g_p) +
  geom_sf(aes(fill = R0_mon_prov), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month), duration = rep(1000,12))

## File with data province and monthly
dengue_df_g_p$cpro <- as.numeric(dengue_df_g_p$cpro)
dengue_df_g_p <- dengue_df_g_p %>%
  left_join(unique(import), by = c("cpro" = "provincia"))
dengue_df_g_p$R0_rel <- dengue_df_g_p$R0_mon_prov/max(dengue_df_g_p$R0_mon_prov)
dengue_df_g_p$R0_bool <- ifelse(dengue_df_g_p$R0_mon_prov>1,1,0 )
## Compute the relative risk : Dengue proability * R0 Dengue relative
dengue_df_g_p$risk <- dengue_df_g_p$dengue_prob*dengue_df_g_p$R0_rel
dengue_df_g_p <- dengue_df_g_p[which(is.na(dengue_df_g_p$cpro ) == FALSE),]

# # Create plots:
ggplot(dengue_df_g_p) +
  geom_sf(aes(fill = risk_range), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month), duration = rep(1000,12))
# 
# hist(dengue_df_g_p$risk)
# plot(dengue_df_g_p$risk, dengue_df_g_p$R0_bool)
# plot(dengue_df_g_p$dengue_prob, dengue_df_g_p$R0_bool)
# plot(dengue_df_g_p$dengue_prob, dengue_df_g_p$R0_mon_prov)
# range(dengue_df_g_p$risk)

dengue_df_g_p$risk_range <- ifelse((dengue_df_g_p$risk < max(dengue_df_g_p$risk[dengue_df_g_p$R0_mon_prov<1])),
                                   "No risk",
                                   ifelse(dengue_df_g_p$risk < 0.01, "Low risk",
                                          ifelse(dengue_df_g_p$risk < 0.02, "Medium risk","High risk")))

ggplot(dengue_df_g_p) +
  geom_sf(aes(fill = risk_range), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month), duration = rep(1000,12))

#-----------------------------------------------------------------------#
##Data frames:

# Risk daily provinces:
head(dengue_df_d_p)
dengue_df_d_p$R0_daily_prov <- dengue_df_d_p$R0
dengue_df_d_p$R0_daily_prov_rel <- dengue_df_d_p$R0_rel
dengue_df_d_p$risk_daily_prov <- dengue_df_d_p$risk
dengue_df_d_p$risk_range_daily_prov <- dengue_df_d_p$risk_range
risk_d_p <- dengue_df_d_p[, c("cpro", "date", "R0_daily_prov_rel",
                              "R0_daily_prov","risk_daily_prov")]
risk_d_p$geometry <- NULL
write.csv(risk_d_p,
          "~/INVASIBILITY_THRESHOLD/code/Dengue/output/risk_dengue_2019_daily_prov.csv")

# risk_d_p <- read.csv("~/INVASIBILITY_THRESHOLD/code/Dengue/output/risk_dengue_2019_daily_prov.csv")

# Risk monthly provinces:
head(dengue_df_g_p)
dengue_df_g_p$R0_mon_prov_rel <- dengue_df_g_p$R0_rel
dengue_df_g_p$risk_mon_prov <- dengue_df_g_p$risk
dengue_df_g_p$risk_range_mon_prov <- dengue_df_g_p$risk_range
risk_m_p <- dengue_df_g_p[, c("cpro", "month", "R0_rel","R0_mon_prov","dengue_prob",
                              "risk_mon_prov")]
risk_m_p$geometry <- NULL
risk_m_p$month1 <- ifelse(risk_m_p$month <10, paste0("0", risk_m_p$month),
                          as.character(risk_m_p$month))
risk_m_p$date <- paste0(risk_m_p$month1,"-2019")
risk_m_p$month1 <- NULL
colnames(risk_m_p)[4] <- "R0_rel_mon_prov"
write.csv(risk_m_p,
          "~/Dengue/output/risk_dengue_2019_monthly_prov.csv")

# R0 Dengue daily municipalities:
head(dengue_df)
dengue_df$R0_daily_muni <- dengue_df$R0_mean
dengue_df$cpro <- as.numeric(dengue_df$cpro)
dengue_R0 <- dengue_df[,c("NATCODE","cpro","R0_daily_muni", "date", "month")]
write.csv(dengue_R0,
          "~/INVASIBILITY_THRESHOLD/code/Dengue/output/Dengue_R0_2019_daily_muni.csv")
rm(dengue_df)

# ## Join all the data frames: 
risk_d_p <- read.csv("~/Dengue/output/risk_dengue_2019_daily_prov.csv")
risk_m_p <- read.csv("~/Dengue/output/risk_dengue_2019_monthly_prov.csv")
dengue_R0 <- read.csv("~/Dengue/output/risk_dengue_2019.csv")

risk_dengue <- dengue_R0 %>% left_join(risk_d_p)
risk_dengue <- risk_dengue %>% left_join(risk_m_p)
write.csv(risk_dengue,
          "~/INVASIBILITY_THRESHOLD/code/Dengue/output/risk_dengue_2019.csv")

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
mon_prov <- esp_can %>% left_join(risk_m_p)
ggplot(mon_prov) +
  geom_sf(aes(fill = risk_mon_prov), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month), duration = rep(1000,12))

risk_d_p$X <- NULL
import <- unique(import[, c("provincia", "dengue_prob")])
colnames(import) <- c("cpro", "dengue_prob")
risk_d_p <- risk_d_p %>% left_join(import)
day_prov <- esp_can %>% left_join(risk_d_p)
ggplot(day_prov) +
  geom_sf(aes(fill = R0_daily_prov_rel), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Day: {current_frame}") +
  transition_manual(as.factor(date))

dengue_R0$cpro <- NULL
day_R0 <- esp_can %>% left_join(dengue_R0)
day_R0$cpro <- as.numeric(day_R0$cpro)
day_R0 <- day_R0[which(day_R0$date == "2019-05-01"),]
ggplot(day_R0) +
  geom_sf(aes(fill = R0_daily_muni), linewidth = 0.01)

esp_can$R0 <- rnorm(nrow(esp_can),0,1)
ggplot(esp_can) + 
  geom_sf(aes(fill =R0))

risk_m_p$month1 <- ifelse(risk_m_p$month <10, paste0("0", risk_m_p$month),
                          as.character(risk_m_p$month))
risk_m_p$date <- paste0(risk_m_p$month1,"-2019")
risk_m_p$month1 <- NULL
