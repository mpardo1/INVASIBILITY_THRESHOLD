rm(list=ls())
library(mapSpain)
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library("data.table")
library(tidyr)
library(parallel)

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
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
### Incorporating rain and human density:
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

## Thermal responses Aedes Albopictus from Mordecai 2017:
a_f_alb <- function(temp){Briere_func(0.000193,10.25,38.32,temp)} # Biting rate
TFD_f_alb <- function(temp){Briere_func(0.0488,8.02,35.65,temp)} # Fecundity
pLA_f_alb <- function(temp){Quad_func(2.663e-03,6.668e+00,3.892e+01,temp)} # Survival probability Egg-Adult
MDR_f_alb <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_f_alb <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span
dE_f_alb <- function(temp){4.66e-03*temp -4.23e-02} # Adult life span

# R0 function by temperature:
R0_func_alb <- function(rain,hum,Te){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  deltaa <- lf_f_alb(Te)
  dE <- dE_f_alb(Te)
  probla <- pLA_f_alb(Te)
  h <- h_f(hum,rain)
  deltaE = 0.1
  R0 <- (f*(a*deltaa)*probla*(h*dE/(h*dE+deltaE)))^(1/3)
  # R0 <- (f*(a*deltaa)*probla*(h/(h+deltaE)))^(1/3)
  return(R0)
}

# R0 with hacthing rate
vec <- seq(0,20,0.01)
te_cte <- 20
hum_cte = 500 
out <- sapply(vec,R0_func_alb,Te=te_cte, hum=hum_cte)

df_out <- data.frame(vec, out)
ggplot(df_out) +
  geom_line(aes(vec,out))

# Data table with tmed and R0 computed for albopictus 
R0_df <- readRDS("~/INVASIBILITY_THRESHOLD/output/R0/R0_ERA5_hourly_mcera_2020.Rds")

# Group by day
df_group <- function(df){
  df <- setDT(df)
  df$date <- as.Date(df$obs_time)
  df <- df[, .(tmean = mean(temperature),
               tmin = min(temperature),
               tmax = max(temperature),
                               sumprec = sum(prec),
                               R0_mean = mean(R0),
                               pop = min(pop)), 
                           by=list(date, NATCODE)]
  
  df[, R0_mean2 := mapply(R0_func_alb, sumprec, pop, tmean)]
  df[, R0_min := mapply(R0_func_alb, sumprec, pop, tmin)]
  df[, R0_max := mapply(R0_func_alb, sumprec, pop, tmax)]
  return(df)
} 

R0_df_daily <- lapply(R0_df,df_group)
rm(R0_df)
R0_df_daily <- as.data.frame(do.call(rbind, R0_df_daily))

# Plot the comparison between the hourly computed R0 versus daily for a muni:
nat_code <- which(esp_can$name == "Barcelona")
df_muni <- R0_df_daily[[nat_code]][,c("date","R0_mean", "R0_mean2")]
colnames(df_muni) <- c("date", "Hourly", "Daily")
df_muni <- reshape2::melt(df_muni, id.vars = "date")

library(RColorBrewer)
cols = brewer.pal(n = 3, name = "Dark2")

ggplot(df_muni) + 
  geom_line(aes(date,value, color =variable, group =variable)) +
  scale_color_manual(values = cols[1:2], name ="") +
  ylab("VSI") + xlab("Date")  +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  theme_bw() + 
  theme(text = element_text(size = 15),legend.position = c(0.9, 0.85)) 
rm(df_muni)

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

R0_df_daily <- R0_df_daily[,c("NATCODE","R0_mean","date",
                              "tmean","pop", "R0_mean2",
                              "R0_min","R0_max", "tmin", "tmax")]
R0_df_daily$month <- lubridate::month(R0_df_daily$date)
R0_df_daily <- setDT(R0_df_daily)

# Agrupate by month
R0_df_daily_m <- R0_df_daily[, .(R0_tmed=mean(R0_mean),
                                 R0_tmed2=mean(R0_mean2),
                                 tmean = mean(tmean),
                                 pop = min(pop)), 
                         by=list(NATCODE,month)]


# Plot the histogram of R0
ggplot(R0_df_daily_m[which(R0_df_daily_m$month == 1),], aes(R0_tmed)) +
  geom_histogram(color="black", fill="lightblue") 

# Plot R0
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
R0_df_daily_m <- esp_can %>% left_join(R0_df_daily_m)
R0_df_daily_m <- setDT(R0_df_daily_m)

# Remove NA from R0 with municipilities
ind <- is.na(R0_df_daily_m$R0_tmed)
R0_cpro <- R0_df_daily_m[which(R0_df_daily_m$cpro %in%
                                 R0_df_daily_m$cpro[ind] & 
                                 is.na(R0_df_daily_m$R0_tmed) == FALSE),] %>%
  group_by(cpro,month) %>%
  summarise(R0_tmed1 = mean(R0_tmed))

R0_df_daily_m  <- R0_df_daily_m %>% left_join(R0_cpro)
R0_df_daily_m  <- R0_df_daily_m[,c("NATCODE", "month", "R0_tmed",
                                   "tmean", "pop", "R0_tmed1")]
rm(R0_cpro)
R0_df_daily_m$R0_tmed <- ifelse(is.na(R0_df_daily_m$R0_tmed),
                                R0_df_daily_m$R0_tmed1,
                                R0_df_daily_m$R0_tmed)
R0_df_daily_m <- esp_can %>% left_join(R0_df_daily_m)

# See the map for R0
ggplot(R0_df_daily_m) +
  geom_sf(aes(fill = R0_tmed), linewidth = 0.01) +
  scale_fill_viridis_c()

# Create ranges:
dengue_df_g_m$group_R0 <- ifelse(dengue_df_g_m$R0_tmed<1, 0,
                                 ifelse(dengue_df_g_m$R0_tmed<25 & dengue_df_g_m$R0_tmed>=1, 1,
                                        ifelse(dengue_df_g_m$R0_tmed<50 & dengue_df_g_m$R0_tmed>=25, 2,
                                               ifelse(dengue_df_g_m$R0_tmed<100 & dengue_df_g_m$R0_tmed>=50, 3,
                                                      ifelse(dengue_df_g_m$R0_tmed<200 & dengue_df_g_m$R0_tmed>=100, 4,
                                                             ifelse(dengue_df_g_m$R0_tmed<=400 & dengue_df_g_m$R0_tmed>=200,5,6))))))
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
#add abbreviated month name
dengue_df_g_m$month_name <- mymonths[ dengue_df_g_m$month ]

#-----------------FINAL MAPS--------------------#
library(gganimate)
library(RColorBrewer)
cols = brewer.pal(n = 7, name = "YlOrBr")
rm(dengue_df)

# # Create plots:
ggplot(R0_df_daily_m) +
  geom_sf(aes(fill = R0_tmed), linewidth = 0.001) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_viridis_c(option = "magma", name = "SVI") +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month))

R0_df_daily_m$group_pop <- ifelse(R0_df_daily_m$R0_tmed<1, 0,
                                 ifelse(R0_df_daily_m$R0_tmed<25 & R0_df_daily_m$R0_tmed>=1, 1,
                                        ifelse(R0_df_daily_m$R0_tmed<50 & R0_df_daily_m$R0_tmed>=25, 2,
                                               ifelse(R0_df_daily_m$R0_tmed<100 & R0_df_daily_m$R0_tmed>=50, 3,
                                                      ifelse(R0_df_daily_m$R0_tmed<200 & R0_df_daily_m$R0_tmed>=100, 4,
                                                             ifelse(R0_df_daily_m$R0_tmed<=400 & R0_df_daily_m$R0_tmed>=200,5,6))))))

df_pop <- R0_df_daily_m[which(R0_df_daily_m$month == 1),"pop"]
df_pop$geometry <- NULL
hist(df_pop$pop)

cols = brewer.pal(n = 7, name = "YlOrBr")
ggplot(R0_df_daily_m[which(R0_df_daily_m$month == 1),]) +
  geom_sf(aes(fill = as.factor(group_pop)), linewidth = 0.001) +
  scale_fill_manual(values = cols,
                    labels = c("<1", "[1,25)", "[25,50)",
                               "[50,100)", "[100,200)", "[200,400]", ">400")) +
  theme_bw()

ggplot(R0_df_daily_m[which(R0_df_daily_m$month == 4),]) +
  geom_sf(aes(fill = tmean), linewidth = 0.001) +
  scale_fill_viridis_c(option="magma") +
  theme_bw()

ggplot(R0_df_daily_m[which(R0_df_daily_m$month == 4),]) +
  geom_sf(aes(fill = sumprec), linewidth = 0.001) +
  scale_fill_viridis_c(option="magma") +
  theme_bw()
