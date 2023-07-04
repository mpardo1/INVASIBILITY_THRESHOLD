rm(list=ls())
library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)
library("viridis")
library("gganimate")

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

Lin_func <- function(cte, cte1, temp){
  outp <- temp*cte + cte1
  if(outp < 0 | is.na(outp)){
    outp <- 0.00001
  }
  return(outp)
}


Quad <- function(cte, cte1,cte2, temp){
  outp <- cte*temp^2 + cte1*temp + cte2
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

#### -------------------------- Albopictus ------------------------- ####
## Thermal responses Aedes Albopictus from Mordecai 2017:
a_f_alb <- function(temp){Briere_func(0.000193,10.25,38.32,temp)} # Biting rate
TFD_f_alb <- function(temp){Briere_func(0.0488,8.02,35.65,temp)} # Fecundity
pLA_f_alb <- function(temp){Quad_func(0.002663,6.668,38.92,temp)} # Survival probability Egg-Adult
MDR_f_alb <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_f_alb <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span
dE_f_alb <- function(temp){Briere_func(0.00006881,8.869,35.09,temp)} # Adult life span

# R0 function by temperature:
R0_func_alb <- function(Te, rain, hum){
  a <- a_f_alb(Te)
  f <- (1/2)*TFD_f_alb(Te)
  deltaa <- lf_f_alb(Te)
  dE <- dE_f_alb(Te)
  probla <- pLA_f_alb(Te)
  h <- h_f(hum,rain)
  deltaE = 0.1
  #R0 <- ((0.3365391*f*a*deltaa)*probla*(h*dE/(h*dE+deltaE)))^(1/3)
  R0 <- ((f*a*deltaa)*probla*(h*dE/(h*dE+deltaE)))^(1/3)
  return(R0)
}

####------------------------------Aegypti------------------------####
a_f_aeg <- function(temp){Briere_func(0.000202,13.35,40.08,temp)} # Biting rate
EFD_f_aeg <- function(temp){Briere_func(0.00856,14.58,34.61,temp)} # Fecundity
pLA_f_aeg <- function(temp){Quad_func(0.004186,9.373,40.26,temp)} # Survival probability Egg-Adult
MDR_f_aeg <- function(temp){Briere_func(0.0000786,11.36,39.17,temp)} # Mosquito Development Rate
lf_f_aeg <- function(temp){Quad_func(0.148,9.16,37.73,temp)} # Adult life span
dE_f_aeg <- function(temp){Briere_func(0.0003775 ,14.88,37.42,temp)} # Adult life span

# R0 function by temperature:
R0_func_aeg <- function(Te, rain,hum){
  a <- a_f_aeg(Te)
  f <- 40#(1/2)*EFD_f_aeg(Te)
  deltaa <- lf_f_aeg(Te)
  dE <- dE_f_aeg(Te)
  probla <- pLA_f_aeg(Te)
  h <- h_f(hum,rain)
  deltE = 0.1
  R0 <- ((f*a*deltaa)*probla*(h*dE/(h*dE+deltE)))^(1/3)
  return(R0)
}


#####----------------Japonicus-----------------####
dE_f_jap <- function(temp){Briere_func(0.0002859,6.360,35.53 ,temp)} # Mosquito Development Rate
dL_f_jap <- function(temp){Briere_func(7.000e-05,9.705e+00,3.410e+01,temp)} # Survival probability Egg-Adult
lf_f_jap <- function(temp){Lin_func(-2.5045,82.6525,temp)} # Adult life span
deltaL_f_jap <- function(temp){QuadN_func(0.0021476,-0.0806067 ,1.0332455,temp)} # Adult life span

# R0 function by temperature:
R0_func_jap <- function(Te, rain,hum){
  a <- 0.35
  f <- 40 #183/2
  lf <- lf_f_jap(Te)
  deltaL <- deltaL_f_jap(Te)
  deltE = 0.1
  dE <- dE_f_jap(Te)
  dL <- dL_f_jap(Te)
  h <- h_f(hum,rain)
  if(dL == 0 | f == 0 | a == 0 | dE == 0 |  Te<0){
    R0 <- 0
  }else{
    R0 <- ((f*a*lf)*(dL/(dL+deltaL))*(h*dE/(h*dE+deltE)))^(1/3)
  }
  return(R0)
}

#----------------------------------------------------------------------#
## Read the data for the R0 computed daily:
year = 2014
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/R0/R0_ERA5_daily_mcera_",year,".Rds")
df_group <- readRDS(Path)
#-----------------------------Create plots------------------------#
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_group$month <- lubridate::month(df_group$date)

### -------------Temporal plots-----------------#
# Temporal R0 for Barcelona
NATCODE_BCN <- esp_can$NATCODE[which(esp_can$name == "Barcelona")]
df_BCN <- df_group[which(df_group$NATCODE == NATCODE_BCN),
                   c("NATCODE","date", "R0_hourly_alb",
                     "R0_hourly_aeg","R0_hourly_jap"
                     )]
colnames(df_BCN) <- c("NATCODE","date", "SVI Albopictus",
                      "SVI Aegypti","SVI Japonicus")

df_BCN <- reshape2::melt(df_BCN, id.vars = c("NATCODE", "date"))
df_BCN <- esp_can %>%
  left_join(df_BCN)
df_BCN <- df_BCN[which(df_BCN$name =="Barcelona"),]

ggplot(df_BCN) +
  geom_line(aes(date,value, color = variable)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  ylab("Vector Suitability Indes (VSI)") +
  xlab("Date") + ggtitle("Barcelona 2020") +
  theme_bw()

rm(df_BCN)

### -------------Temporal plots-----------------#
# Plot R0
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_group <- esp_can %>% left_join(df_group)
df_group <- setDT(df_group)

# Remove NA from R0 with municipilities
ind <- is.na(df_group$R0_hourly_alb)
R0_cpro <- df_group[which(df_group$cpro %in%
                            df_group$cpro[ind] & 
                                 is.na(df_group$R0_hourly_alb) == FALSE),] %>%
  group_by(cpro,month) %>%
  summarise(R0_tmed_alb = mean(R0_hourly_alb),
            R0_tmed_aeg = mean(R0_hourly_aeg),
            R0_tmed_jap = mean(R0_hourly_jap),
            R0d_tmed_alb = mean(R0_dai_alb),
            R0d_tmed_aeg = mean(R0_dai_aeg),
            R0d_tmed_jap = mean(R0_dai_jap))

df_group  <- df_group %>% left_join(R0_cpro)
df_group  <- df_group[,c("NATCODE","cpro", "month", "temp", "min_temp",
                         "max_temp","prec",  "pop", "R0_tmed_alb",
                         "R0_tmed_aeg", "R0_tmed_jap", "R0_hourly_alb",
                         "R0_hourly_aeg", "R0_hourly_jap", "R0_dai_alb",
                         "R0_dai_aeg", "R0_dai_jap")]
rm(R0_cpro)

## Delete NA in the map for hourly R0
df_group$R0_hourly_alb <- ifelse(is.na(df_group$R0_hourly_alb),
                           df_group$R0_tmed_alb,
                           df_group$R0_hourly_alb)
df_group$R0_hourly_aeg <- ifelse(is.na(df_group$R0_hourly_aeg),
                                 df_group$R0_tmed_aeg,
                                 df_group$R0_hourly_aeg)
df_group$R0_hourly_jap <- ifelse(is.na(df_group$R0_hourly_jap),
                                 df_group$R0_tmed_jap,
                                 df_group$R0_hourly_jap)

## Delete NA in the map for daily R0
ind <- is.na(df_group$R0_dai_alb)
R0_cpro <- df_group[which(df_group$cpro %in%
                            df_group$cpro[ind] & 
                            is.na(df_group$R0_dai_alb) == FALSE),] %>%
  group_by(cpro,month) %>%
  summarise(R0_tmed_alb = mean(R0_hourly_alb),
            R0_tmed_aeg = mean(R0_hourly_aeg),
            R0_tmed_jap = mean(R0_hourly_jap),
            R0d_tmed_alb = mean(R0_dai_alb),
            R0d_tmed_aeg = mean(R0_dai_aeg),
            R0d_tmed_jap = mean(R0_dai_jap))

df_group  <- df_group %>% left_join(R0_cpro)
df_group  <- df_group[,c("NATCODE", "month", "temp", "min_temp",
                         "max_temp","prec",  "pop", "R0_tmed_alb",
                         "R0_tmed_aeg", "R0_tmed_jap", "R0_hourly_alb",
                         "R0_hourly_aeg", "R0_hourly_jap", "R0_dai_alb",
                         "R0_dai_aeg", "R0_dai_jap")]
rm(R0_cpro)
df_group$R0_dai_alb <- ifelse(is.na(df_group$R0_dai_alb),
                                 df_group$R0_tmed_alb,
                                 df_group$R0_dai_alb)
df_group$R0_dai_aeg <- ifelse(is.na(df_group$R0_dai_aeg),
                                 df_group$R0_tmed_aeg,
                                 df_group$R0_dai_aeg)
df_group$R0_dai_jap <- ifelse(is.na(df_group$R0_dai_jap),
                                 df_group$R0_tmed_jap,
                                 df_group$R0_dai_jap)

df_group_na <- df_group[which(is.na(df_group$R0_hourly_jap)),]

## Whole map moving monthly
df_group_mon <- df_group[, .(temp = mean(temp),
                             prec = sum(prec), 
                             pop = min(pop),
                             R0_monthly_alb = mean(R0_hourly_alb),
                             R0_monthly_aeg = mean(R0_hourly_aeg),
                             R0_monthly_jap = mean(R0_hourly_jap),
                             R0_monthlyd_alb = mean(R0_dai_alb),
                             R0_monthlyd_aeg = mean(R0_dai_aeg),
                             R0_monthlyd_jap = mean(R0_dai_jap)), 
                         by=list(NATCODE,month)]

df_group_mon[, R0_mon_alb := mapply(R0_func_alb, temp, prec, pop)]
df_group_mon[, R0_mon_aeg := mapply(R0_func_aeg, temp, prec, pop)]
df_group_mon[, R0_mon_jap := mapply(R0_func_jap, temp, prec, pop)]

df_group_mon <- esp_can %>%
  left_join(df_group_mon)

rm(df_group)
### Difference between computing R0 daily monthly or hourly in the montly results:
df_group_mon$diff_hd <- abs(df_group_mon$R0_monthly_alb - df_group_mon$R0_monthlyd_alb)
df_group_mon$diff_dm <- abs(df_group_mon$R0_monthlyd_alb - df_group_mon$R0_mon_alb)
hist(df_group_mon$diff_dm)
hist(df_group_mon$diff_hd)

## Animate maps:
ggplot(df_group_mon) +
  geom_sf(aes(fill = R0_monthly_alb), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month))

ggplot(df_group_mon) +
  geom_sf(aes(fill = R0_monthly_aeg), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month))

ggplot(df_group_mon) +
  geom_sf(aes(fill = R0_monthly_jap), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(as.factor(month))

# Maps number of months that > 1
df_group_mon$bool_R0_alb <- ifelse(df_group_mon$R0_monthly_alb < 1,0,1)
df_group_mon$bool_R0_aeg <- ifelse(df_group_mon$R0_monthly_aeg < 1,0,1)
df_group_mon$bool_R0_jap <- ifelse(df_group_mon$R0_monthly_jap < 1,0,1)

df_group_mon$bool_m_R0_alb <- ifelse(df_group_mon$R0_mon_alb < 1,0,1)
df_group_mon$bool_m_R0_aeg <- ifelse(df_group_mon$R0_mon_aeg < 1,0,1)
df_group_mon$bool_m_R0_jap <- ifelse(df_group_mon$R0_mon_jap < 1,0,1)

df_group_mon$bool_d_R0_alb <- ifelse(df_group_mon$R0_monthlyd_alb < 1,0,1)
df_group_mon$bool_d_R0_aeg <- ifelse(df_group_mon$R0_monthlyd_aeg < 1,0,1)
df_group_mon$bool_d_R0_jap <- ifelse(df_group_mon$R0_monthlyd_jap < 1,0,1)

df_group_y <- df_group_mon %>% group_by(NATCODE) %>%
  summarise(R0_sum_alb = sum(bool_R0_alb),
            R0_sum_aeg = sum(bool_R0_aeg),
            R0_sum_jap = sum(bool_R0_jap),
            R0_sum_d_alb = sum(bool_d_R0_alb),
            R0_sum_d_aeg = sum(bool_d_R0_aeg),
            R0_sum_d_jap = sum(bool_d_R0_jap),
            R0_sum_m_alb = sum(bool_m_R0_alb),
            R0_sum_m_aeg = sum(bool_m_R0_aeg),
            R0_sum_m_jap = sum(bool_m_R0_jap),
            R0_avg_alb = mean(R0_monthly_alb),
            R0_avg_aeg = mean(R0_monthly_aeg),
            R0_avg_jap = mean(R0_monthly_jap),
            avg_temp = mean(temp),
            sum_prec = sum(prec), 
            pop = mean(pop))

df_sum <- df_group_mon[which(df_group_mon$month >3 &
                     df_group_mon$month <11 ),]
df_group_sum <- df_sum %>% group_by(NATCODE) %>%
  summarise(R0_sum_alb = sum(bool_R0_alb),
            R0_sum_aeg = sum(bool_R0_aeg),
            R0_sum_jap = sum(bool_R0_jap),
            R0_avg_alb = mean(R0_monthly_alb),
            R0_avg_aeg = mean(R0_monthly_aeg),
            R0_avg_jap = mean(R0_monthly_jap),
            avg_temp = mean(temp),
            sum_prec = sum(prec),
            pop = mean(pop))

rm(df_group_na,df_BCN,df_sum)
# Whole map group by number of months suitable
library(RColorBrewer)
library(ggpubr)

plot_summonths <- function(df){
  num_colors <- 13
  # Create a palette function using colorRampPalette
  my_palette <- colorRampPalette(c("#faf0ca","#f95738", "#732c2c"))
  
  colors <- c("#43A2CA", "#7BCCC4", "#BAE4BC", "#F0F9E8",
              "#FFF7EC","#FEE8C8","#FDD49E","#FDBB84",
              "#FC8D59","#EF6548","#D7301F", "#B30000",
              "#7F0000") 
  ggplot(df) +
    geom_sf(aes(fill = as.factor(R0)), colour = NA) +
    geom_sf(data = can_box) + coord_sf(datum = NA) +
    scale_fill_manual(values = colors,
                      name = "Nº months\n suitable") +
    theme_bw() 
}

## Computed with hourly data
df_group_y$R0 <- df_group_y$R0_sum_alb
plot_sum_alb <- plot_summonths(df_group_y)
plot_sum_alb

Path <- "~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/AlboSum2014.pdf"
dev.copy2pdf(file=Path, width = 7, height = 5)

df_group_y$R0 <- df_group_y$R0_sum_aeg
plot_sum_aeg <- plot_summonths(df_group_y)
plot_sum_aeg
Path <- "~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/AegSum2014.pdf"
dev.copy2pdf(file=Path, width = 7, height = 5)

df_group_y$R0 <- df_group_y$R0_sum_jap
plot_sum_jap <- plot_summonths(df_group_y)
plot_sum_jap
Path <- "~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/JapSum2014.pdf"
dev.copy2pdf(file=Path, width = 7, height = 5)

ggarrange(plot_sum_alb + ggtitle("Aedes Albopictus"),
          plot_sum_aeg + ggtitle("Aedes Aegypti"),
          plot_sum_jap + ggtitle("Aedes Japonicus"),
          common.legend = TRUE)

### PLOOOTS

## Take out the NA values as the average of the province
str(df_group_y)
esp_can_cpro <-setDT(esp_can[,c("NATCODE", "cpro")])
esp_can_cpro$geometry <- NULL
df_group_y <- df_group_y %>% left_join(esp_can_cpro)
df_group_cpro <- df_group_y[which(is.na(df_group_y$R0_sum_m_alb)==FALSE),] %>% 
  group_by(cpro) %>%
  summarise(R0_aeg =mean(R0_sum_m_aeg),
            R0_alb = mean(R0_sum_m_alb),
            R0_jap = mean(R0_sum_m_jap))

df_group_cpro$geometry <- NULL
df_group_y <- df_group_y %>% left_join(df_group_cpro)
df_group_y$R0_sum_m_alb <- ifelse(is.na(df_group_y$R0_sum_m_alb ),
                                  df_group_y$R0_alb,
                                  df_group_y$R0_sum_m_alb )
df_group_y$R0_sum_m_aeg <- ifelse(is.na(df_group_y$R0_sum_m_aeg ),
                                  df_group_y$R0_aeg,
                                  df_group_y$R0_sum_m_aeg )
df_group_y$R0_sum_m_jap <- ifelse(is.na(df_group_y$R0_sum_m_jap ),
                                  df_group_y$R0_jap,
                                  df_group_y$R0_sum_m_jap )

df_group_y$R0 <- ceiling(df_group_y$R0_sum_m_alb)
plot_sum_alb <- plot_summonths(df_group_y)
plot_sum_alb
Path <- "~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/mon_AlboSum2014.pdf"
dev.copy2pdf(file=Path, width = 7, height = 5)

df_group_y$R0 <- ceiling(df_group_y$R0_sum_m_aeg)
plot_sum_aeg <- plot_summonths(df_group_y)
plot_sum_aeg
Path <- "~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/mon_AegSum2014.pdf"
dev.copy2pdf(file=Path, width = 7, height = 5)

df_group_y$R0 <- ceiling(df_group_y$R0_sum_m_jap)
plot_sum_jap <- plot_summonths(df_group_y)
plot_sum_jap
Path <- "~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/mon_JapSum2014.pdf"
dev.copy2pdf(file=Path, width = 7, height = 5)

ggarrange(plot_sum_alb + ggtitle("Aedes Albopictus"),
          plot_sum_aeg + ggtitle("Aedes Aegypti"),
          plot_sum_jap + ggtitle("Aedes Japonicus"),
          common.legend = TRUE)

##-------------------- AVG maps -----------------------
plot_avg_aeg <- ggplot(df_group_sum) +
  geom_sf(aes(fill = R0_avg_aeg), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral",
                       name = "Annual R0") +
  theme_bw()
plot_avg_aeg

plot_avg_jap <- ggplot(df_group_sum) +
  geom_sf(aes(fill = R0_avg_jap), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral",
                       name = "Annual R0") +
  theme_bw()
plot_avg_jap

plot_avg_alb <- ggplot(df_group_sum) +
  geom_sf(aes(fill = R0_avg_alb), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral",
                       name = "Annual R0") +
  theme_bw()

plot_avg_alb

ggarrange(plot_avg_alb + ggtitle("Aedes Albopictus"),
          plot_avg_aeg + ggtitle("Aedes Aegypti"),
          plot_avg_jap + ggtitle("Aedes Japonicus"),
          common.legend = TRUE)

### Plot of average temperature and rainfall
df_group_y[,]
plot_temp <- ggplot(df_group_y) +
  geom_sf(aes(fill = avg_temp), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral", name = "Mean Temp") +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void()

plot_rain <- ggplot(df_group_y) +
  geom_sf(aes(fill = sum_prec), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral", name = "Sum prec") +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void()

ggarrange(plot_temp,plot_rain)

###----------------- VALIDATION -----------------------#
## MAp Spain -------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
can_box <- esp_get_can_box()

# Presence absence data Albopictus Spain:
Path = "~/INVASIBILITY_THRESHOLD/data/PA/PresenceAbsence_MA_BG2.Rds"
df_pa <- readRDS(Path)
df_pa <- df_pa %>% group_by(NATCODE) %>%
  summarise(sum = sum(A_PRIM_DET_OFICIAL),
            sum_MA = sum(A_PRIM_DET_CITSCI))
df_pa$PA <- ifelse(df_pa$sum == 0 & df_pa$sum_MA == 0,0,1 )
df_pa <- esp_can %>% left_join(df_pa)
df_pa[which(is.na(df_pa$PA)),"PA"] <- 0

ggplot(df_pa) +
  geom_sf(aes(fill = as.factor(PA)), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_manual(values = c("#713E5A","#63A375"), name = "PA") +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void()

### Comparison between presence absence and number of months R0>1
df_pa <- df_pa[, c("NATCODE", "PA")]
df_pa$geometry <- NULL

df_group_m <- df_group_y[,c("NATCODE","R0_avg_alb", "R0_sum_alb")]
df_pa <- df_pa %>% left_join(df_group_m)

NATCODE_CAT <- esp_can[which(esp_can$ine.ccaa.name == "Cataluña"),"NATCODE"]
NATCODE_CAT$geometry <- NULL
df_pa_CAT <- df_pa[which(as.numeric(df_pa$NATCODE) %in% 
                           as.numeric(NATCODE_CAT$NATCODE)),]

hist(df_pa_CAT[which(df_pa_CAT$PA == 1), "R0_avg_alb"])
ggplot(df_pa_CAT[which(df_pa_CAT$PA == 1),]) +
  geom_histogram(aes(R0_avg_alb), binwidth = 0.2,
                 fill =  "#E1CE7A") + xlab("Annual average suitability") +
  theme_bw()

ggplot(df_pa_CAT[which(df_pa_CAT$PA == 1),]) +
  geom_histogram(aes(R0_sum_alb), binwidth = 0.8,
                 fill =  "#E1CE7A") + xlab("Number of months suitable") +
  theme_bw()

ggplot(df_pa_CAT) +
  geom_histogram(aes(R0_sum_alb, fill = as.factor(PA)), binwidth = 0.5) + 
  xlab("Number of months suitable") +
  theme_bw()

ggplot(df_pa_CAT) +
  geom_histogram(aes(R0_avg_alb, fill= as.factor(PA)), binwidth = 0.5)
#-------------------------------P/(P+A)----------------------------------------#
### Range Annual average R0
df_range <- function(df_pa_CAT){
  df_pa_CAT$range <- ifelse(df_pa_CAT$R0_avg_alb  < 1, "<1",
                            ifelse(df_pa_CAT$R0_avg_alb  >= 1 & df_pa_CAT$R0_avg_alb  < 2, "[1,2)",
                                   ifelse(df_pa_CAT$R0_avg_alb  >= 2 & df_pa_CAT$R0_avg_alb  < 3, "[2,3)",
                                          ifelse(df_pa_CAT$R0_avg_alb  >= 3 & df_pa_CAT$R0_avg_alb  < 4, "[3,4)",
                                                 ifelse(df_pa_CAT$R0_avg_alb  >= 4 & df_pa_CAT$R0_avg_alb  < 5, "[4,5)",
                                                        ifelse(df_pa_CAT$R0_avg_alb  >= 5 & df_pa_CAT$R0_avg_alb  < 6, "[5,6]",">6") )))))
  df_r_1 <- df_pa_CAT[which(df_pa_CAT$PA == 1),] %>% 
    group_by(range) %>% summarize(num_1 = n())
  df_r_0 <- df_pa_CAT[which(df_pa_CAT$PA == 0),] %>% 
    group_by(range) %>% summarize(num_0 = n())
  
  df_r <- df_r_1 %>% left_join(df_r_0)
  df_r$prop_1 <- df_r$num_1/(df_r$num_1+df_r$num_0)
  return(df_r)

}

###### Sum number of months ###
df_num_months <- function(df_pa_CAT){
  df_r_1 <- df_pa_CAT[which(df_pa_CAT$PA == 1),] %>% 
    group_by(R0_sum_alb) %>% summarize(num_1 = n())
  df_r_0 <- df_pa_CAT[which(df_pa_CAT$PA == 0),] %>% 
    group_by(R0_sum_alb) %>% summarize(num_0 = n())
  
  df_r <- df_r_1 %>% left_join(df_r_0)
  df_r$prop_1 <- df_r$num_1/(df_r$num_1+df_r$num_0)
  return(df_r)

}

## Compute the plots for cataluña
df_pa <- df_pa[, c("NATCODE", "PA")]
df_pa$geometry <- NULL

df_group_m <- df_group_y[,c("NATCODE","R0_avg_alb", "R0_sum_alb")]
df_pa <- df_pa %>% left_join(df_group_m)

NATCODE_CAT <- esp_can[which(esp_can$ine.ccaa.name == "Cataluña"),"NATCODE"]
NATCODE_CAT$geometry <- NULL
df_pa_CAT <- df_pa[which(as.numeric(df_pa$NATCODE) %in% 
                           as.numeric(NATCODE_CAT$NATCODE)),]

df_r_CAT <- df_range(df_pa_CAT)
df_r_CAT$ccaa <- "Catalunya"
df_sum_CAT <- df_num_months(df_pa_CAT)
df_sum_CAT$ccaa <- "Catalunya"

## Compute the plots for Andalucoa
df_pa <- df_pa[, c("NATCODE", "PA")]
df_pa$geometry <- NULL

df_group_m <- df_group_y[,c("NATCODE","R0_avg_alb", "R0_sum_alb")]
df_pa <- df_pa %>% left_join(df_group_m)

NATCODE_AND <- esp_can[which(esp_can$ine.ccaa.name == "Andalucía"),"NATCODE"]
NATCODE_AND$geometry <- NULL
df_pa_AND <- df_pa[which(as.numeric(df_pa$NATCODE) %in%
                           as.numeric(NATCODE_AND$NATCODE)),]

df_r_AND <- df_range(df_pa_AND)
df_r_AND$ccaa <- "Andalucía"
df_sum_AND <- df_num_months(df_pa_AND)
df_sum_AND$ccaa <- "Andalucía"

## Compute the plots for Valencia
df_pa <- df_pa[, c("NATCODE", "PA")]
df_pa$geometry <- NULL

df_group_m <- df_group_y[,c("NATCODE","R0_avg_alb", "R0_sum_alb")]
df_pa <- df_pa %>% left_join(df_group_m)

NATCODE_VAL <- esp_can[which(esp_can$codauto == "10"),"NATCODE"]
NATCODE_VAL$geometry <- NULL
df_pa_VAL <- df_pa[which(as.numeric(df_pa$NATCODE) %in%
                           as.numeric(NATCODE_VAL$NATCODE)),]

df_r_VAL <- df_range(df_pa_VAL)
df_sum_VAL <- df_num_months(df_pa_VAL)
df_r_VAL$ccaa <- "Comunitat Valenciana"
df_sum_VAL$ccaa <- "Comunitat Valenciana"

## Compute the plots for Pais Vasco
df_pa <- df_pa[, c("NATCODE", "PA")]
df_pa$geometry <- NULL

df_group_m <- df_group_y[,c("NATCODE","R0_avg_alb", "R0_sum_alb")]
df_pa <- df_pa %>% left_join(df_group_m)

NATCODE_EUS <- esp_can[which(esp_can$codauto == "16"),"NATCODE"]
NATCODE_EUS$geometry <- NULL
df_pa_EUS <- df_pa[which(as.numeric(df_pa$NATCODE) %in%
                           as.numeric(NATCODE_EUS$NATCODE)),]

df_r_EUS <- df_range(df_pa_EUS)
df_sum_EUS <- df_num_months(df_pa_EUS)
df_r_EUS$ccaa <- "Euskadi"
df_sum_EUS$ccaa <- "Euskadi"

# df_ccaa <- esp_can[,c("codauto", "ine.ccaa.name")]
# df_ccaa$geometry <- NULL
# unique(df_ccaa)

## Join all the data frames
df_r <- rbind(df_r_VAL, df_r_CAT, df_r_AND, df_r_EUS)
df_sum <- rbind(df_sum_VAL, df_sum_CAT, df_sum_AND, df_sum_EUS)

gg_range <- ggplot(df_r) + 
  geom_point(aes(x=factor(range, 
                          level=c("<1","[1,2)","[2,3)","[3,4)",
                                  "[4,5)","[5,6)",">6")), prop_1, color = ccaa)) +
  xlab("Annual Average R0") + 
  ylab("Proportion of presence") + theme_bw()

gg_sum <- ggplot(df_sum) + 
  geom_point(aes(R0_sum_alb, prop_1, color =ccaa)) +
  xlab("Sum of months with R0>1") + 
  ylab("Proportion of sum") + theme_bw()

ggarrange(gg_range, gg_sum, common.legend = TRUE)

#---------------------P/A-------------------------------------------#
### Range Annual average R0
df_range <- function(df_pa_CAT){
  df_pa_CAT$range <- ifelse(df_pa_CAT$R0_avg_alb  < 1, "<1",
                            ifelse(df_pa_CAT$R0_avg_alb  >= 1 & df_pa_CAT$R0_avg_alb  < 2, "[1,2)",
                                   ifelse(df_pa_CAT$R0_avg_alb  >= 2 & df_pa_CAT$R0_avg_alb  < 3, "[2,3)",
                                          ifelse(df_pa_CAT$R0_avg_alb  >= 3 & df_pa_CAT$R0_avg_alb  < 4, "[3,4)",
                                                 ifelse(df_pa_CAT$R0_avg_alb  >= 4 & df_pa_CAT$R0_avg_alb  < 5, "[4,5)",
                                                        ifelse(df_pa_CAT$R0_avg_alb  >= 5 & df_pa_CAT$R0_avg_alb  < 6, "[5,6]",">6") )))))
  df_r_1 <- df_pa_CAT[which(df_pa_CAT$PA == 1),] %>% 
    group_by(range) %>% summarize(num_1 = n())
  df_r_0 <- df_pa_CAT[which(df_pa_CAT$PA == 0),] %>% 
    group_by(range) %>% summarize(num_0 = n())
  
  df_r <- df_r_1 %>% left_join(df_r_0)
  df_r$prop_1 <- df_r$num_1/df_r$num_0
  return(df_r)
  
}

###### Sum number of months ###
df_num_months <- function(df_pa_CAT){
  df_r_1 <- df_pa_CAT[which(df_pa_CAT$PA == 1),] %>% 
    group_by(R0_sum_alb) %>% summarize(num_1 = n())
  df_r_0 <- df_pa_CAT[which(df_pa_CAT$PA == 0),] %>% 
    group_by(R0_sum_alb) %>% summarize(num_0 = n())
  
  df_r <- df_r_1 %>% left_join(df_r_0)
  df_r$prop_1 <- df_r$num_1/df_r$num_0
  return(df_r)
  
}

## Compute the plots for cataluña
df_pa <- df_pa[, c("NATCODE", "PA")]
df_pa$geometry <- NULL

df_group_m <- df_group_y[,c("NATCODE","R0_avg_alb", "R0_sum_alb")]
df_pa <- df_pa %>% left_join(df_group_m)

NATCODE_CAT <- esp_can[which(esp_can$ine.ccaa.name == "Cataluña"),"NATCODE"]
NATCODE_CAT$geometry <- NULL
df_pa_CAT <- df_pa[which(as.numeric(df_pa$NATCODE) %in% 
                           as.numeric(NATCODE_CAT$NATCODE)),]

df_r_CAT <- df_range(df_pa_CAT)
df_r_CAT$ccaa <- "Catalunya"
df_sum_CAT <- df_num_months(df_pa_CAT)
df_sum_CAT$ccaa <- "Catalunya"
df_sum_CAT[which(is.na(df_sum_CAT$num_0)),"num_0"] = 0
df_sum_CAT$prop_presence <- df_sum_CAT$num_1/(df_sum_CAT$num_1 +df_sum_CAT$num_0)
ggplot(df_sum_CAT) +
  geom_point(aes(R0_sum_alb,prop_presence)) + 
  scale_x_continuous(breaks = seq(0,12,1)) + 
  ylab("Proportion presence") + xlab("Number of months suitable") +
  theme_bw()
## Compute the plots for Andalucoa
df_pa <- df_pa[, c("NATCODE", "PA")]
df_pa$geometry <- NULL

df_group_m <- df_group_y[,c("NATCODE","R0_avg_alb", "R0_sum_alb")]
df_pa <- df_pa %>% left_join(df_group_m)

NATCODE_AND <- esp_can[which(esp_can$ine.ccaa.name == "Andalucía"),"NATCODE"]
NATCODE_AND$geometry <- NULL
df_pa_AND <- df_pa[which(as.numeric(df_pa$NATCODE) %in%
                           as.numeric(NATCODE_AND$NATCODE)),]

df_r_AND <- df_range(df_pa_AND)
df_r_AND$ccaa <- "Andalucía"
df_sum_AND <- df_num_months(df_pa_AND)
df_sum_AND$ccaa <- "Andalucía"
df_sum_AND[which(is.na(df_sum_AND$num_0)),"num_0"] = 0
df_sum_AND$prop_presence <- df_sum_AND$num_1/(df_sum_AND$num_1 +df_sum_AND$num_0)
ggplot(df_sum_AND) +
  geom_point(aes(R0_sum_alb,prop_presence)) + 
  scale_x_continuous(breaks = seq(5,12,1)) + 
  ylab("Proportion presence") + xlab("Number of months suitable") +
  theme_bw()

## Compute the plots for Valencia
df_pa <- df_pa[, c("NATCODE", "PA")]
df_pa$geometry <- NULL

df_group_m <- df_group_y[,c("NATCODE","R0_avg_alb", "R0_sum_alb")]
df_pa <- df_pa %>% left_join(df_group_m)

NATCODE_VAL <- esp_can[which(esp_can$codauto == "10"),"NATCODE"]
NATCODE_VAL$geometry <- NULL
df_pa_VAL <- df_pa[which(as.numeric(df_pa$NATCODE) %in%
                           as.numeric(NATCODE_VAL$NATCODE)),]

df_r_VAL <- df_range(df_pa_VAL)
df_sum_VAL <- df_num_months(df_pa_VAL)
df_sum_VAL[which(is.na(df_sum_VAL$num_0)),"num_0"] = 0
df_sum_VAL$prop_presence <- df_sum_VAL$num_1/(df_sum_VAL$num_1 +df_sum_VAL$num_0)

df_r_VAL$ccaa <- "Comunitat Valenciana"
df_sum_VAL$ccaa <- "Comunitat Valenciana"

ggplot(df_sum_VAL) +
  geom_point(aes(R0_sum_alb,prop_presence)) + 
  scale_x_continuous(breaks = seq(1,12,1)) + 
  ylab("Proportion presence") + xlab("Number of months suitable") +
  ylim(c(0,1)) +
  theme_bw()

## Compute the plots for Pais Vasco
df_pa <- df_pa[, c("NATCODE", "PA")]
df_pa$geometry <- NULL

df_group_m <- df_group_y[,c("NATCODE","R0_avg_alb", "R0_sum_alb")]
df_pa <- df_pa %>% left_join(df_group_m)

NATCODE_EUS <- esp_can[which(esp_can$codauto == "16"),"NATCODE"]
NATCODE_EUS$geometry <- NULL
df_pa_EUS <- df_pa[which(as.numeric(df_pa$NATCODE) %in%
                           as.numeric(NATCODE_EUS$NATCODE)),]

df_r_EUS <- df_range(df_pa_EUS)
df_sum_EUS <- df_num_months(df_pa_EUS)
df_r_EUS$ccaa <- "Euskadi"
df_sum_EUS$ccaa <- "Euskadi"

# df_ccaa <- esp_can[,c("codauto", "ine.ccaa.name")]
# df_ccaa$geometry <- NULL
# unique(df_ccaa)

## Join all the data frames
df_r <- rbind(df_r_VAL, df_r_CAT, df_r_AND, df_r_EUS)
df_sum <- rbind(df_sum_VAL, df_sum_CAT, df_sum_AND, df_sum_EUS)

gg_range <- ggplot(df_r) + 
  geom_point(aes(x=factor(range, 
                          level=c("<1","[1,2)","[2,3)","[3,4)",
                                  "[4,5)","[5,6)",">6")), prop_1, color = ccaa)) +
  xlab("Annual Average R0") + 
  ylab("P/A") + theme_bw()

gg_sum <- ggplot(df_sum) + 
  geom_point(aes(R0_sum_alb, prop_1, color =ccaa)) +
  xlab("Sum of months with R0>1") + 
  ylab("P/A") + theme_bw()

ggarrange(gg_range , gg_sum, common.legend = TRUE)

#-----------------------------------------------------------------------#
#### The same for Girona ####
NATCODE_GN <- esp_can[which(esp_can$ine.prov.name == "Girona"),"NATCODE"]
NATCODE_GN$geometry <- NULL
df_pa_GN <- df_pa[which(as.numeric(df_pa$NATCODE) %in% 
                           as.numeric(NATCODE_GN$NATCODE)),]

hist(df_pa_GN[which(df_pa_GN$PA == 1), "R0_avg_alb"])
ggplot(df_pa_GN[which(df_pa_GN$PA == 1),]) +
  geom_histogram(aes(R0_avg_alb), binwidth = 0.1)

ggplot(df_pa_GN) +
  geom_histogram(aes(R0_avg_alb, fill= as.factor(PA)), binwidth = 0.2)

## Comparsion with number of months suitable
plot_pa_hourly <- ggplot(df_pa) + 
  geom_point(aes(R0_sum, PA), size = 0.7) + 
  theme_bw()

plot_pa_daily <- ggplot(df_pa) + 
  geom_point(aes(R0_sum_comp, PA), size = 0.7) + 
  theme_bw()

ggarrange(plot_pa_hourly,plot_pa_daily)

## Comparsion with average R0 annual
plot_pa_hourly_avg <- ggplot(df_pa) + 
  geom_point(aes(R0_avg_alb, PA), size = 0.7) + 
  geom_vline(aes(xintercept = 1),
             linetype = "dashed", color = "red") +
  xlab("Annual average R0") + 
  theme_bw()
plot_pa_hourly_avg
plot_pa_daily_avg <- ggplot(df_pa) + 
  geom_point(aes(R0_sum_alb, PA), size = 0.7) + 
  xlab("Sum months R0>1") + 
  theme_bw()

ggarrange(plot_pa_hourly_avg,plot_pa_daily_avg)

####------------- Map PA from Albopictus and Japonicus----------------##
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
can_box <- esp_get_can_box()

# Japonicus PA data
Path <- "~/INVASIBILITY_THRESHOLD/data/PA/MUNS_ANYS-japonicus.csv"
PA_jap <- read.csv(Path)
PA_jap$A_PRIM_DET_OFICIAL <- ifelse( PA_jap$A_PRIM_DET_OFICIAL == "#N/A", 0, PA_jap$A_PRIM_DET_OFICIAL)
PA_jap$A_PRIM_DET_CITSCI <- ifelse( PA_jap$A_PRIM_DET_CITSCI == "#N/A", 0, PA_jap$A_PRIM_DET_CITSCI)

PA_jap$Japonicus <- ifelse(PA_jap$A_PRIM_DET_OFICIAL == 0 & PA_jap$A_PRIM_DET_CITSCI == 0, 0,1)
PA_jap_ESP <- esp_can %>% left_join(PA_jap)

ggplot(PA_jap_ESP) + 
  geom_sf(aes(fill = as.factor(Japonicus)), linewidth = 0.05)  +
  scale_fill_manual(values = c("#ee4266",  "#0ead69","white") , 
                    name = "") + theme_bw()

# Albopictus PA data
Path = "~/INVASIBILITY_THRESHOLD/data/PA/PresenceAbsence_MA_BG2.Rds"
PA_alb <- readRDS(Path)
PA_alb$Albopictus <- ifelse(PA_alb$A_PRIM_DET_OFICIAL == 0 & PA_alb$A_PRIM_DET_CITSCI == 0, 0,1)
PA_alb <- PA_alb[,c("NATCODE", "Albopictus")]
PA_jap <- PA_jap[,c("NATCODE", "Japonicus")]

PA <- PA_alb %>% left_join(PA_jap)
PA$esp <- ifelse(PA$Albopictus == 1 & PA$Japonicus == 0, "Albopictus",
                 ifelse(PA$Albopictus == 0 & PA$Japonicus == 1, "Japonicus", 
                        ifelse(PA$Albopictus == 1 & PA$Japonicus == 1, "Both","Not detected")   ))

PA <- esp_can %>% left_join(PA)
PA[which(is.na(PA$esp)),"esp"] <- "Not detected"
PA_esp <- ggplot(PA) + 
  geom_sf(aes(fill = as.factor(esp)), linewidth = 0.05) +
  scale_fill_manual(values = c("#ffa62b","#ee4266",  "#0ead69","white") , 
                    name = "") +
  geom_sf(data = can_box) + coord_sf(datum = NA) + theme_bw() +
  theme(legend.position = c(0.8, 0.2),
        text = element_text(size = letsize),
        legend.text=element_text(size=letsize)) 
PA_esp

legend <- cowplot::get_legend(PA_esp)

ggarrange(plot_sum_alb + ggtitle("Aedes Albopictus"),
          plot_sum_aeg + ggtitle("Aedes Aegypti"),
          plot_sum_jap + ggtitle("Aedes Japonicus"),
          PA_esp + ggtitle("Presencia/Ausencia") ,
          common.legend = TRUE)

rm(df_group,df_group_mon,df_group_y, esp_can, PA, PA_1, PA_0,df_pa)

#### -------Comparison PA Japonicus---------- ####
head(PA_jap_ESP)
NATCODE_AST <- esp_can[which(esp_can$codauto == "03"),"NATCODE"]
NATCODE_AST$geometry <- NULL
df_pa_AST <- setDT(PA_jap_ESP[which(as.numeric(PA_jap_ESP$NATCODE) %in% 
                           as.numeric(NATCODE_AST$NATCODE)),])[,c("NATCODE", "Japonicus")]

df_group_y_AST <- df_group_y[which(as.numeric(df_group_y$NATCODE) %in% 
                                     as.numeric(NATCODE_AST$NATCODE)),] %>% 
  left_join(df_pa_AST)

ggplot(df_group_y_AST) + 
  geom_point(aes(R0_sum_jap, Japonicus)) + 
  theme_bw()

ggplot(df_group_y_AST) + 
  geom_point(aes(R0_avg_jap, Japonicus)) +
  theme_bw()

ggplot(df_group_y_AST[which(df_group_y_AST$Japonicus == 1),]) +
  geom_histogram(aes(R0_avg_jap), binwidth = 0.07)

ggplot(df_group_y_AST[which(df_group_y_AST$Japonicus == 1),]) +
  geom_histogram(aes(R0_sum_jap), binwidth = 0.07)
