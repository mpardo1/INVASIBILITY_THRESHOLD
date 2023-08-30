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

# vec = seq(0,40,0.1)
# out <- unlist(lapply(vec,a_f_alb))
# max(out)
# vec = seq(0,40,0.1)
# out <- unlist(lapply(vec,TFD_f_alb))
# max(out)
# vec = seq(0,40,0.1)
# out <- unlist(lapply(vec,lf_f_alb))
# max(out)
# vec = seq(0,40,0.1)
# out <- unlist(lapply(vec,dE_f_alb))
# max(out)
# vec = seq(0,40,0.1)
# out <- unlist(lapply(vec,pLA_f_alb))
# max(out)
# vec = seq(0,10,0.1)
# out <- unlist(lapply(vec,h_f, hum=0))
# max(out)

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

# ## Dependence in human density
# Te = 16
# rain = 0.1
# vec = seq(0,2000,1)
# out <- lapply(vec, R0_func_aeg, rain = rain, Te = Te)
# plot(vec, out)
# 
# ## Dependence in rain
# Te = 16
# hum = 1
# vec = seq(0,10,0.11)
# out <- lapply(vec, R0_func_aeg, hum = hum, Te = Te)
# plot(vec, out)

#----------------------------------------------------------------------#
## Read the data for the R0 computed daily:
# year = 2022
# Path <- paste0("/home/marta/INVASIBILITY_THRESHOLD/output/mcera5/process_Daily_ERA5_daily_mcera_",year,".Rds")
year = 2004
Path <- paste0("/home/marta/INVASIBILITY_THRESHOLD/output/mcera5/process_hourly_daily_ERA5_daily_mcera_",
               year,".Rds")
# saveRDS(dt_weather,Path)
df_group <- setDT(readRDS(Path))

df_group$id <- 1
test <- df_group[,.(n = sum(id)), by = list(NATCODE)]
min(test$n)
max(test$n)
nrow(test)

#-----------------------------Create plots------------------------#
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_group$month <- lubridate::month(df_group$date)

# Population 2022:
Path <- "/home/marta/INVASIBILITY_THRESHOLD/data/pop/pobmun22.csv"
pop22 <- read.csv(Path, sep = ",")
Path <- "/home/marta/INVASIBILITY_THRESHOLD/data/pop/pobmun04.csv"
pop22 <- read.csv(Path, sep = ";")
pop22$cmun <- ifelse(pop22$CMUN<10, paste0("00",pop22$CMUN),
                     ifelse(pop22$CMUN<100, paste0("0",pop22$CMUN),as.character(pop22$CMUN)))
pop22$cpro <- ifelse(pop22$CPRO<10,
                     paste0("0",pop22$CPRO),as.character(pop22$CPRO))
pop22$POB22 <- gsub('\\.','',pop22$TOTAL)
pop22$POB22 <- as.numeric(pop22$POB22)
esp_can <- esp_can %>% left_join(pop22, by = c("cpro","cmun") )
test_pop <- esp_can[which(is.na(esp_can$POB22)),
                    c("name", "NATCODE", "POB22")]

test_pop$geometry <- NULL
nrow(test_pop)
esp_can[which(is.na(esp_can$POB22)),"POB22"] <- 0
esp_can$area <- as.numeric(st_area(esp_can))/1000000
esp_can$dens <- esp_can$POB22/esp_can$area
esp_can <- setDT(esp_can[,c("NATCODE","name", "dens")])
esp_can$geometry <- NULL
# Path <- "~/INVASIBILITY_THRESHOLD/output/pop/pop22.Rds"
# saveRDS(esp_can_pop, Path)
df_group <- df_group %>% left_join(esp_can)

# Test weather data -------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_day <- df_group[which(df_group$date == as.Date("2004-07-05")),]
df_day <- esp_can %>% left_join(df_day)
ggplot(df_day) + 
  geom_sf(aes(fill = tmean ), color = NA) +
  scale_fill_viridis_c(option = "magma") + 
  theme_bw()

hist(df_group$prec1)
rm(df_day)

#---------------------Compute R0-------------------------#
## Es el prec1 no el prec !! IMPORTANTEE!!!
df_group$prec <- as.numeric(df_group$prec1)
df_group[, R0_dai_alb := mapply(R0_func_alb, tmean, prec, dens)]
df_group[, R0_dai_aeg := mapply(R0_func_aeg, tmean, prec, dens)]
df_group[, R0_dai_jap := mapply(R0_func_jap, tmean, prec, dens)]

## ----------PLOT TEMP----------#
# df_prov <- esp_can[,c("cpro","ine.prov.name")]
# df_prov$geometry <- NULL
# df_prov <- unique(df_prov)
# provincia = "Valencia/València"
# df_GN <- df_group[which(df_group$NATCODE %in%
#                           esp_can$NATCODE[which(esp_can$ine.prov.name == provincia)]),]
# df_GN <- df_GN[,.(tmean = mean(tmean),
#                   prec = mean(prec),
#                   dens = as.numeric(sum(pop)/sum(area))), by = list(date)]
# 
# df_GN[, R0_dai_alb := mapply(R0_func_alb, tmean, prec, dens)]
# df_GN[, R0_dai_aeg := mapply(R0_func_aeg, tmean, prec, dens)]
# df_GN[, R0_dai_jap := mapply(R0_func_jap, tmean, prec, dens)]
# 
# library("latex2exp")
# ggplot(df_GN) + 
#   geom_line(aes(date, R0_dai_alb)) + 
#   ylab(TeX("$R_M$")) + 
#   scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y") +
#   ggtitle(provincia) +
#   theme_bw()
# 
## It is the rainfall why there are so many peaks and downs
# coeff <- 1
# 
# ggplot(df_BCN, aes(x=date)) +
#   geom_line( aes(y=R0_dai_alb), color = "darkblue") + 
#   geom_line( aes(y=prec), color = "yellow") + # Divide by 10 to get the same range than the temperature
#   scale_y_continuous(
#     name = "R0",
#     sec.axis = sec_axis(~.*coeff, name="Temperature")
#   ) + theme_bw()

#### ----------- Monthly agg----------------###
df_group_mon <- df_group[, .(tmean = mean(tmean),
                             tmin = min(tmean),
                             tmax = max(tmean),
                             prec = sum(prec), 
                             precmean = mean(prec), 
                             dens = min(dens),
                             dens1 = max(dens)), 
                         by=list(NATCODE,month)]

df_group_mon[, R0_mon_alb := mapply(R0_func_alb, tmean, precmean, dens)]
df_group_mon[, R0_mon_aeg := mapply(R0_func_aeg, tmean, precmean, dens)]
df_group_mon[, R0_mon_jap := mapply(R0_func_jap, tmean, precmean, dens)]
df_group_mon[, R0_mon_alb_min := mapply(R0_func_alb, tmin, precmean, dens)]
df_group_mon[, R0_mon_aeg_min := mapply(R0_func_aeg, tmin, precmean, dens)]
df_group_mon[, R0_mon_jap_min := mapply(R0_func_jap, tmin, precmean, dens)]
df_group_mon[, R0_mon_alb_max := mapply(R0_func_alb, tmax, precmean, dens)]
df_group_mon[, R0_mon_aeg_max := mapply(R0_func_aeg, tmax, precmean, dens)]
df_group_mon[, R0_mon_jap_max := mapply(R0_func_jap, tmax, precmean, dens)]

###--------------GROUP SUM MONTHS---------####
# Maps number of months that > 1
df_group_mon$bool_R0_alb <- ifelse(df_group_mon$R0_mon_alb < 1,0,1)
df_group_mon$bool_R0_aeg <- ifelse(df_group_mon$R0_mon_aeg < 1,0,1)
df_group_mon$bool_R0_jap <- ifelse(df_group_mon$R0_mon_jap < 1,0,1)
df_group_mon$bool_R0_alb_min <- ifelse(df_group_mon$R0_mon_alb_min < 1,0,1)
df_group_mon$bool_R0_aeg_min <- ifelse(df_group_mon$R0_mon_aeg_min < 1,0,1)
df_group_mon$bool_R0_jap_min <- ifelse(df_group_mon$R0_mon_jap_min < 1,0,1)
df_group_mon$bool_R0_alb_max <- ifelse(df_group_mon$R0_mon_alb_max < 1,0,1)
df_group_mon$bool_R0_aeg_max <- ifelse(df_group_mon$R0_mon_aeg_max < 1,0,1)
df_group_mon$bool_R0_jap_max <- ifelse(df_group_mon$R0_mon_jap_max < 1,0,1)

## Test:
ggplot(df_group_mon) + geom_point(aes(tmean, bool_R0_jap))
df_group_mon[which(df_group_mon$bool_R0_jap == 0 &
                     df_group_mon$tmean < 15),]

# R0_func_jap(13.7,56,0.6)
##------------------ Plots months------------------#
library(RColorBrewer)
library(ggpubr)
library("latex2exp")
plot_months <- function(df, month){
  df1 <- df[which(df$month == month),]
  # Create a palette function using colorRampPalette
  plot <- ggplot(df1) +
    geom_sf(aes(fill = R0), colour = NA) +
    geom_sf(data = can_box) + coord_sf(datum = NA) +
    scale_fill_distiller(palette = "Spectral",
                         limits = c(min(df$R0),max(df$R0)),
                         name = TeX("$R_M$")) +
    ggtitle(as.character(month)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.1,0.8))
  return(plot)
}

# Esp can shapefile --------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
## Para hacer un cuadrado con seis plots cambio el numero de 
# month y el nombre del plot y lo hago para cada especie.
df_group_mon <- esp_can %>% left_join(df_group_mon, by = "NATCODE")
df_group_mon$R0 <- df_group_mon$R0_mon_jap
month = 11
plot_11 <- plot_months(df_group_mon,month)
plot_11
ggarr <- ggarrange(plot_3,plot_4,plot_5,
          plot_6,plot_7,plot_8,
          plot_9,plot_10,plot_11,
          nrow=3,ncol = 3, common.legend = TRUE)

ggarr

Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/2022/WholeyearAeg",year,".pdf")
# dev.copy2pdf(file=Path, width = 7, height = 5)
# ggsave(Path, plot = ggarr)

### PLOT MONTH COMPARISON SPECIES #####
## Change some parts of the plots in order to compare one month between species
library("paletteer")
pal = paletteer_c("ggthemes::Red-Green-Gold Diverging", 30) 
# Definir colores base

plot_months <- function(df, month,esp){
  df1 <- df[which(df$month == month),]
  
  plot <- ggplot(df1) +
    geom_sf(aes(fill = R0), colour = NA) +
    geom_sf(data = can_box) + coord_sf(datum = NA) +
    scale_fill_viridis_c(option = "C",
                         limits = c(0,6),
                         name = TeX("$R_M$")) +
    ggtitle(esp) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5,face = "italic"),
          legend.position = c(0.1,0.8))
  return(plot)
}

## Month that you want to see the comparison
month = 7
df_group_mon$R0 <- df_group_mon$R0_mon_alb
plot_alb <- plot_months(df_group_mon,month, "Aedes albopictus")
plot_alb
df_group_mon$R0 <- df_group_mon$R0_mon_aeg
plot_aeg <- plot_months(df_group_mon,month, "Aedes aegypti")
plot_aeg
df_group_mon$R0 <- df_group_mon$R0_mon_jap
plot_jap <- plot_months(df_group_mon,month, "Aedes japonicus")
plot_jap

ggarr = ggarrange(plot_alb +
                    theme(legend.position = "none"),
                  plot_aeg +
                    theme(legend.position = "none"), 
                  ncol = 2 )
ggarrange(ggarr, plot_jap, ncol = 1,
          common.legend = TRUE, widths = c(1.1,0.65))

## ----------PLOT ANNUAL AVERAGE SEASON----------#
## Group by year:
df_group_mon$geometry <- NULL
df_group_mon <- setDT(df_group_mon)
df_group_y <- df_group_mon[,.(tmean = mean(tmean),
            prec = mean(prec),
            dens =min(dens),
            R0_an_alb = mean(R0_mon_alb),
            R0_an_aeg = mean(R0_mon_aeg),
            R0_an_jap = mean(R0_mon_jap),
            R0_sum_alb = sum(bool_R0_alb),
            R0_sum_aeg = sum(bool_R0_aeg),
            R0_sum_jap = sum(bool_R0_jap),
            R0_sum_alb_min = sum(bool_R0_alb_min),
            R0_sum_aeg_min = sum(bool_R0_aeg_min),
            R0_sum_jap_min = sum(bool_R0_jap_min),
            R0_sum_alb_max = sum(bool_R0_alb_max),
            R0_sum_aeg_max = sum(bool_R0_aeg_max),
            R0_sum_jap_max = sum(bool_R0_jap_max)),
            by = list(NATCODE)]

df_group_y[, R0_anual_alb := mapply(R0_func_alb, tmean, prec, dens)]
df_group_y[, R0_anual_aeg := mapply(R0_func_aeg, tmean, prec, dens)]
df_group_y[, R0_anual_jap := mapply(R0_func_jap, tmean, prec, dens)]

df_group_y <- esp_can %>% left_join(df_group_y)
ggplot(df_group_y) +
      geom_sf(aes(fill = R0_an_alb), colour = NA) +
      geom_sf(data = can_box) + coord_sf(datum = NA) +
      scale_fill_distiller(palette = "Spectral",
                           name = TeX("$R_M$")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

# ## Just season:
# df_group_y <- df_group_mon[which(df_group_mon$month<12 & df_group_mon$month>3 ),
#                            .(tmean = mean(tmean),
#                               prec = mean(prec),
#                               dens =min(dens),
#                              R0_an_alb = mean(R0_mon_alb),
#                              R0_an_aeg = mean(R0_mon_aeg),
#                              R0_an_jap = mean(R0_mon_jap),
#                               R0_sum_alb = sum(bool_R0_alb),
#                               R0_sum_aeg = sum(bool_R0_aeg),
#                               R0_sum_jap = sum(bool_R0_jap)), by = list(NATCODE)]
# 
### Compute the RM for the diferent species with the annual mean temperature
# df_group_y[, R0_anual_alb := mapply(R0_func_alb, tmean, prec, dens)]
# df_group_y[, R0_anual_aeg := mapply(R0_func_aeg, tmean, prec, dens)]
# df_group_y[, R0_anual_jap := mapply(R0_func_jap, tmean, prec, dens)]

## Construct the maps
# df_group_y <- esp_can %>% left_join(df_group_y)
# ggplot(df_group_y) +
#   geom_sf(aes(fill = R0_anual_alb), colour = NA) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   scale_fill_distiller(palette = "Spectral",
#                        name = TeX("$R_M$")) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5))

## Test if variables make sense:
# df_group_y <- esp_can %>% left_join(df_group_y)
ggplot(df_group_y) + 
  geom_sf(aes(fill = tmean)) +
  scale_fill_viridis_c()

ggplot(df_group_y) + 
  geom_sf(aes(fill = prec)) +
  scale_fill_viridis_c()

ggplot(df_group_y) + 
  geom_sf(aes(fill = dens)) +
  scale_fill_viridis_c()

## ----------------- TRANSITION MAPS ------------------#
# df_group_mon <- esp_can %>% left_join(df_group_mon)
# 
# ggplot(df_group_mon) +
#   geom_sf(aes(fill = R0_mon_jap), linewidth = 0.01) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   scale_fill_viridis_c() +
#   theme_void() +
#   labs(title = "Month: {current_frame}") +
#   transition_manual(as.factor(month))

# Whole map group by number of months suitable
library(ggpubr)
library(RColorBrewer)
name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal[11]
pal[12] = "#74011C"
pal[13] = "#4B0011"
letsize = 16
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
    scale_fill_manual(values = pal,
                      name = "Nº months\n suitable",
                      limits = factor(seq(0,12,1))) +
    theme_bw() 
}

## Computed with sum months maps 
df_group_y$R0 <- df_group_y$R0_sum_alb
plot_sum_alb <- plot_summonths(df_group_y)
plot_sum_alb

Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/AlboSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

df_group_y$R0 <- df_group_y$R0_sum_aeg
plot_sum_aeg <- plot_summonths(df_group_y)
plot_sum_aeg
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/AegSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

df_group_y$R0 <- df_group_y$R0_sum_jap
plot_sum_jap <- plot_summonths(df_group_y)
plot_sum_jap
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/JapSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

# Extract the legend
legend_only <- get_legend(plot_sum_alb +
                            theme(legend.position = "top"))
ggarrange(plot_sum_alb + ggtitle(expression(italic("Ae. albopictus")))+
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5)),
          plot_sum_aeg + ggtitle(expression(italic("Ae. aegypti")))+
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5)),
          plot_sum_jap + ggtitle(expression(italic("Ae. japonicus")))+
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5)),
          legend_only,
          ncol=2, nrow = 2)

### With minimum temp :
## Computed with sum months maps 
df_group_y$R0 <- df_group_y$R0_sum_alb_min
plot_sum_alb <- plot_summonths(df_group_y)
plot_sum_alb

Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/min_AlboSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

df_group_y$R0 <- df_group_y$R0_sum_aeg_min
plot_sum_aeg <- plot_summonths(df_group_y)
plot_sum_aeg
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/min_AegSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

df_group_y$R0 <- df_group_y$R0_sum_jap_min
plot_sum_jap <- plot_summonths(df_group_y)
plot_sum_jap
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/min_JapSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

# With maximum temp
## Computed with sum months maps 
df_group_y$R0 <- df_group_y$R0_sum_alb_max
plot_sum_alb <- plot_summonths(df_group_y)
plot_sum_alb

Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/max_AlboSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

df_group_y$R0 <- df_group_y$R0_sum_aeg_max
plot_sum_aeg <- plot_summonths(df_group_y)
plot_sum_aeg
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/max_AegSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

df_group_y$R0 <- df_group_y$R0_sum_jap_max
plot_sum_jap <- plot_summonths(df_group_y)
plot_sum_jap
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/max_JapSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

### ---------Average year monthly maps------------------#
max_R0 <- max(df_group_y$R0_an_aeg,
              df_group_y$R0_an_alb,
              df_group_y$R0_an_jap)
ggplot(df_group_y) +
  geom_sf(aes(fill = R0_an_alb), colour = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral",
                       name = TeX("$R0 annual$"),
                       limits =c(0,max_R0)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/avg_montly_AlbSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

## Aegypti
ggplot(df_group_y) +
  geom_sf(aes(fill = R0_an_aeg), colour = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral",
                       name = TeX("$R0 annual$"),
                       limits =c(0,max_R0)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/avg_montly_AegSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

## Japonicus
ggplot(df_group_y) +
  geom_sf(aes(fill = R0_an_jap), colour = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral",
                       name = TeX("$R0 annual$"),
                       limits =c(0,max_R0)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/avg_montly_JapSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

###--------TEST------------###
### Use the R0 compute daily and after average
df_group_mon1 <- df_group[, .(tmean = mean(tmean),
                             prec = sum(prec), 
                             dens = min(dens),
                             R0_mon_alb_dai = mean(R0_dai_alb),
                             R0_mon_aeg_dai = mean(R0_dai_aeg),
                             R0_mon_jap_dai = mean(R0_dai_jap)), 
                         by=list(NATCODE,month)]

df_group_mon1$bool_R0_alb <- ifelse(df_group_mon1$R0_mon_alb_dai < 1,0,1)
df_group_mon1$bool_R0_aeg <- ifelse(df_group_mon1$R0_mon_aeg_dai < 1,0,1)
df_group_mon1$bool_R0_jap <- ifelse(df_group_mon1$R0_mon_jap_dai < 1,0,1)

## Rename df and delete other variables:
df_group_y1 <- df_group_y
df_group_y1 <- df_group_y1[,c(8,c(19:34))]
df_group_y1$geometry <- NULL

df_group_y <- df_group_mon[, .(tmean = mean(tmean),
                             prec = sum(prec), 
                             meanprec = mean(prec),
                             dens = min(dens),
                             R0_sum_alb_dai = sum(bool_R0_alb),
                             R0_sum_aeg_dai = sum(bool_R0_aeg),
                             R0_sum_jap_dai = sum(bool_R0_jap)), 
                         by=list(NATCODE)]

df_group_y[, R0_avg_alb := mapply(R0_func_alb, tmean, meanprec, dens)]
df_group_y[, R0_avg_aeg := mapply(R0_func_aeg, tmean, meanprec, dens)]
df_group_y[, R0_avg_jap := mapply(R0_func_jap, tmean, meanprec, dens)]

df_group_y <- df_group_y %>% left_join(df_group_y1)
## Save file for the validation with PA data
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/R0/datasets/R0_",year,".Rds")
saveRDS(df_group_y, Path)

# ### Extract the average R0 for the mosquito season
# df_group_mon$geometry <- NULL
# df_group_mon <- setDT(df_group_mon)
# df_group_sum <- df_group_mon[which(df_group_mon$month >3 & df_group_mon$month <11),]
# df_group_sum <- df_group_sum[, .(tmean = mean(tmean),
#                                prec = sum(prec), 
#                                meanprec = mean(prec),
#                                dens = min(dens)), 
#                            by=list(NATCODE)]
# df_group_sum[, R0_avg_alb_sum := mapply(R0_func_alb, tmean, meanprec, dens)]
# df_group_sum[, R0_avg_aeg_sum := mapply(R0_func_aeg, tmean, meanprec, dens)]
# df_group_sum[, R0_avg_jap_sum := mapply(R0_func_jap, tmean, meanprec, dens)]
# 
# df_group_tot <- (df_group_y[,c("NATCODE",
#                               "R0_sum_alb",
#                               "R0_sum_aeg",
#                               "R0_sum_jap",
#                               "R0_an_alb",
#                               "R0_an_aeg",
#                               "R0_an_jap")] %>% 
#   left_join(df_group_y1) ) %>% left_join(df_group_sum[,c("NATCODE",
#                                                          "R0_avg_alb_sum",
#                                                          "R0_avg_aeg_sum",
#                                                          "R0_avg_jap_sum")])


### Plots maps
df_group_y <- esp_can %>% left_join(df_group_y)
df_group_y$R0 <- df_group_y$R0_sum_alb_dai
plot_sum_alb <- plot_summonths(df_group_y)
plot_sum_alb
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/daily_AlboSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

df_group_y$R0 <- df_group_y$R0_sum_aeg_dai
plot_sum_aeg <- plot_summonths(df_group_y)
plot_sum_aeg
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/daily_AegSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

df_group_y$R0 <- df_group_y$R0_sum_jap_dai
plot_sum_jap <- plot_summonths(df_group_y)
plot_sum_jap
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/daily_JapSum",year,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 5)

##--------------------TEST------------------------###
ggplot(df_group_y) + 
  geom_sf(aes(fill = prec)) +
  scale_fill_viridis_c()
