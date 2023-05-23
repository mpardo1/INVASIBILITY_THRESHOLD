rm(list=ls())
library(mapSpain)
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
library(tidyr)
library(gganimate)

# Spain map municipalities
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$R0_test <- runif(length(esp_can$codauto),0,5)
can_box <- esp_get_can_box()

ggplot(esp_can) +
  geom_sf(aes(fill = R0_test), size = 0.1) +
  geom_sf(data = can_box) + theme_bw()

# Path <- "/home/marta/INVASIBILITY_THRESHOLD/data/recintos_autonomicas_inspire_peninbal_etrs89.shp"
# df_muni <- read_sf(Path)
# Path <- "/home/marta/Documentos/PHD/2022/INVASIBILY/Municipios_Data/Municipios_IGN.shp"
# df_muni <- read_sf(Path)

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

# pop_mun_spain <- function(start_year, end_year){
#   
#   url <- paste0("https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/29005?date=", 
#                 start_year, "0101:", end_year, "1231")
#   
#   req <- GET(url)
#   
#   df <- jsonlite::fromJSON(rawToChar(req$content))
#   
#   df_subset <- df[df$Nombre %like% "Total. Total habitantes", ] %>%
#     tidyr::unnest(Data)
#   
#   df_subset_n <- df_subset %>% separate(Nombre, sep = ". Total. Total habitantes.", 
#                                         into = c("Municipality", "Remove")) %>%
#     dplyr:: select(COD,Municipality, Anyo, Valor) %>%
#     rename(
#       COD = COD,
#       municipality = Municipality,
#       year = Anyo,
#       population = Valor)
#   
#   return(df_subset_n)
# }
# 
# df_pop <- pop_mun_spain(2010,2010)

#### -------------------------- Albopictus ------------------------- ####
## Thermal responses Aedes Albopictus from Mordecai 2017:
a_f_alb <- function(temp){Briere_func(0.000193,10.25,38.32,temp)} # Biting rate
TFD_f_alb <- function(temp){Briere_func(0.0488,8.02,35.65,temp)} # Fecundity
pLA_f_alb <- function(temp){Quad_func(2.663e-03,6.668e+00,3.892e+01,temp)} # Survival probability Egg-Adult
MDR_f_alb <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_f_alb <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span
dE_f_alb <- function(temp){4.66e-03*temp -4.23e-02} # Adult life span

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

# R0 function by temperature:
R0_func_alb <- function(rain,hum,Te){
  a <- a_f_alb(Te)
  f <- TFD_f_alb(Te)
  deltaa <- 1/lf_f_alb(Te)
  probla <- pEA_f_alb(Te)
  h <- h_f(hum,rain)
  deltE = 0.1
  R0 <- sqrt(f*(a/deltaa)*probla*(h*(h+deltE)))
  return(R0)
}

# Population density in each municipality.
census <- mapSpain::pobmun19
esp_can_pop <- esp_can %>% left_join(census, by = c("cmun" = "cmun","cpro" = "cpro"))
esp_can_pop$area <- as.numeric(st_area(esp_can_pop))/1000000
esp_can_pop$pop_km <- esp_can_pop$pob19/esp_can_pop$area
esp_can_pop$NATCODE <- as.numeric(paste0("34",
                                         esp_can_pop$codauto,
                                         esp_can_pop$cpro,
                                         esp_can_pop$LAU_CODE))
esp_can_pop <- esp_can_pop[,c("pop_km","NATCODE")]
esp_can_pop$geometry <- NULL

#####
# Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/R0_aemet_weather_year_2_22.Rds"
R0_monthly <- function(year){
  Path = paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/",year,"/")
  listfile <- list.files(Path)
  weather_t <- data.table()
  for(file in listfile){
    Path <- paste0(Path,file)
    weather <- readRDS(Path)
    weather_df <- as.data.frame(do.call(rbind, weather))
    weather_dt <- setDT(weather_df) # Convert data.frame to data.table
    rm(weather,weather_df)
    weather_dt$month <- lubridate::month(weather_dt$date)
    weather_dt$year <- lubridate::year(weather_dt$date)
    weather_dt <- weather_dt[ , .(tmed = mean(tmean),
                                  tmin = min(tmean),
                                  tmax = max(tmean)),by = list(month,year,NATCODE)] 
    weather_t <- rbind(weather_t,weather_dt)
    
  }
  
  Path = paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/rainfall/",year,"/")
  listfile <- list.files(Path)
  weather_r <- data.table()
  for(file in listfile){
    Path <- paste0(Path,file)
    weather <- readRDS(Path)
    weather_df <- reshape2::melt(weather,id.vars = "NATCODE")
    colnames(weather_df) <- c("NATCODE", "date", "precmed")
    weather_df$date <- as.Date(weather_df$date, "%Y-%m-%d" )
    weather_dt <- setDT(weather_df) # Convert data.frame to data.table
    rm(weather,weather_df)
    weather_dt$month <- lubridate::month(weather_dt$date)
    weather_dt$year <- lubridate::year(weather_dt$date)
    weather_dt <- weather_dt[ , .(sumprec = sum(precmed),
                                  precmin = min(precmed),
                                  precmax = max(precmed)),
                              by = list(month,year,NATCODE)] 
    weather_r <- rbind(weather_r,weather_dt)
    
  }
  
  weather_w <- weather_t %>% left_join(weather_r, 
                                       by = c("NATCODE","month","year"))
  weather <- weather_w %>% left_join(esp_can_pop, 
                                     by = c("NATCODE"))
  
  weather$R0_med <- 0
  weather$R0_min <- 0
  weather$R0_max <- 0
  
  for(i in c(1:nrow(weather))){
    print(paste0("i:",i))
    weather$R0_med[i] <- R0_func_alb(weather$sumprec[i],
                                     weather$pop_km[i], 
                                     weather$tmed[i])
    weather$R0_min[i] <- R0_func_alb(weather$sumprec[i],
                                     weather$pop_km[i], 
                                     weather$tmin[i])
    weather$R0_max[i] <- R0_func_alb(weather$sumprec[i],
                                     weather$pop_km[i], 
                                     weather$tmax[i])
  }
  return(weather)
}

R0_daily_muni <- function(year,muni){
  Path = paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/",year,"/")
  listfile <- list.files(Path)
  weather_t <- data.table()
  for(file in listfile){
    Path2 <- paste0(Path,file)
    weather <- readRDS(Path2)
    weather_df <- as.data.frame(do.call(rbind, weather))
    weather_dt <- setDT(weather_df) # Convert data.frame to data.table
    rm(weather,weather_df)
    weather_dt <- weather_dt[which(weather_dt$NATCODE == muni),]
    weather_t <- rbind(weather_t,weather_dt)
  }
  
  rm(weather_dt)
  Path = paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/rainfall/",year,"/")
  listfile <- list.files(Path)
  weather_r <- data.table()
  for(file in listfile){
    Path1 <- paste0(Path,file)
    weather <- readRDS(Path1)
    weather_df <- reshape2::melt(weather,id.vars = "NATCODE")
    colnames(weather_df) <- c("NATCODE", "date", "precmed")
    weather_df$date <- as.Date(weather_df$date, "%Y-%m-%d" )
    weather_dt <- setDT(weather_df) # Convert data.frame to data.table
    rm(weather,weather_df)
    weather_dt <- weather_dt[which(weather_dt$NATCODE == muni),]
    weather_r <- rbind(weather_r,weather_dt)
    
  }
  rm(weather_dt)
  weather_w <- weather_t %>% left_join(weather_r, 
                                       by = c("NATCODE","date"))
  weather <- weather_w %>% left_join(esp_can_pop, 
                                     by = c("NATCODE"))
  
  weather$R0_med <- 0
  weather[which(is.na(weather$precmed)),] <- 0
  for(i in c(1:nrow(weather))){
    print(paste0("i:",i))
    weather$R0_med[i] <- R0_func_alb(weather$precmed[i],
                                     weather$pop_km[i], 
                                     weather$tmean[i])
  }
  return(weather)
}

year_n = "2020"
# R0mon <- R0_monthly(year_n)
# saveRDS(R0mon,"~/INVASIBILITY_THRESHOLD/output/R0/R0_ERA5_2020.Rds")
R0mon <- readRDS("~/INVASIBILITY_THRESHOLD/output/R0/R0_ERA5_2020.Rds")
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
muni_n = unique(esp_can[which(esp_can$name == "Barcelona"),"NATCODE"])
R0day_bar <- R0_daily_muni(year_n, muni_n$NATCODE)
esp_can <- esp_can[,c("NATCODE")]
weather_esp <- esp_can %>% left_join(weather)

# # Create plots:
ggplot(weather_esp) +
  geom_sf(aes(fill = R0_med), linewidth = 0.01) +
  scale_fill_viridis(name = "R0(T)", limits = c(0,max(weather_esp$R0_med))) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Month: {current_frame}") +
  transition_manual(month)

weather_esp$bool <- ifelse(weather_esp$R0_med >=1,1,0)
weather_bool <- weather_esp %>% group_by(NATCODE,year) %>%
  summarize(sum_bool = sum(bool),
            avg_temp = mean(tmed),
            avg_R0 = mean(R0_med))

plot_sum_albo <- ggplot(weather_bool) +
  geom_sf(aes(fill = sum_bool), lwd = 0) +
  scale_fill_viridis(name = "Nº of months with R0>1",
                     limits = c(0, 12), option="turbo") +
  geom_sf(data = can_box) + coord_sf(datum = NA)  +
  ggtitle("Aedes Albopictus 2020") +
  theme_bw()
plot_sum_albo


R0day_bar$year <- lubridate::year(R0day_bar$date)
R0_bar <- R0day_bar[which(R0day_bar$year == "2020"),]
ggplot(R0_bar) + 
  geom_line(aes(date,R0_med), size = 0.5) +
  theme_bw()

##---------------------------Hatching Rate-----------------------------------#
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
Path <- "~/Documents/PHD/2023//Mosquito\ invasibility/outputhatching.Rds"
# saveRDS(df_xy,Path)
df_xy <- readRDS(Path)

library(rgl)
library(RColorBrewer)
# Create a color palette using the color_brewer package
my_colors <- c("yellow","red", "black")
my_palette <- colorRampPalette(my_colors)(100)

plot3d(x=df_xy$Var1,y=df_xy$Var2,z=df_xy$z, col = my_palette[df_xy$z * 100],
       cex.lab=10,cex.axis=5,
       xlab = "Human density (km2)", ylab = "Rainfall(mm day)",
       zlab = "Haching rate") 


rgl.postscript("~/Documents/PHD/2023//Mosquito\ invasibility/Plots/my_plot.ps", fmt = "eps")


