### Code to do a validation between female traps counts and R0
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

# -------------------------Functions R0-------------------------#
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

## Data Catu ------------------------------------------------------
Path <- "~/INVASIBILITY_THRESHOLD/data/Datos_Catu/gi_min_model_pred.RData"
load(Path)
unique(gi_min_model_pred$trap_name)

# Process data from Catu with trap data ----------------------------
trap_data <- setDT(gi_min_model_pred[,c("trap_name", "province",
                                        "city", "start_date","end_date",
                                        "females","precipitation", "mean_temperature",
                                        "population", "pred", "l7precipitation",
                                        "l14precipitation", "l21precipitation", 
                                        "trapping_effort", "pred")])
rm(gi_min_model_pred)
trap_data$prec7 <- trap_data$l7precipitation/7
trap_data$prec14 <- trap_data$l14precipitation/14
trap_data$trapping_effort <- as.numeric(trap_data$trapping_effort)
trap_data$females_daily <- trap_data$females/trap_data$trapping_effort
trap_data[which(trap_data$city == "La Bisbal de l'Empordà"), "population"] <- 10859
trap_data[, R0_alb := mapply(R0_func_alb, mean_temperature, prec7, population)]
trap_data$female_norm <- trap_data$females/max(trap_data$females)
trap_data$pred_norm <- trap_data$pred/max(trap_data$pred)

# Compare model sampling effort versus count females ----------------
ggplot(trap_data) +
  geom_point(aes(start_date, female_norm), color = "red")+
  geom_point(aes(start_date, pred_norm), color = "blue")

# Normalized R0 between 0 and 1 to compare with counts --------------
trap_data$R0_alb_norm <- trap_data$R0_alb/max(trap_data$R0_alb)

# DF of specific cities ---------------------------------------------------
list_cit <- list("Blanes", "Lloret de Mar", "Tordera", "Palafolls")
trap_data_filt <- trap_data[which(trap_data$city %in% list_cit)] %>%
  group_by(city, start_date) %>% 
  summarise(female = sum(females),
            R0_alb = mean(R0_alb))

# DF compute maximum -------------------------------------------------------
trap_citi_max <- trap_data_filt %>%
  group_by(city) %>% 
  summarise(female_max= max(female),
            R0_alb_max = max(R0_alb))

# DF compute norm female and R0  --------------------------------------------
trap_data_filt <- trap_data_filt %>% left_join(trap_citi_max)
trap_data_filt$female_norm <- trap_data_filt$female/trap_data_filt$female_max
trap_data_filt$R0_alb_norm <- trap_data_filt$R0_alb/trap_data_filt$R0_alb_max


ggplot(trap_data_filt) +
  geom_point(aes(R0_alb_norm,female_norm, color = city))

# Fit each dots by city -------------------------------------------------------
df_out_fem <- data.frame()
for(i in c(1:length(list_cit))){
  Fitting_fem <- nls(female_norm ~ exp(cont1*R0_alb_norm)*cont,
                     data = trap_data_filt[which(trap_data_filt$city == list_cit[[i]]),],
                     start = list(cont = 0.001, cont1 = 0))
  
  summary(Fitting_fem)
  
  mod <- function(te){
    t0 <- as.numeric(Fitting_fem$m$getPars()[1])
    tm <- as.numeric(Fitting_fem$m$getPars()[2])
    t0*exp(tm*te)
  }
  
  vec <- seq(0,1,0.01)
  df_aux <- data.frame(temp_ae <- vec,
                       fem <- sapply(vec, mod))
  df_aux$cit <- list_cit[[i]]
  colnames(df_aux) <- c("vec", "out", "cit")
  df_out_fem <- rbind(df_aux, df_out_fem)
  colnames(df_out_fem) <- c("vec", "out", "cit")
}

# Plot with dots and fit exp ---------------------------------------------
name_pal = "Set1"
display.brewer.pal(length(list_cit), name_pal)
pal <- rev(brewer.pal(length(list_cit), name_pal))
sizelet = 14
plot_counts <- ggplot(data = trap_data_filt) +
  geom_point(aes(x = R0_alb_norm, 
                 y = female_norm, color = city), size = 0.8, alpha = 0.6) +
  geom_line(data = df_out_fem,
            aes(vec , out, color = cit),
            lwd = 0.8) +
  scale_color_manual(values = pal) +
  xlab(TeX("Relative $R_M$")) +
  ylab("Normalized number of females") +
  theme_bw() + 
  theme(text = element_text(size = sizelet),
        legend.position = c(0.2,0.65))
plot_counts


# Load Data --------------------------------------------------------------------
## year = 2022, we will use this year to validate the data
year = 2020
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/R0/datasets/R0_",year,".Rds")
df_group_tot <- readRDS(Path)
head(df_group_tot)

## Data Catu BG traps
Path <- "~/INVASIBILITY_THRESHOLD/data/Datos_Catu/gi_min_model_pred.RData"
load(Path)
unique(gi_min_model_pred$trap_name)

# Presence absence data Albopictus Spain:
Path = "~/INVASIBILITY_THRESHOLD/data/PA/Albopictus_Spain_Pa.csv"
df_pa <- read.csv(Path)
df_pa[which(df_pa$NATCODE == "34011111012"), "PA"] <- 1

# Map Spain --------------------------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
can_box <- esp_get_can_box()

# Plot PA Albopictus -----------------------------------------------------------
df_pa <- esp_can %>% left_join(df_pa)
PA_alb <- ggplot(df_pa) +
  geom_sf(aes(fill = as.factor(PA)), color = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_manual(values = c("#D9EAF8","#377EB8"), name = "PA") +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void()
PA_alb

### Comparison between presence absence and number of months R0>1
df_pa <- df_pa[, c("NATCODE", "PA")]
df_pa$geometry <- NULL

df_group_m <- df_group_tot[,c("NATCODE",
                              "R0_sum_alb",
                              "R0_sum_alb_min",
                              "R0_sum_alb_max",
                              "R0_sum_alb_dai")]
df_pa <- df_pa %>% left_join(df_group_m)

NATCODE_CAT <- esp_can[which(esp_can$ine.ccaa.name == "Cataluña"),"NATCODE"]
NATCODE_CAT$geometry <- NULL
df_pa_CAT <- df_pa[which(as.numeric(df_pa$NATCODE) %in% 
                           as.numeric(NATCODE_CAT$NATCODE)),]

### Sum months suitable:
hist(df_pa_CAT[which(df_pa_CAT$PA == 1), "R0_sum_alb_min"])
ggplot(df_pa_CAT[which(df_pa_CAT$PA == 1),]) +
  geom_histogram(aes(R0_sum_alb), binwidth = 0.2,
                 fill =  "#E1CE7A") + xlab("Number months suitable") +
  theme_bw()

## Average R0 annual:
ggplot(df_pa_CAT[which(df_pa_CAT$PA == 1),]) +
  geom_histogram(aes(R0_sum_alb), binwidth = 0.2,
                 fill =  "#E1CE7A") + xlab("Number months suitable") +
  theme_bw()

# Prop presence df ------------------------------------------
df_num_months <- function(df_pa_CAT){
  df_r_1 <- df_pa_CAT[which(df_pa_CAT$PA == 1),] %>% 
    group_by(R0_sum_alb) %>% summarize(num_1 = n())
  df_r_0 <- df_pa_CAT[which(df_pa_CAT$PA == 0),] %>% 
    group_by(R0_sum_alb) %>% summarize(num_0 = n())
  
  df_r <- merge(df_r_1 ,df_r_0,
                all.x = TRUE, all.y = TRUE)
  df_r$prop_1 <- ifelse(is.na(df_r$num_0),1,
                        ifelse(is.na(df_r$num_1),
                               0,df_r$num_1/(df_r$num_1+df_r$num_0)))
  df_r$num_1 <- ifelse(is.na(df_r$num_1),0,df_r$num_1)
  df_r$num_0 <- ifelse(is.na(df_r$num_0),0,df_r$num_0)
  df_r$sum_muni <- df_r$num_1 + df_r$num_0
  return(df_r)
  
}

# Prop presence df min temp ------------------------------------------
df_num_months_min <- function(df_pa_CAT){
  df_r_1 <- df_pa_CAT[which(df_pa_CAT$PA == 1),] %>% 
    group_by(R0_sum_alb_min) %>% summarize(num_1 = n())
  df_r_0 <- df_pa_CAT[which(df_pa_CAT$PA == 0),] %>% 
    group_by(R0_sum_alb_min) %>% summarize(num_0 = n())
  
  df_r <- merge(df_r_1 ,df_r_0,
                all.x = TRUE, all.y = TRUE)
  df_r$prop_1 <- ifelse(is.na(df_r$num_0),1,
                        ifelse(is.na(df_r$num_1),
                               0,df_r$num_1/(df_r$num_1+df_r$num_0)))
  df_r$num_1 <- ifelse(is.na(df_r$num_1),0,df_r$num_1)
  df_r$num_0 <- ifelse(is.na(df_r$num_0),0,df_r$num_0)
  df_r$sum_muni <- df_r$num_1 + df_r$num_0
  return(df_r)
}

# Prop presence df max temp ------------------------------------------
df_num_months_max <- function(df_pa_CAT){
  df_r_1 <- df_pa_CAT[which(df_pa_CAT$PA == 1),] %>% 
    group_by(R0_sum_alb_max) %>% summarize(num_1 = n())
  df_r_0 <- df_pa_CAT[which(df_pa_CAT$PA == 0),] %>% 
    group_by(R0_sum_alb_max) %>% summarize(num_0 = n())
  
  df_r <- merge(df_r_1 ,df_r_0, all.x = TRUE, all.y = TRUE)
  df_r$prop_1 <- ifelse(is.na(df_r$num_0),1,
                        ifelse(is.na(df_r$num_1),
                               0,df_r$num_1/(df_r$num_1+df_r$num_0)))
  df_r$num_1 <- ifelse(is.na(df_r$num_1),0,df_r$num_1)
  df_r$num_0 <- ifelse(is.na(df_r$num_0),0,df_r$num_0)
  df_r$sum_muni <- df_r$num_1 + df_r$num_0
  return(df_r)
}

# Prop presence df avg R0 ------------------------------------------
df_avg_months <- function(df_pa_CAT){
  df_r_1 <- df_pa_CAT[which(df_pa_CAT$PA == 1),] %>% 
    group_by(R0_rang_alb) %>% summarize(num_1 = n())
  df_r_0 <- df_pa_CAT[which(df_pa_CAT$PA == 0),] %>% 
    group_by(R0_rang_alb) %>% summarize(num_0 = n())
  
  df_r <- merge(df_r_1 ,df_r_0, all.x = TRUE, all.y = TRUE)
  df_r$prop_1 <- ifelse(is.na(df_r$num_0),1,
                        ifelse(is.na(df_r$num_1),
                               0,df_r$num_1/(df_r$num_1+df_r$num_0)))
  df_r$sum_muni <- df_r$num_1 + df_r$num_0
  return(df_r)
}

# Filter df -----------------------------------------------------
df_pa <- df_pa[, c("NATCODE", "PA")]
df_pa$geometry <- NULL

# Join map with PA data -----------------------------------------
df_group_m <- df_group_tot
df_pa <- df_pa %>% left_join(df_group_m)

unique(esp_can$ine.ccaa.name)

# Func to compute plot PA prop vs summonths --------------------
plot_sum_p <- function(ccaa){
  NATCODE_CAT <- esp_can[which(esp_can$ine.ccaa.name == ccaa),"NATCODE"]
  NATCODE_CAT$geometry <- NULL
  df_pa_CAT <- df_pa[which(as.numeric(df_pa$NATCODE) %in% 
                             as.numeric(NATCODE_CAT$NATCODE)),]
  
  df_sum_CAT <- df_num_months(df_pa_CAT)
  df_sum_CAT$ccaa <- ccaa
  
  ggplot(df_sum_CAT) +
    geom_point(aes(R0_sum_alb,prop_1)) +
    xlab("Nº months suitable") + 
    ylab("Proportion of municipalities with presence") +
    ylim(c(0,1)) +
    ggtitle(ccaa) +    
    scale_x_continuous(breaks = seq(1,12,1)) +
    theme_bw()
}

# Func to compute plot PA prop vs summonths max temp ----------------
plot_sum_p_max <- function(ccaa){
  NATCODE_CAT <- esp_can[which(esp_can$ine.ccaa.name == ccaa),"NATCODE"]
  NATCODE_CAT$geometry <- NULL
  df_pa_CAT <- df_pa[which(as.numeric(df_pa$NATCODE) %in% 
                             as.numeric(NATCODE_CAT$NATCODE)),]
  
  df_sum_CAT <- df_num_months_max(df_pa_CAT)
  df_sum_CAT$ccaa <- ccaa
  
  ggplot(df_sum_CAT) +
    geom_point(aes(R0_sum_alb_max,prop_1)) +
    xlab("Nº months suitable") + 
    ylab("Proportion of municipalities with presence") +
    ylim(c(0,1)) +
    ggtitle(ccaa) +    
    scale_x_continuous(breaks = seq(1,12,1)) +
    theme_bw()
}

# Func to compute plot PA prop vs summonths -----------------------
plot_sum_p_min <- function(ccaa){
  NATCODE_CAT <- esp_can[which(esp_can$ine.ccaa.name == ccaa),"NATCODE"]
  NATCODE_CAT$geometry <- NULL
  df_pa_CAT <- df_pa[which(as.numeric(df_pa$NATCODE) %in% 
                             as.numeric(NATCODE_CAT$NATCODE)),]
  
  df_sum_CAT <- df_num_months_min(df_pa_CAT)
  df_sum_CAT$ccaa <- ccaa
  
  ggplot(df_sum_CAT) +
    geom_point(aes(R0_sum_alb_min,prop_1)) +
    xlab("Nº months suitable") + 
    ylab("Proportion of municipalities with presence") +
    ylim(c(0,1)) +
    ggtitle(ccaa) +  
    scale_x_continuous(breaks = seq(1,12,1)) +
    theme_bw()
}

# DF with ccaa names ----------------------------------------------
esp_can_ccaa <- esp_can[,c("NATCODE", "ine.ccaa.name")]
df_pa_ccaa <- esp_can %>% left_join(df_pa)

# Join more than one ccaa ---------------------------------------------
list_ccaa = c("Cataluña" ,  
              "Comunitat Valenciana", 
              "País Vasco","Andalucía","Aragón")

NATCODE_CAT <- esp_can[which(esp_can$ine.ccaa.name %in% list_ccaa ),"NATCODE"]

# Add PA data --------------------------------------------------------
NATCODE_CAT$geometry <- NULL
df_pa_CAT <- df_pa[which(as.numeric(df_pa$NATCODE) %in% 
                           as.numeric(NATCODE_CAT$NATCODE)),]

# Compute df with prop PA --------------------------------------------
df_sum_CAT <- data.frame()
for(i in c(1:length(list_ccaa))){
  df_aux <- df_num_months(df_pa_CAT[which(df_pa_CAT$ine.ccaa.name == list_ccaa[i] ),])
  df_aux$ccaa_n <- list_ccaa[i]
  df_sum_CAT <- rbind(df_aux,df_sum_CAT)
}

# Plot color related to ccaa ----------------------------------------
name_pal = "Set1"
display.brewer.pal(length(list_ccaa), name_pal)
pal <- rev(brewer.pal(length(list_ccaa), name_pal))
plot_ccaa <- ggplot(df_sum_CAT) +
  geom_line(aes(R0_sum_alb,prop_1, color =ccaa_n)) +
  geom_point(aes(R0_sum_alb,prop_1, color =ccaa_n,
                 size = sum_muni), alpha = 0.6) +
  xlab("Nº months suitable") + 
  ylab("Proportion of municipalities with presence") +
  ylim(c(0,1)) + 
  scale_color_manual(name = "", values = pal) +
  scale_size_continuous(name = "Number municipalities",
                        breaks = c(5,100,200),
                        labels =c("<5","[5,100]",
                                  ">200")) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  theme_bw() +
  theme(legend.position = c(0.15,0.55),
        text = element_text(size = 14)) 
plot_ccaa

# Plot all ccaa selected together ----------------------------------------
df_group <- df_sum_CAT %>% group_by(R0_sum_alb) %>%
  summarise(num_1 = sum(num_1),
            num_0 = sum(num_0),
            prop_1 = sum(num_1)/(sum(num_0)+sum(num_1)),
            sum_muni = sum(sum_muni))
plot_tot <- ggplot(df_group) +
  geom_line(aes(R0_sum_alb,prop_1)) +
  geom_point(aes(R0_sum_alb,prop_1,
                 size = sum_muni), alpha = 0.6) +
  xlab("Nº months suitable") + 
  ylab("Proportion of municipalities with presence") +
  ylim(c(0,1)) + 
  scale_size_continuous(name = "Number municipalities",
                        breaks = c(5,100,500,1000,1500),
                        labels =c("[0,5]","[5,100]",
                                  "[100,500]","[500,1000]",
                                  "[1000, 1500]")) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.position = c(0.2,0.6)) 

# Plot all ccaa ----------------------------------------
df_aux <- df_num_months(df_pa)
df_group <- df_aux %>% group_by(R0_sum_alb) %>%
  summarise(num_1 = sum(num_1),
            num_0 = sum(num_0),
            prop_1 = sum(num_1)/(sum(num_0)+sum(num_1)),
            sum_muni = sum(sum_muni))
sum(df_group$sum_muni)
plot_esp <- ggplot(df_group) +
  geom_line(aes(R0_sum_alb,prop_1)) +
  geom_point(aes(R0_sum_alb,prop_1,
                 size = sum_muni), alpha = 0.6) +
  xlab("Nº months suitable") + 
  ylab("Proportion of municipalities with presence") +
  ylim(c(0,1)) + 
  scale_size_continuous(name = "Number municipalities",
                        breaks = c(100,500,1000,2000,3000, 4000),
                        labels =c("[0,100]","[100,500]",
                                  "[500,1000]","[1000,2000]",
                                  "[2000, 3000]",
                                  "[3000, 4000]")) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.position = c(0.2,0.6)) 

# Join min, max, mean toguether ---------------------------------------
NATCODE_CAT$geometry <- NULL
df_pa_CAT <- df_pa[which(df_pa$ine.ccaa.name == "Cataluña"),]

# Compute df with prop PA --------------------------------------------
df_sum_CAT <- data.frame()
df_mean <- df_num_months(df_pa_CAT)
df_min <- df_num_months_min(df_pa_CAT)
df_max <- df_num_months_max(df_pa_CAT)
df_mean$temp <- "Mean temp"
df_min$temp <- "Min temp"
colnames(df_min)[1] <- "R0_sum_alb"
df_max$temp <- "Max temp"
colnames(df_max)[1] <- "R0_sum_alb"
df_sum_CAT <- rbind(df_mean,df_min,df_max)

# Plot color related to temp min max o mean ---------------------------------
name_pal = "Set1"
display.brewer.pal(3, name_pal)
pal <- rev(brewer.pal(3, name_pal))
plot_min_max <- ggplot(df_sum_CAT) +
  geom_line(aes(R0_sum_alb,prop_1, color =temp)) +
  geom_point(aes(R0_sum_alb,prop_1, color =temp,
                 size = sum_muni), alpha = 0.6) +
  xlab("Nº months suitable") + 
  ylab("Proportion of municipalities with presence") +
  ylim(c(0,1)) + 
  scale_color_manual(name = "", values = pal) +
  scale_size_continuous(name = "Number municipalities",
                        breaks = c(5,100,200),
                        labels =c("<5","[5,200]",
                                  ">200")) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  theme_bw() +
  theme(legend.position = c(0.73,0.2),
        legend.box = "horizontal",
        text = element_text(size = 14)) 

ggarr <- ggarrange(NULL,PA_alb+ ggtitle("A"),plot_counts+ ggtitle("B"),
                   nrow = 1, widths = c(0.15,1,1))

ggarrange(ggarr ,
          plot_ccaa+ ggtitle("C"), 
          ncol = 1, nrow = 2)

ggarrange(ggarr,
          plot_ccaa + ylab("Proportion presence")+ ggtitle("C"),
          plot_min_max + ylab("Proportion presence")+ ggtitle("D"),
          ncol = 1, nrow = 3)

