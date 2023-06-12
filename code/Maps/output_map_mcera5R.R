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

df_group <- readRDS("~/INVASIBILITY_THRESHOLD/output/R0/R0_ERA5_daily_mcera_2020.Rds")

#-----------------------------Create plots------------------------#
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_group$month <- lubridate::month(df_group$date)

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
            R0_tmed_jap = mean(R0_hourly_jap))

df_group  <- df_group %>% left_join(R0_cpro)
df_group  <- df_group[,c("NATCODE", "month", "temp", "min_temp",
                         "max_temp","prec",  "pop", "R0_tmed_alb",
                         "R0_tmed_aeg", "R0_tmed_jap", "R0_hourly_alb",
                         "R0_hourly_aeg", "R0_hourly_jap")]
rm(R0_cpro)

df_group$R0_hourly_alb <- ifelse(is.na(df_group$R0_hourly_alb),
                           df_group$R0_tmed_alb,
                           df_group$R0_hourly_alb)
df_group$R0_hourly_aeg <- ifelse(is.na(df_group$R0_hourly_aeg),
                                 df_group$R0_tmed_aeg,
                                 df_group$R0_hourly_aeg)
df_group$R0_hourly_jap <- ifelse(is.na(df_group$R0_hourly_jap),
                                 df_group$R0_tmed_jap,
                                 df_group$R0_hourly_jap)

df_group_na <- df_group[which(is.na(df_group$R0_hourly_jap)),]

## Whole map moving monthly
df_group_mon <- df_group[, .(temp = mean(temp),
                             prec = sum(prec), 
                             pop = min(pop),
                             R0_monthly_alb = mean(R0_hourly_alb),
                             R0_monthly_aeg = mean(R0_hourly_aeg),
                             R0_monthly_jap = mean(R0_hourly_jap)), 
                         by=list(NATCODE,month)]

df_group_mon <- esp_can %>%
  left_join(df_group_mon)

rm(df_group)

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

df_group_y <- df_group_mon %>% group_by(NATCODE) %>%
  summarise(R0_sum_alb = sum(bool_R0_alb),
            R0_sum_aeg = sum(bool_R0_aeg),
            R0_sum_jap = sum(bool_R0_jap),
            R0_avg_alb = mean(R0_monthly_alb),
            R0_avg_aeg = mean(R0_monthly_aeg),
            R0_avg_jap = mean(R0_monthly_jap),
            avg_temp = mean(temp),
            sum_prec = sum(prec))

# Whole map group by number of months suitable
library(RColorBrewer)
library(ggpubr)
plot_sum_alb <- ggplot(df_group_y) +
  geom_sf(aes(fill = R0_sum_alb), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral",
                       name = "Nº months suitable",
                       limits=c(0,12)) +
  theme_bw()

plot_sum_aeg <- ggplot(df_group_y) +
  geom_sf(aes(fill = R0_sum_aeg), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral",
                       name = "Nº months suitable",
                       limits=c(0,12)) +
  theme_bw()

plot_sum_jap <- ggplot(df_group_y) +
  geom_sf(aes(fill = R0_sum_jap), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_distiller(palette = "Spectral",
                       name = "Nº months suitable",
                       limits=c(0,12)) +
  theme_bw()


ggarrange(plot_sum_alb + ggtitle("Aedes Albopictus"),
          plot_sum_aeg + ggtitle("Aedes Aegypti"),
          plot_sum_jap + ggtitle("Aedes Japonicus"),
          common.legend = TRUE)

### Plot of average temperature and rainfall
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
  geom_point(aes(R0_avg, PA), size = 0.7) + 
  geom_vline(aes(xintercept = 1),
             linetype = "dashed", color = "red") +
  xlab("Annual average R0") + 
  theme_bw()

plot_pa_daily_avg <- ggplot(df_pa) + 
  geom_point(aes(R0_avg_comp, PA), size = 0.7) + 
  geom_vline(aes(xintercept = 1), 
             linetype = "dashed", color = "red") +
  xlab("Annual average R0") + 
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
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(legend.position = c(0.8, 0.2)) +
  theme_bw()
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
