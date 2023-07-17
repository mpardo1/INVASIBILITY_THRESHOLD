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

## year = 2022, we will use this year to validate the data
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/R0/datasets/R0_2022.Rds")
df_group_tot <- readRDS(Path)

## Data Catu 
Path <- "~/INVASIBILITY_THRESHOLD/data/Datos_Catu/gi_min_model_pred.RData"
load(Path)
unique(gi_min_model_pred$trap_name)

###----------------- VALIDATION -----------------------#
## Map Spain -------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
can_box <- esp_get_can_box()

# Presence absence data Albopictus Spain:
Path = "~/INVASIBILITY_THRESHOLD/data/PA/Albopictus_Spain_Pa.csv"
df_pa <- read.csv(Path)

df_pa <- esp_can %>% left_join(df_pa)
ggplot(df_pa) +
  geom_sf(aes(fill = as.factor(PA)), linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_manual(values = c("#713E5A","#63A375"), name = "PA") +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void()

### Comparison between presence absence and number of months R0>1
df_pa <- df_pa[, c("NATCODE", "PA")]
df_pa$geometry <- NULL

df_group_m <- df_group_tot[,c("NATCODE",
                              "R0_sum_alb",
                              "R0_sum_alb_min",
                              "R0_sum_alb_max",
                              "R0_avg_alb",
                              "R0_sum_alb_dai")]
df_pa <- df_pa %>% left_join(df_group_m)

NATCODE_CAT <- esp_can[which(esp_can$ine.ccaa.name == "Cataluña"),"NATCODE"]
NATCODE_CAT$geometry <- NULL
df_pa_CAT <- df_pa[which(as.numeric(df_pa$NATCODE) %in% 
                           as.numeric(NATCODE_CAT$NATCODE)),]

### Sum months suitable:
hist(df_pa_CAT[which(df_pa_CAT$PA == 1), "R0_sum_alb"])
ggplot(df_pa_CAT[which(df_pa_CAT$PA == 1),]) +
  geom_histogram(aes(R0_sum_alb), binwidth = 0.2,
                 fill =  "#E1CE7A") + xlab("Number months suitable") +
  theme_bw()

## Average R0 annual:
ggplot(df_pa_CAT[which(df_pa_CAT$PA == 1),]) +
  geom_histogram(aes(R0_sum_alb), binwidth = 0.2,
                 fill =  "#E1CE7A") + xlab("Number months suitable") +
  theme_bw()

#-------------------------------P/(P+A)----------------------------------------#
# ### Range Annual average R0
# df_range <- function(df_pa_CAT){
#   df_pa_CAT$range <- ifelse(df_pa_CAT$R0_avg_alb  < 1, "<1",
#                             ifelse(df_pa_CAT$R0_avg_alb  >= 1 & df_pa_CAT$R0_avg_alb  < 2, "[1,2)",
#                                    ifelse(df_pa_CAT$R0_avg_alb  >= 2 & df_pa_CAT$R0_avg_alb  < 3, "[2,3)",
#                                           ifelse(df_pa_CAT$R0_avg_alb  >= 3 & df_pa_CAT$R0_avg_alb  < 4, "[3,4)",
#                                                  ifelse(df_pa_CAT$R0_avg_alb  >= 4 & df_pa_CAT$R0_avg_alb  < 5, "[4,5)",
#                                                         ifelse(df_pa_CAT$R0_avg_alb  >= 5 & df_pa_CAT$R0_avg_alb  < 6, "[5,6]",">6") )))))
#   df_r_1 <- df_pa_CAT[which(df_pa_CAT$PA == 1),] %>% 
#     group_by(range) %>% summarize(num_1 = n())
#   df_r_0 <- df_pa_CAT[which(df_pa_CAT$PA == 0),] %>% 
#     group_by(range) %>% summarize(num_0 = n())
#   
#   df_r <- df_r_1 %>% left_join(df_r_0)
#   df_r$prop_1 <- df_r$num_1/(df_r$num_1+df_r$num_0)
#   return(df_r)
#   
# }

###### Sum number of months ###
df_num_months <- function(df_pa_CAT){
  df_r_1 <- df_pa_CAT[which(df_pa_CAT$PA == 1),] %>% 
    group_by(R0_sum_alb) %>% summarize(num_1 = n())
  df_r_0 <- df_pa_CAT[which(df_pa_CAT$PA == 0),] %>% 
    group_by(R0_sum_alb) %>% summarize(num_0 = n())
  
  df_r <- df_r_1 %>% left_join(df_r_0)
  df_r$prop_1 <- ifelse(is.na(df_r$num_0),1,df_r$num_1/(df_r$num_1+df_r$num_0))
  return(df_r)
  
}

df_avg_months <- function(df_pa_CAT){
  df_r_1 <- df_pa_CAT[which(df_pa_CAT$PA == 1),] %>% 
    group_by(R0_rang_alb) %>% summarize(num_1 = n())
  df_r_0 <- df_pa_CAT[which(df_pa_CAT$PA == 0),] %>% 
    group_by(R0_rang_alb) %>% summarize(num_0 = n())
  
  df_r <- df_r_1 %>% left_join(df_r_0)
  df_r$prop_1 <- ifelse(is.na(df_r$num_0),1,
                        df_r$num_1/(df_r$num_1+df_r$num_0))
  return(df_r)
}

## Compute the plots
df_pa <- df_pa[, c("NATCODE", "PA")]
df_pa$geometry <- NULL

###------------------SUM MONTHS------------------#
df_group_m <- df_group_tot[,c("NATCODE", "R0_sum_alb")]
df_pa <- df_pa %>% left_join(df_group_m)

unique(esp_can$ine.ccaa.name)

plot_sum_p <- function(ccaa){
  NATCODE_CAT <- esp_can[which(esp_can$ine.ccaa.name == ccaa),"NATCODE"]
  NATCODE_CAT$geometry <- NULL
  df_pa_CAT <- df_pa[which(as.numeric(df_pa$NATCODE) %in% 
                             as.numeric(NATCODE_CAT$NATCODE)),]
  
  df_sum_CAT <- df_num_months(df_pa_CAT)
  df_sum_CAT$ccaa <- ccaa
  
  ggplot(df_sum_CAT) +
    geom_point(aes(R0_sum_alb,prop_1)) +
    xlab("R0 nº months") + 
    ylab("Proportion of municipalities with presence") +
    ylim(c(0,1)) +
    ggtitle(ccaa) +  
    theme_bw()
}

ccaa = "Cataluña"
plot_sum_p(ccaa)
ccaa = "Comunitat Valenciana"
plot_sum_p(ccaa)
ccaa = "País Vasco"
plot_sum_p(ccaa)

####------------------Average R0--------------##
df_group_m <- df_group_tot[,c("NATCODE", "R0_an_alb")]
df_pa <- df_pa %>% left_join(df_group_m)
df_pa$R0_avg_alb <- df_pa$R0_an_alb

plot_avg_p <- function(ccaa){
  NATCODE_CAT <- esp_can[which(esp_can$ine.ccaa.name == ccaa),"NATCODE"]
  NATCODE_CAT$geometry <- NULL
  df_pa_CAT <- df_pa[which(as.numeric(df_pa$NATCODE) %in% 
                             as.numeric(NATCODE_CAT$NATCODE)),]
  
  range(df_pa_CAT$R0_an_alb)
  df_pa_CAT$R0_rang_alb <- ifelse(df_pa_CAT$R0_an_alb < 0.5, 0, 
                           ifelse(df_pa_CAT$R0_an_alb < 0.7, 1,
                           ifelse(df_pa_CAT$R0_an_alb < 1, 2,
                           ifelse(df_pa_CAT$R0_an_alb < 1.2, 3,
                           ifelse(df_pa_CAT$R0_an_alb < 1.5, 4,
                           ifelse(df_pa_CAT$R0_an_alb < 1.7, 5,6))))))
  
  df_sum_CAT <- df_avg_months(df_pa_CAT)
  df_sum_CAT$ccaa <- ccaa
  
  ggplot(df_sum_CAT) +
    geom_point(aes(R0_rang_alb,prop_1)) +
    xlab("R0 Average monthly year") + 
    ylab("Proportion of municipalities with presence") +
    ylim(c(0,1)) +
    ggtitle(ccaa) +  
    theme_bw()
}

unique(esp_can$ine.ccaa.name)
ccaa = "Cataluña"
plot_avg_p(ccaa)
ccaa = "Comunitat Valenciana"
plot_avg_p(ccaa)
ccaa = "País Vasco"
plot_avg_p(ccaa)

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
