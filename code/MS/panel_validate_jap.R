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

# Load Data --------------------------------------------------------------------
## year = 2022, we will use this year to validate the data
year = 2020
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/R0_clim_",
               year,".Rds")
df_group_tot <- readRDS(Path)
head(df_group_tot)

## Data Catu BG traps
Path <- "~/INVASIBILITY_THRESHOLD/data/Datos_Catu/gi_min_model_pred.RData"
load(Path)
unique(gi_min_model_pred$trap_name)

# Presence absence data Japonicus Spain:
Path <- "~/INVASIBILITY_THRESHOLD/data/PA/MUNS_ANYS-japonicus.csv"
PA_jap <- read.csv(Path)
PA_jap$A_PRIM_DET_OFICIAL <- ifelse( PA_jap$A_PRIM_DET_OFICIAL == "#N/A", 0, PA_jap$A_PRIM_DET_OFICIAL)
PA_jap$A_PRIM_DET_CITSCI <- ifelse( PA_jap$A_PRIM_DET_CITSCI == "#N/A", 0, PA_jap$A_PRIM_DET_CITSCI)

PA_jap$Japonicus <- ifelse(PA_jap$A_PRIM_DET_OFICIAL == 0 & PA_jap$A_PRIM_DET_CITSCI == 0, 0,1)

# Map Spain --------------------------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",
                                     esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
can_box <- esp_get_can_box()
PA_jap_ESP <- esp_can %>% left_join(PA_jap)
PA_jap_ESP$Japonicus <- ifelse(is.na(PA_jap_ESP$Japonicus),0,PA_jap_ESP$Japonicus)
pa_jap <- ggplot(PA_jap_ESP) + 
  geom_sf(aes(fill = as.factor(Japonicus)), linewidth = 0.05, color = NA)  +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme_minimal() +
  scale_fill_manual(values = c("#D9EAF8","#377EB8"), name = " ") 

### Comparison between presence absence and number of months R0>1
PA_jap_ESP <- PA_jap_ESP[, c("NATCODE", "Japonicus", "ine.ccaa.name")]
df_group_m <- df_group_tot[,c("NATCODE",
                              "R0_sum_jap",
                              "R0_sum_jap_min",
                              "R0_sum_jap_max")]
df_group_m$geometry <- NULL
PA_jap_ESP <- PA_jap_ESP %>% left_join(df_group_m)

# Proportion of municipalities presence ----------------------------------------
# Function to create df number months suitable vs proportion presence
df_num_months <- function(df_pa_CAT){
  df_r_1 <- df_pa_CAT[which(df_pa_CAT$Japonicus == 1),] %>% 
    group_by(R0_sum_jap) %>% summarize(num_1 = n())
  df_r_0 <- df_pa_CAT[which(df_pa_CAT$Japonicus == 0),] %>% 
    group_by(R0_sum_jap) %>% summarize(num_0 = n())
  
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

# Create plots for different regions -------------------------------------------
# Function to create the plot sum months
plot_sum_p <- function(ccaa){
  NATCODE_CAT <- esp_can[which(esp_can$ine.ccaa.name == ccaa),"NATCODE"]
  NATCODE_CAT$geometry <- NULL
  df_pa_CAT <- PA_jap_ESP[which(as.numeric(PA_jap_ESP$NATCODE) %in% 
                             as.numeric(NATCODE_CAT$NATCODE)),]
  
  df_sum_CAT <- df_num_months(df_pa_CAT)
  df_sum_CAT$ccaa <- ccaa
  
  ggplot(df_sum_CAT) +
    geom_point(aes(R0_sum_jap,prop_1)) +
    xlab("Nº months\n suitable") + 
    ylab("Proportion of municipalities with presence") +
    ylim(c(0,1)) +
    ggtitle(ccaa) +  
    theme_bw()
}

# Filter Data frames -------------------------------------------------------------
esp_can_ccaa <- esp_can[,c("NATCODE", "ine.ccaa.name")]
colors <- c("#43A2CA", "#7BCCC4", "#BAE4BC", "#F0F9E8",
            "#FFF7EC","#FEE8C8","#FDD49E","#FDBB84",
            "#FC8D59","#EF6548","#D7301F", "#B30000",
            "#7F0000")

# Plot the specific region coloured --------------------------------------------
ccaa = "Rioja, La" #"Asturias, Principado de" #"Cantabria"
ggplot(PA_jap_ESP[which(PA_jap_ESP$ine.ccaa.name == ccaa),]) +
  geom_sf(aes(fill = as.factor(R0_sum_jap)), colour = NA) +
  coord_sf(datum = NA) + 
  scale_fill_manual(values = colors,
                    name = "Nº months\n suitable",
                    limits = factor(seq(0,12,1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot num months vs prop PA ---------------------------------------------------
PA_jap_ESP$geometry <- NULL
unique(PA_jap_ESP[which(PA_jap_ESP$Japonicus == 1),"ine.ccaa.name"])

# Choose ccaa
ccaa = "Cantabria"
ccaa = "País Vasco"
ccaa = "Cantabria"
ccaa = "Asturias, Principado de"
ccaa = "País Vasco"

plot <- plot_sum_p(ccaa)
plot
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/JAP_PA_SUM_",ccaa,".png")
ggsave(Path, plot = plot)

# Plot multiple ccaa together ------------------------------------------------
# Join more than one ccaa ---------------------------------------------
list_ccaa = c("Cantabria","País Vasco",
              "Cantabria","Asturias, Principado de",
              "País Vasco")

NATCODE_CAT <- esp_can[which(esp_can$ine.ccaa.name %in% list_ccaa ),"NATCODE"]

# Add PA data --------------------------------------------------------
NATCODE_CAT$geometry <- NULL
df_pa_CAT <- PA_jap_ESP[which(as.numeric(PA_jap_ESP$NATCODE) %in% 
                           as.numeric(NATCODE_CAT$NATCODE)),]

# Compute df with prop PA --------------------------------------------
df_sum_CAT <- data.frame()
for(i in c(1:length(list_ccaa))){
  df_aux <- df_num_months(df_pa_CAT[which(df_pa_CAT$ine.ccaa.name == list_ccaa[i] ),])
  df_aux$ccaa_n <- list_ccaa[i]
  df_sum_CAT <- rbind(df_aux,df_sum_CAT)
}

# Plot color related to ccaa ----------------------------------------
ccaa_jap <- ggplot(df_sum_CAT) +
  geom_line(aes(R0_sum_jap,prop_1, color =ccaa_n)) +
  geom_point(aes(R0_sum_jap,prop_1, color =ccaa_n,
                 size = sum_muni), alpha = 0.6) +
  xlab("Nº months suitable") + 
  ylab("Proportion of municipalities with presence") +
  ylim(c(0,1)) + 
  scale_color_discrete(name = "") +
  scale_size_continuous(name = "Number municipalities",
                        breaks = c(5,20,40,60,80),
                        labels =c("[0,5]","[5,20]",
                                  "[20,40]","[40,60]",
                                  "[60,80]")) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  theme_bw() +
  theme(legend.position = c(0.15,0.55),
        text = element_text(size = 14)) 

# Plot all thre ccaa in one line -----------------------------
df_group <- df_sum_CAT %>% group_by(R0_sum_jap) %>%
  summarise(num_1 = sum(num_1),
            num_0 = sum(num_0),
            prop_1 = sum(num_1)/(sum(num_0)+sum(num_1)),
            sum_muni = sum(sum_muni))
jap_esp <- ggplot(df_group) +
  geom_line(aes(R0_sum_jap,prop_1)) +
  geom_point(aes(R0_sum_jap,prop_1,
                 size = sum_muni), alpha = 0.6) +
  xlab("Nº months suitable") + 
  ylab("Proportion of municipalities with presence") +
  ylim(c(0,1)) + 
  scale_size_continuous(name = "Number municipalities",
                        breaks = c(5,50,100,150),
                        labels =c("[0,50]","[50,100]",
                                  "[100,150]","[150,200]")) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.position = c(0.3,0.6)) 

# european scale ----------------------------------------------------------
clim_pop_df <- readRDS("~/INVASIBILITY_THRESHOLD/data/japonicus/pa/clim_pop_df.Rds")
country_n <- unique(clim_pop_df[which(clim_pop_df$pa_jap == 1),"cntryName"])
clim_pop_filt <- clim_pop_df[which(clim_pop_df$cntryName %in% country_n),]
clim_pop_filt_g <- clim_pop_filt %>% group_by(jap) %>%
  summarise(sum_1 = sum(pa_jap), n = n())
clim_pop_filt_g$prop_pa <- clim_pop_filt_g$sum_1/clim_pop_filt_g$n
prop_pres_jap <- ggplot(clim_pop_filt_g) + 
  geom_point(aes(jap, prop_pa, size = n))+ 
  geom_line(aes(jap, prop_pa)) + 
  ylab("Proportion presence") +
  xlab( "Nº months suitable") + 
  scale_x_continuous(breaks = seq(0,12,1)) +
  theme_bw() + scale_size_continuous(name = "Nº regions",
                                     labels = c("[0,100)","[100,1000)",
                                                "[1000,2000)","[2000,3000]",">3000"),
                                     breaks = c(0,100,1000,2000,3000)) +
  theme(legend.position = c(0.85,0.7),
        text = element_text(size = 14))


# read presence absence data ------------------------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/status_2303.shp"
pa_jap <- read_sf(path)
pa_jap <- pa_jap[which(pa_jap$leave == 1),]
pa_jap$pa_jap <- ifelse(pa_jap$japonicus != "absent" & 
                          pa_jap$japonicus != "noData", 1,0)  

list_eu <- unique(pa_jap$cntryName)[c(1:2,4,6,7:8,10:14,16,19:23,25:26,28:30,32:35,39:42,
                           45:52,54:56,58:59,61,62,67,68,70,71)]
pa_jap_eu <- pa_jap[which(pa_jap$cntryName %in% list_eu),]
pa_jap_eu <- ggplot(pa_jap_eu) + 
  geom_sf(aes(fill = as.factor(pa_jap)), color = NA) +
  theme_minimal() + coord_sf(datum = NA) +
  xlim(c(-20,40)) + ylim(c(25,70)) + 
  scale_fill_manual(values = c("#D9EAF8","#377EB8"), name = " ") 
pa_jap_eu

# create panel --------------------------------------------------------
library(ggpubr)
gg1 <- ggarrange(pa_jap_eu + ggtitle("A"),jap_esp + ggtitle("B"))
ggarrange(gg1, ccaa_jap + ggtitle("C"), ncol = 1)
