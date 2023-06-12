rm(list=ls())
.rs.restartR()
library("mapSpain")
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(gganimate)
library(viridis)
library("data.table")

Path = "~/INVASIBILITY_THRESHOLD/data/PresenceAbsence_MA_BG2.Rds"
df_pa <- readRDS(Path)

esp_can <- esp_get_munic_siane(moveCAN = TRUE)
# Check how to form NATCODE:
esp_can_filt <- esp_can[,c(1,2,3,4)]
esp_can_filt$geometry <- NULL
unique(esp_can_filt)

can_box <- esp_get_can_box()

### Read RDS with R0
Path <- "~/INVASIBILITY_THRESHOLD/output/weather/Daily/R0/R0_aemet_weather_year_2_22.Rds"
weather_R0 <- readRDS(Path)
weather_R0$bool <- ifelse(weather_R0$R0_tmed >1,1,0)
weather_R0 <- weather_R0 %>% group_by(name) %>%
  summarise(sum_month = sum(weather_R0$bool), avg_R0med = mean(R0_tmed), 
                avg_R0min = mean(R0_tmin), avg_R0max = mean(R0_tmax))

esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()

# Check how to form NATCODE:
unique(esp_can_filt)
nr <- nrow(esp_can)
esp_can$dist <- 0
for(i in c(1:nr)){
  esp_can$dist[i] <- st_distance(esp_can[which(esp_can$name == "Sant Cugat del Vallès"),],esp_can[i,])
}

ggplot(esp_can) +
  geom_sf(aes(fill = dist), size = 0.1) +
  scale_fill_viridis(name = "distance") +
  geom_sf(data = can_box) + theme_bw()

esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE))
# esp_can <- esp_can[-(which(is.na(esp_can$name))),]
df_pa$NATCODE <- as.numeric(df_pa$NATCODE)

df_pa$year_first <- 0
df_pa$method <- "none"
for(i in c(1:nrow(df_pa))){
  if(df_pa$A_PRIM_DET_OFICIAL[i] == 0 & df_pa$A_PRIM_DET_CITSCI[i] == 0){
    df_pa$year_first[i] <- 0
    df_pa$method[i] <- "none"
  }else if(df_pa$A_PRIM_DET_OFICIAL[i] == 0 & df_pa$A_PRIM_DET_CITSCI[i] > 0){
    df_pa$year_first[i] <- df_pa$A_PRIM_DET_CITSCI[i]
    df_pa$method[i] <- "MA"
  }else if(df_pa$A_PRIM_DET_CITSCI[i] == 0 & df_pa$A_PRIM_DET_OFICIAL[i] > 0){
    df_pa$year_first[i] <- df_pa$A_PRIM_DET_OFICIAL[i]
    df_pa$method[i] <- "Trap"
  }else if(df_pa$A_PRIM_DET_CITSCI[i] > 0 & df_pa$A_PRIM_DET_OFICIAL[i] > 0 & df_pa$A_PRIM_DET_CITSCI[i] >  df_pa$A_PRIM_DET_OFICIAL[i]){
    df_pa$year_first[i] <- df_pa$A_PRIM_DET_OFICIAL[i]
    df_pa$method[i] <- "MA"
  }else if(df_pa$A_PRIM_DET_CITSCI[i] > 0 & df_pa$A_PRIM_DET_OFICIAL[i] > 0 & df_pa$A_PRIM_DET_CITSCI[i] < df_pa$A_PRIM_DET_OFICIAL[i]){
    df_pa$year_first[i] <- df_pa$A_PRIM_DET_CITSCI[i]
    df_pa$method[i] <- "Trap"
  }
  
}

# df_join <- esp_can %>% left_join(df_pa)
# ggplot(df_join) +
#   geom_sf(aes(fill = factor(method)), size = 0.01) + 
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) + 
#   theme_bw() +
#   theme(legend.text=element_text(size=15),
#         legend.position = c(0.13,0.8)) + 
#   scale_fill_manual(values = c("#ffeda0", "#B60808", "#B60808"), name = "",
#                     labels = c("None", "Traps","MA")) +
#   labs(title = "Year: {current_frame}") +
#   transition_manual(year_first)
# df_join$pa[which(is.na(df_join$pa))] <- 0
# 
# df_join$pa <- ifelse((df_join$A_PRIM_DET_OFICIAL == 0 & df_join$A_PRIM_DET_CITSCI == 0),0,1)
# df_join$pa[which(is.na(df_join$pa))] <- 0
# 
# ggplot(df_join) +
#   geom_sf(aes(fill = factor(pa)), size = 0.1) +
#   geom_sf(data = can_box) + theme_bw()
# 
# df_pa_filt <- df_join[,c("NATCODE","pa","year_first")]
# df_pa_2004 <- data.frame(df_pa_filt$NATCODE, pa = 0, year_first = 2004)
# for(i in c(2005:2022)){
#   df_pa_2005 <- data.frame(df_pa_filt$NATCODE, pa = 0, year_first = i)
#   
#   df_pa_2004 <- rbind(df_pa_2004, df_pa_2005)
# }
# df_pa_filt$geometry <- NULL
# colnames(df_pa_2004) <- colnames(df_pa_filt)
# df_pa_filt <- rbind(df_pa_filt,df_pa_2004)
# df_pa_filt <- df_pa_filt[-which(df_pa_filt$year == 0),]
# df_pa_filt <- df_pa_filt %>% group_by(NATCODE,year_first) %>%
#   summarise(year_first = mean(year_first), pa = max(pa))
# 
# filt <- df_pa_filt[which(df_pa_filt$pa == 1),c("NATCODE","year_first")]
# for(i in c(1:nrow(filt))){
#   df_pa_filt[which(df_pa_filt$NATCODE == as.numeric(filt[i,"NATCODE"]) & 
#                     df_pa_filt$year_first >  as.numeric(filt[i,"year_first"])),"pa"] = 1
# }
# 
# df_pa_filt <- esp_can %>% left_join(df_pa_filt)
# ggplot(df_pa_filt) +
#   geom_sf(aes(fill = factor(pa)), size = 0.01) + 
#   scale_fill_manual(values = c("#ffeda0", "#B60808"), name = "",
#                     labels = c("Absence", "Presence")) +
#   geom_sf(data = can_box) + coord_sf(datum = NA) +
#   theme_bw() +
#   theme(legend.text=element_text(size=15),
#         plot.title = element_text(size=15)) + 
#   labs(title = "Year: {current_frame}") +
#   transition_manual(year_first)
# 
# anim_save("~/Documentos/PHD/2023/INVASIBILITY/Plots/map_PA.gif",
#           animation = last_animation())

# Presence basence gidt diferenciated 
df_join$method[which(is.na(df_join$method))] <- "None"

df_join$pa <- ifelse((df_join$A_PRIM_DET_OFICIAL == 0 & df_join$A_PRIM_DET_CITSCI == 0),0,1)
df_join$pa[which(is.na(df_join$pa))] <- 0

ggplot(df_join) +
  geom_sf(aes(fill = factor(pa)), size = 0.1) +
  geom_sf(data = can_box) + theme_bw()

df_pa_filt <- df_join[,c("NATCODE","method","year_first")]
df_pa_2004 <- data.frame(NATCODE = df_pa_filt$NATCODE, method = "Absence", year_first = 2004)
df_pa_filt_2004 <- df_pa_filt[which(df_pa_filt$year_first==2004),]
df_pa_2004 <- df_pa_2004 %>% left_join(df_pa_filt_2004, by="NATCODE")
df_pa_2004$method <- ifelse(is.na(df_pa_2004$method.y)== FALSE, df_pa_2004$method.y, "Absence" )
for(i in c(2005:2022)){
  df_pa_2005 <- data.frame(NATCODE = df_pa_filt$NATCODE, method = "Absence", year_first = i)
  df_pa_filt_2005 <- df_pa_filt[which(df_pa_filt$year_first==i),]
  df_pa_2005 <- df_pa_2005 %>% left_join(df_pa_filt_2005, by="NATCODE")
  df_pa_2005$method <- ifelse(is.na(df_pa_2005$method.y)== FALSE, df_pa_2005$method.y, "Absence")
  df_pa_2004 <- rbind(df_pa_2004, df_pa_2005)
}

df_pa_2004$geometry <- NULL
df_plot <- esp_can %>% left_join(df_pa_2004)
ggplot(df_plot) +
  geom_sf(aes(fill = factor(method.x)), size = 0.01) + 
  scale_fill_manual(values = c("#ffeda0", "#B60808", "#B60808"), name = "",
                    labels = c("Absence", "MA","Traps")) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme_bw() +
  theme(legend.text=element_text(size=15),
        plot.title = element_text(size=15)) + 
  labs(title = "Year: {current_frame}") +
  transition_manual(year_first.x)

anim_save("~/Documentos/PHD/2023/INVASIBILITY/Plots/map_PA.gif",
          animation = last_animation())

df_pa_filt$geometry <- NULL
colnames(df_pa_2004) <- colnames(df_pa_filt)
df_pa_filt <- rbind(df_pa_filt,df_pa_2004)
df_pa_filt <- df_pa_filt[-which(df_pa_filt$year == 0),]


## Comparison between R0 and presence absence
df_R0_PA <- weather_R0 %>% left_join(df_join)
df_R0_PA <- df_R0_PA[-which(is.na(df_R0_PA)),]

ggplot(df_R0_PA) + 
  geom_point(aes(avg_R0min, pa), size = 0.1) + 
  theme_bw()

ggplot(df_R0_PA) + 
  geom_point(aes(avg_R0med, pa), size = 0.1) + 
  theme_bw()

ggplot(df_R0_PA) + 
  geom_point(aes(avg_R0max, pa), size = 0.1) + 
  theme_bw()

ggplot(df_R0_PA) + 
  geom_point(aes(dist, pa), size = 0.1) + 
  theme_bw()




df_pa_filt <- df_pa[, c(1,5)]
# df_pa_filt$year <- df_pa_filt$year_first
# df_pa_filt <- reshape(df_pa_filt, idvar = "year_first", timevar = "year", direction = "wide")
# df_pa_filt[is.na(df_pa_filt)] <- 0
df_bar <- unique(df_join[which(df_join$name == "Barcelona" | df_join$name %like% "Sant Adrià de Besòs"),])
plot(df_bar)
st_touches(df_join)
df_join <- esp_can %>% left_join(df_pa_filt)

## Compute jumps in colonization
df_join <- df_join[,c(6,10,11)]
list_2004 <- df_join[which(df_join$year_first == 2004), ]
df_jump <- data.frame(year = numeric(0), num_jumps = numeric(0))
df_join <- df_join[which(df_join$year_first >0),]
min_year = min(df_join$year_first) 
max_year = max(df_join$year_first) - 1
diff <- max_year - min_year 
for(i in c(1:diff)){
  list_2004 <- df_join[which(df_join$year_first == (min_year + i - 1) & df_join$year_first > (min_year + i - 2)), ] 
  list_2005 <- df_join[which(df_join$year_first == (min_year + i) & df_join$year_first > (min_year + i - 1)), ] 
  # plot(list_2005[,c(1,3)])
  touches <- st_touches(list_2004, list_2005)
  list_touch <- unique(unlist(st_touches(list_2005[-unique(unlist(touches)),])))
  if(length(list_touch) > 0){
    num_jumps <- nrow(list_2005) - length(unique(unlist(touches))) - list_touch
  }else{
    num_jumps <- nrow(list_2005) - length(unique(unlist(touches)))
  }
 
  df_jump[i,] <- c((min_year + i),num_jumps )
}

ggplot(df_jump) + 
  geom_line(aes(year,num_jumps)) + 
  ylab("Number of municipalities") + 
  theme_bw() +
  theme(text = element_text(size = 15))

# # Check if the df is well computed:
# list_2004 <- df_join[which(df_join$year_first ==  2005 & df_join$year_first > 2004), ] 
# list_2005 <- df_join[which(df_join$year_first == 2006 & df_join$year_first > 2005), ] 
# # plot(list_2005[,c(1,3)])
# touches <- st_touches(list_2004, list_2005)
# num_jumps <- nrow(list_2005) - length(unique(unlist(touches)))
# plot(rbind(list_2005[,c(2,3)],list_2004[,c(2,3)]))

muni_mat <- readRDS(file = "/home/marta/INVASIBILITY_THRESHOLD/data/mob/output/output_mat_mob_muni.Rds")
muni_incoming <- data.frame(LAU_CODE = colnames(muni_mat), inc_travel = rowSums(muni_mat))
muni_incoming$inc_relative <- muni_incoming$inc_travel/max(muni_incoming$inc_travel)
muni_ine_mitma <- read.csv("~/INVASIBILITY_THRESHOLD/data/mob/relacion_ine_zonificacionMitma.csv", sep = "|")
muni_ine_mitma <- muni_ine_mitma[,c(1,3,5)]
colnames(muni_ine_mitma)[3] <- "LAU_CODE" 
muni_incoming <- muni_incoming %>% left_join(muni_ine_mitma)


esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE)
esp_can$LAU_CODE <- as.numeric(esp_can$LAU_CODE)
muni_incoming$LAU_CODE <- as.numeric(muni_incoming$municipio_ine)

muni_incoming_plot <- esp_can %>% left_join(muni_incoming)
ggplot(muni_incoming_plot) +
  geom_sf(aes(fill = inc_relative), size = 0.1) +
  geom_sf(data = can_box) + theme_bw()


