rm(list=ls())
library(mapSpain)
library(dplyr)

# read pa albopictus data ----------------------------------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/PA/PresenceAbsence_MA_BG.csv"
df_pa <- read.csv(path,sep = ",", header= TRUE)

# spain municipalities data --------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
esp_can$geometry <- NULL

# join to data frames --------------------------------------------------------
esp_can <- esp_can %>% left_join(df_pa)
val <- esp_can[esp_can$ine.ccaa.name == "Comunitat Valenciana",]
unique(val$QUIEN.FUE.ANTES)
