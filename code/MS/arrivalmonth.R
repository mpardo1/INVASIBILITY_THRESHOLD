path <- "~/INVASIBILITY_THRESHOLD/data/PA/PresenceAbsence_MA_BG.csv"
df_pa <- read.csv(path)


esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
esp_can$geometry <- NULL

df_pa <- df_pa %>% left_join(esp_can[,c("NATCODE", "ine.ccaa.name", "name")])
unique(df_pa$ine.ccaa.name)
unique(df_pa[which(df_pa$ine.ccaa.name == "País Vasco"), "QUIEN.FUE.ANTES" ])
unique(df_pa[which(df_pa$ine.ccaa.name == "Aragón"), "QUIEN.FUE.ANTES" ])
unique(df_pa[which(df_pa$ine.ccaa.name == "Madrid, Comunidad de"), "QUIEN.FUE.ANTES" ])
unique(df_pa[which(df_pa$ine.ccaa.name == "Andalucía"), "QUIEN.FUE.ANTES" ])
