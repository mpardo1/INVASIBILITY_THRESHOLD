library(mapSpain)
library(tidyverse)
path <- "/home/marta/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/R0_clim_2020.Rds"
df2020 <- readRDS(path)
df2020 <- df2020[, c(1:4,8,20)]
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/catu_R0_clim_",
               year,".Rds")
saveRDS(df2020, Path)
# Esp can shapefile --------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df2020 <- esp_can %>% left_join(df2020, by = "NATCODE")
ggplot(df2020) + 
  geom_sf(aes(fill=R0_anual_alb), color = NA) +
  scale_fill_distiller(palette = "Spectral") + theme_bw()

unique(df2020$ine.ccaa.name)
df_aux <- df2020[which(df2020$ine.ccaa.name == "Cataluña"),"R0_sum_alb"]
