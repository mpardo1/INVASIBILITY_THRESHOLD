# Compute map with diff between years
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

# Load data weather ----------------------------------------------------------
year = 2004
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/R0_clim_",
               year,".Rds")
df_2004 <- setDT(readRDS(Path))
df_2004 <- df_2004[,c("NATCODE", "R0_sum_alb", "R0_sum_aeg", "R0_sum_jap")]
colnames(df_2004) <-c ("NATCODE", "Alb_2004", "Aeg_2004", "Jap_2004")

year = 2020
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/R0_clim_",
               year,".Rds")
df_2020 <- setDT(readRDS(Path))
df_2020 <- df_2020[,c("NATCODE", "R0_sum_alb", "R0_sum_aeg", "R0_sum_jap")]
colnames(df_2020) <-c ("NATCODE", "Alb_2020", "Aeg_2020", "Jap_2020")

year = 2040
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/clim_",
               year,".Rds")
df_2040 <- setDT(readRDS(Path))
df_2040 <- df_2040[,c("NATCODE", "alb", "aeg", "jap")]
colnames(df_2040) <-c ("NATCODE", "Alb_2040", "Aeg_2040", "Jap_2040")

# Map Spain municipalities ----------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
df_group$month <- lubridate::month(df_group$date)

# Plot maps ------------------------------------------------------------
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
  df <- esp_can %>% left_join(df)
  num_colors <- 13
  # Create a palette function using colorRampPalette
  my_palette <- colorRampPalette(c("#faf0ca","#f95738", "#732c2c"))
  
  colors <- c("#43A2CA", "#7BCCC4", "#BAE4BC", "#F0F9E8",
              "#FFF7EC","#FEE8C8","#FDD49E","#FDBB84",
              "#FC8D59","#EF6548","#D7301F", "#B30000",
              "#7F0000") 
  ggplot(df) +
    geom_sf(aes(fill = as.factor(R0)),
            colour = NA) +
    geom_sf(data = can_box) +
    coord_sf(datum = NA) +
    scale_fill_manual(values = pal,
                      name = "Nº months\n suitable",
                      limits = factor(seq(0,12,1))) +
    theme_bw()  +
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.direction = "horizontal")
}

# Albopictus ---------------------------------------------------------
# 2004
df_2004$R0 <- df_2004$Jap_2004
plot_2004 <- plot_summonths(df_2004)
plot_2004
df_2004$R0 <- NULL

# 2020
df_2020$R0 <- df_2020$Jap_2020
plot_2020 <- plot_summonths(df_2020)
plot_2020
df_2020$R0 <- NULL

# 2040
df_2040$R0 <- df_2040$Jap_2040
plot_2040 <- plot_summonths(df_2040)
plot_2040
df_2040$R0 <- NULL

# Join all the plots -------------------------------------------------
library(ggpubr)
ggarrange(plot_2004 + ggtitle("2004"),
          plot_2020 + ggtitle("2020"),
          plot_2040 + ggtitle("2040"),
          ncol = 3,
          common.legend = TRUE)

# Compute diff years -------------------------------------------------
df_join <- df_2004 %>% 
  left_join(df_2020) %>% 
  left_join(df_2040)

df_join$diff_0420 <- df_join$Alb_2020 - df_join$Alb_2004
df_join$diff_2040 <- df_join$Alb_2040 - df_join$Alb_2020
df_join$diff_0440 <- df_join$Alb_2040 - df_join$Alb_2004

# Plot map diff ------------------------------------------------------
df_join <- esp_can %>% left_join(df_join)

name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal[11]
pal[12] = "#74011C"
pal[13] = "#4B0011"

plot_1 <- ggplot(df_join) +
  geom_sf(aes(fill = as.factor(diff_0420)), colour = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_manual(values = pal,
                       name = "Difference \n in months",
                       limits = c(min(df_join$diff_0420,
                                      df_join$diff_2040):max(df_join$diff_0420,
                                                             df_join$diff_2040))) +
  ggtitle("Difference 2020 and 2004") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "horizontal")

plot_2 <- ggplot(df_join) +
  geom_sf(aes(fill = as.factor(diff_2040)), colour = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_manual(values = pal,
                       name = "Difference \n in months",
                      limits = c(min(df_join$diff_0420,
                                   df_join$diff_2040):max(df_join$diff_0420,
                                                          df_join$diff_2040))) +
  ggtitle("Difference 2040 and 2020") +
  theme_bw() 

# Panel join all plots ------------------------------------------------------
ggarrange(plot_1,plot_2, ncol = 2, common.legend = TRUE)

plot_3 <- ggplot(df_join) +
  geom_sf(aes(fill = as.factor(diff_0440)), colour = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_manual(values = pal1,
                       name = "Difference \n in months",
                       limits = c(-7:5)) +
  ggtitle("Difference 2040 and 2004") +
  theme_bw() 
