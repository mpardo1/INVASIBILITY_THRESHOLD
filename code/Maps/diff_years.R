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
# this comes from maps_mcera5_group.R
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

# this comes from future_climate/future_climate.R
year = 2040
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/clim_",
               year,".Rds")
df_2040 <- setDT(readRDS(Path))
df_2040 <- df_2040[,c("NATCODE", "alb", "aeg", "jap")]
colnames(df_2040) <-c ("NATCODE", "Alb_2040", "Aeg_2040", "Jap_2040")

year = 2060
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/clim_2060.Rds")
df_2060 <- setDT(readRDS(Path))
df_2060 <- df_2060[,c("NATCODE", "alb", "aeg", "jap")]
colnames(df_2060) <-c ("NATCODE", "Alb_2060", "Aeg_2060", "Jap_2060")

dataset = 'ACCESS-CM2'
path_dir <-'tmpr_370'
ssp = '370'
time = '2041-2060'

# this comes from europe_map.R
clim_df <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/output/summon_eu_alb_",
                          time,"_mo_",dataset,"_",ssp,".Rds"))

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
pal[12] = "#74011C"
pal[13] = "#4B0011"
letsize = 16
plot_summonths <- function(df){
  df <- esp_can %>% left_join(df)
  ggplot(df) +
    geom_sf(aes(fill = as.factor(R0)),
            colour = NA) +
    geom_sf(data = can_box) +
    coord_sf(datum = NA) +
    scale_fill_manual(values = pal,
                      name = "Nº months\n suitable",
                      limits = factor(seq(0,12,1))) +
    theme_minimal()  +
    theme(legend.position = "none",
          legend.text = element_text(14)) 
}

# Albopictus ---------------------------------------------------------
# 2004
df_2004$R0 <- df_2004$Alb_2004
plot_2004 <- plot_summonths(df_2004)
plot_2004
df_2004$R0 <- NULL

# 2020
df_2020$R0 <- df_2020$Alb_2020
plot_2020 <- plot_summonths(df_2020)
plot_2020
df_2020$R0 <- NULL

# 2040
df_2040$R0 <- df_2040$Alb_2040
plot_2040 <- plot_summonths(df_2040)
plot_2040
df_2040$R0 <- NULL

# 2060
df_2060$R0 <- df_2060$Alb_2060
plot_2060 <- plot_summonths(df_2060)
plot_2060
df_2060$R0 <- NULL

# Join all the plots -------------------------------------------------
library(ggpubr)
ggarrange(plot_2004 +
            ggtitle("A                        2004"),
          plot_2020 +
            ggtitle("B                         2020"),
          plot_2040 +
            ggtitle("C                         2040"),
          ncol = 3,
          common.legend = TRUE)

esp <- ggarrange(plot_2004 +
            ggtitle("A            2004"),
          plot_2020 +
            ggtitle("B            2020"),
          plot_2040 +
            ggtitle("C          2041-2060"),
          plot_2060 +
            ggtitle("D           2061-2080"),
          ncol = 2,nrow = 2,
          common.legend = TRUE,
          legend = "left")

# maps with europe -------------------------------------------------
alb <- ggplot(clim_df) +
  geom_tile(aes(x = lon, y = lat, 
                fill = as.factor(sum_alb)),alpha = 1) +
  scale_fill_manual(values = pal,
                    name = "Nº months\n suitable",
                    limits = factor(seq(0,12,1)),
                    na.value = "white") +
  ylim(c(25,75)) + xlim(c(-30,40)) +
  xlab("") + ylab("") +
  theme_minimal()  +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks.length = unit(0, "null"),
        axis.ticks.margin = unit(0, "null")) +
  guides(fill = guide_legend(nrow = 1),
         label.position = "none")

leg_alb <- get_legend(alb)
as_ggplot(leg_alb)

ggarrange(esp, alb, ncol = 2,
          heights = c(1.7,2),
          legend = "none")

# 3 species ---------------------------------------------------------
# Albopictus
df_2020$R0 <- df_2020$Alb_2020
plot_2004 <- plot_summonths(df_2020)
plot_2004
df_2004$R0 <- NULL

# Aegypti
df_2020$R0 <- df_2020$Aeg_2020
plot_2020 <- plot_summonths(df_2020)
plot_2020
df_2020$R0 <- NULL

# Japonicus
df_2020$R0 <- df_2020$Jap_2020
plot_2040 <- plot_summonths(df_2020)
plot_2040
df_2040$R0 <- NULL

# Join all the plots -------------------------------------------------
library(ggpubr)
ggarrange(plot_2004 +
            ggtitle(expression(paste("A               ", italic("Ae. albopictus")))),
          plot_2020 +
            ggtitle(expression(paste("B               ", italic("Ae. aegypti")))),
          plot_2040 +
            ggtitle(expression(paste("C               ", italic("Ae. japonicus")))),
          ncol = 3,
          common.legend = TRUE)
plot_2060
ggsave("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/Maps/Aeg_2004_2020_2040.pdf",
       width = 8, height = 4)

# Compute diff years -------------------------------------------------
df_join <- df_2004 %>% 
  left_join(df_2020) %>% 
  left_join(df_2040) %>% 
  left_join(df_2060)

df_join$diff_0420 <- df_join$Alb_2020 - df_join$Alb_2004
df_join$diff_2040 <- df_join$Alb_2040 - df_join$Alb_2020
df_join$diff_0440 <- df_join$Alb_2040 - df_join$Alb_2004
df_join$diff_8040 <- df_join$Alb_2060 - df_join$Alb_2040
df_join$diff_2080 <- df_join$Alb_2060 - df_join$Alb_2020
df_join$diff_0480 <- df_join$Alb_2060 - df_join$Alb_2004

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
  scale_fill_manual(values = pal,
                       name = "Difference \n in months",
                       limits = c(-7:5)) +
  ggtitle("Difference 2040 and 2004") +
  theme_bw() 

plot_4 <- ggplot(df_join) +
  geom_sf(aes(fill = as.factor(diff_8040)), colour = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_manual(values = pal,
                    name = "Difference \n in months",
                    limits = c(-7:5)) +
  ggtitle("Difference 2080 and 2040") +
  theme_bw() 

plot_5 <- ggplot(df_join) +
  geom_sf(aes(fill = as.factor(diff_2080)), colour = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_manual(values = pal,
                    name = "Difference \n in months",
                    limits = c(-7:5)) +
  ggtitle("Difference 2080 and 2020") +
  theme_bw() 

plot_5 <- ggplot(df_join) +
  geom_sf(aes(fill = as.factor(diff_0480)), colour = NA) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  scale_fill_manual(values = pal,
                    name = "Difference \n in months",
                    limits = c(-7:5)) +
  ggtitle("Difference 2080 and 2004") +
  theme_bw() 

ggarrange(plot_4,plot_5, ncol = 2, common.legend = TRUE)
