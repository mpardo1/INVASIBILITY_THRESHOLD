# Code to join the plots from future and present maps
# also to compute the difference in number of months
rm(list= ls())
library(eurostat)
library(terra)
library(tidyverse)
library(data.table)
library(sf)
library(ggpubr)
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")

# Load data frames
time = "2041-2060"
clim_df_41 <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/output/summon_eu_",time,".Rds"))
colnames(clim_df_41) <- c("id","sum_alb_fut41","sum_aeg_fut41","sum_jap_fut41","lon","lat")
time = "2061-2080"
clim_df <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/output/summon_eu_",time,".Rds"))
colnames(clim_df) <- c("id","sum_alb_fut","sum_aeg_fut","sum_jap_fut","lon","lat")
clim_pop <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_R0_fitfuture_clim_",2020,".Rds"))
colnames(clim_pop) <- c("id","sum_alb_pres","sum_aeg_pres","sum_jap_pres","lon","lat")

# Plot 2020 aeg and alb
# ggplot albopictus
library(RColorBrewer)
name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal[11]
pal[12] = "#74011C"
pal[13] = "#4B0011"
alb <- ggplot(clim_pop,
              aes(x = lon, y = lat,
                  fill = as.factor(sum_alb_pres))) +
  geom_raster() +
  scale_fill_manual(values = pal,
                    name = "Nº suitable \n months",
                    limits = factor(seq(0,12,1)),
                    na.value = "white") +
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
        axis.ticks.margin = unit(0, "null"))

leg_sum <- get_legend(ggplot(clim_pop,
                             aes(x = lon, y = lat,
                                 fill = as.factor(sum_aeg_pres))) +
                        geom_raster() +
                        scale_fill_manual(values = pal,
                                          name = "Nº suitable \n months",
                                          limits = factor(seq(0,12,1)),
                                          na.value = "white")+
                        guides(fill = guide_legend(
                          ncol = 14,  # Set the number of columns
                          title.position = "left",  # Position title at the top
                          label.position = "bottom"  # Position labels at the bottom
                        )))
gg1 <- ggarrange(alb + ggtitle(expression(paste("a) ",italic("Ae. albopictus")))),
          aeg + ggtitle(expression(paste("b) ",italic("Ae. aegypti")))),
          nrow = 1)

ggarrange(gg1,leg_sum, ncol = 1, heights = c(1,0.2))

# Climat change panels ----------------------------------------------
# Join two data frames
clim_df <- clim_df %>% left_join(clim_pop, by = join_by(lon,lat))
clim_df <- clim_df %>% left_join(clim_df_41, by = join_by(lon,lat))

# Compute the difference
clim_df$diff_alb <- clim_df$sum_alb_fut - clim_df$sum_alb_pres
clim_df$diff_aeg <- clim_df$sum_aeg_fut - clim_df$sum_aeg_pres
clim_df$diff_alb41 <- clim_df$sum_alb_fut41 - clim_df$sum_alb_pres
clim_df$diff_aeg41 <- clim_df$sum_aeg_fut41 - clim_df$sum_aeg_pres
clim_df$diff_alb6141 <- clim_df$sum_alb_fut - clim_df$sum_alb_fut41
clim_df$diff_aeg6141 <- clim_df$sum_aeg_fut - clim_df$sum_aeg_fut41

# Create a palette

name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal1 <- rep("0",length(pal) +1 )
pal1[3:6] <- pal[1:4]
pal1[1] <- "#000455"
pal1[2] <- "#0C1290"
pal1[7] <- "#FFFFFF"
pal1[8:13] <- pal[6:11]
pal1[14] = "#74011C"
# pal[13] = "#4B0011"
pal2 <- pal1
pal2[15] = "black"
pal2[16] = "black"
pal2[17] = "black"
# Check raster points with negative numbers
clim_df$diff_alb_mod <- clim_df$diff_aeg
clim_df$diff_alb_mod <- ifelse(clim_df$diff_alb_mod>=3,10,clim_df$diff_alb_mod)
ggplot(clim_df,
       aes(x = lon, y = lat,
           fill = as.factor(diff_alb_mod))) +
  geom_raster() +
  scale_fill_manual(values = pal2,
                    name = "Difference in\n suitable months",
                    limits = factor(seq(-6,10,1)))

# ggplot albopictus
diff_aeg <- ggplot(clim_df,
       aes(x = lon, y = lat,
           fill = as.factor(diff_aeg))) +
  geom_raster() +
  scale_fill_manual(values = pal1,
                    name = "Difference in\n suitable months",
                    na.value = "white",
                    limits = factor(seq(-6,7,1))) +
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
        axis.ticks.margin = unit(0, "null")) 
# +
#   guides(fill = guide_legend(
#     ncol = 14,  # Set the number of columns
#     title.position = "left",  # Position title at the top
#     label.position = "bottom"  # Position labels at the bottom
#   )) 

leg <- (get_legend(ggplot(clim_df,
                            aes(x = lon, y = lat,
                                fill = as.factor(diff_aeg))) +
                       geom_raster() +
                       scale_fill_manual(values = pal1,
                                         name = "Difference in\n months",
                                         na.value = "#FAFAFA",
                                         limits = factor(seq(-6,7,1))))) 
                   # +
                   #                         guides(fill = guide_legend(
                   #                           ncol = 14,  # Set the number of columns
                   #                           title.position = "left",  # Position title at the top
                   #                           label.position = "bottom"  # Position labels at the bottom
                   #                         )) ) )
        

# Plots para el sup
# ggplot albopictus
diff_alb <- ggplot(clim_df,
                   aes(x = lon, y = lat,
                       fill = as.factor(diff_alb))) +
  geom_raster() +
  scale_fill_manual(values = pal1[c(3:4,7,10:13)],
                    name = "Difference in\n suitable months",
                    na.value = "white",
                    limits = factor(seq(-2,4,1))) +
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
        axis.ticks.margin = unit(0, "null"))  

leg <- (get_legend(ggplot(clim_df,
                          aes(x = lon, y = lat,
                              fill = as.factor(diff_aeg6141))) +
                     geom_raster() +
                     scale_fill_manual(values = pal1[c(3:4,7,10:13)],
                                       name = "Difference in\n months",
                                       na.value = "#FAFAFA",
                                       limits = factor(seq(-2,4,1))) )+
          guides(fill = guide_legend(
            ncol = 14,  # Set the number of columns
            title.position = "left",  # Position title at the top
            label.position = "bottom"  # Position labels at the bottom
          )) 
)

# ggplot albopictus
name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal[11]
pal[12] = "#74011C"
pal[13] = "#4B0011"
aeg <- ggplot(clim_df,
                   aes(x = lon, y = lat,
                       fill = as.factor(sum_aeg_fut))) +
  geom_raster() +
  scale_fill_manual(values = pal,
                    name = "Nº suitable \n months",
                    limits = factor(seq(0,12,1)),
                    na.value = "white") +
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
        axis.ticks.margin = unit(0, "null"))
# +
#   guides(fill = guide_legend(
#     ncol = 14,  # Set the number of columns
#     title.position = "left",  # Position title at the top
#     label.position = "bottom"  # Position labels at the bottom
#   )) 

leg1 <- get_legend( ggplot(clim_df,
                            aes(x = lon, y = lat,
                                fill = as.factor(sum_alb_fut))) +
                       geom_raster() +
                       scale_fill_manual(values = pal,
                                         name = "Nº suitable \n months",
                                         limits = factor(seq(0,12,1)),
                                         na.value = "white"))
           #          +
           # guides(fill = guide_legend(
           #   ncol = 13,  # Set the number of columns
           #   title.position = "left",  # Position title at the top
           #   label.position = "bottom"  # Position labels at the bottom
           # )))

gg1 <- ggarrange(alb + ggtitle(expression(paste("a) ",italic("Ae. albopictus")))),
                 aeg + ggtitle(expression(paste("b) ",italic("Ae. aegypti")))),
                 leg1,
                 nrow = 1,
                 widths = c(1,1,0.3))
gg2 <- ggarrange(diff_alb + ggtitle(expression(paste("c) ",italic("Ae. albopictus")))),
                 diff_aeg + ggtitle(expression(paste("d) ",italic("Ae. aegypti")))),
                 leg,
                 nrow = 1,
                 widths = c(1,1,0.3))
ggarrange(gg1,gg2, nrow=2)
ggarrange(leg1,
          alb + ggtitle(expression(paste("A ",italic("Ae. albopictus")))),
          aeg + ggtitle(expression(paste("B ",italic("Ae. aegypti")))),
          diff_alb + ggtitle(expression(paste("C ",italic("Ae. albopictus")))),
          diff_aeg + ggtitle(expression(paste("D ",italic("Ae. aegypti")))),
          leg,
          nrow = 1,
          widths = c(0.25,1,1,1,1,0.27))

ggarrange(gg1,gg2,
          nrow = 2,
          widths = c(1,1))


gg1 <- ggarrange(alb + theme(legend.position = "none") +
                   ggtitle(expression(paste("A ",italic("Ae. albopictus")))),
                 aeg +  theme(legend.position = "none") +
                   ggtitle(expression(paste("B ",italic("Ae. aegypti")))),
                 nrow = 1)
gg1 <- ggarrange(gg1, leg1,nrow = 2, heights = c(1,0.2))
gg2 <- ggarrange(diff_alb + theme(legend.position = "none") +
                   ggtitle(expression(paste("C ",italic("Ae. albopictus")))),
                 diff_aeg + theme(legend.position = "none") +
                   ggtitle(expression(paste("D ",italic("Ae. aegypti")))),
                 nrow = 1)
gg2 <- ggarrange(gg2, leg,nrow = 2, heights = c(1,0.2))
ggarrange(gg1, gg2, nrow = 1)

# ggplot aegypti
# check where the negative values are
ggplot(clim_df,
       aes(x = lon, y = lat,
           fill = as.factor(diff_aeg))) +
  geom_raster() +
  scale_fill_manual(values = pal1,
                    name = "Difference in\n suitable months",
                    limits = factor(seq(-6,-1,1)))
# whole plot
ggplot(clim_df,
       aes(x = lon, y = lat,
           fill = as.factor(diff_aeg))) +
  geom_raster() +
  scale_fill_manual(values = pal1,
                    name = "Difference in\n suitable months",
                    na.value = "white",
                    limits = factor(seq(-6,7,1))) +
  theme(legend.position = "right",
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
        axis.ticks.margin = unit(0, "null"))  

# Diff from 61-41
ggplot(clim_df,
       aes(x = lon, y = lat,
           fill = as.factor(diff_aeg6141))) +
  geom_raster() +
  scale_fill_manual(values = pal1[c(2,3,7,11,12,13,14)],
                    name = "Difference in\n suitable months",
                    na.value = "#F6F6F6",
                    limits = factor(seq(-2,4,1))) +
  theme(legend.position = "right",
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
        axis.ticks.margin = unit(0, "null"))  

# Create a range for easy visualization
pal2 <- pal1[c(2,4,7,11,13)]
clim_df$diff_alb_rang <- factor(ifelse(clim_df$diff_alb <= -3, "[-6,-3]",
                           ifelse(clim_df$diff_alb < 0, "(-3,0)",
                                  ifelse(clim_df$diff_alb == 0, "0",
                                    ifelse(clim_df$diff_alb <= 3, "(0,3]","(3,7]")))),
                           levels = c("[-6,-3]","(-3,0)",
                                      "0","(0,3]","(3,7]"))
ggplot(clim_df,
       aes(x = lon, y = lat,
           fill = diff_alb_rang)) +
  geom_raster() +
  scale_fill_manual(values = pal2,
                    name = "Difference in\n suitable months",
                    na.value = "#F6F6F6") +
  theme(legend.position = "right",
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
        axis.ticks.margin = unit(0, "null"))  

# Check months weather
time = "2061-2080"
clim_fut <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/output/eu_alb_aeg_",time,"_.Rds"))
clim_pre <- readRDS("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_clim_same_coords_2020.Rds")
clim_pre[is.na(clim_pre$pop),"tmean"] <- NA
clim_fut[is.na(clim_fut$pop),"tmean"] <- NA

library(ggpubr)
month_n <- 5
ggarrange(ggplot(clim_fut[clim_fut$month == month_n,],
       aes(x = lon, y = lat,
           fill = tmean)) +
  geom_raster() + scale_fill_viridis_c(option = "magma",
                                       limits = c(-2,45),
                                       na.value = "#F6F6F6") +
    ggtitle("Future projections: 2061-2080"),
  ggplot(clim_pre[clim_pre$month == month_n,],
                        aes(x = lon, y = lat,
                            fill = tmean)) +
    geom_raster()+ scale_fill_viridis_c(option = "magma",
                                        limits =  c(-2,45),
                                        na.value = "#F6F6F6") +
    ggtitle("2020"))
# clim_pre <- clim_pre %>% left_join(clim_fut, by = join_by(lon,lat))

# Spain ----------------------------------------------------------------
# this comes from future_climate/future_climate.R
year = 2060
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/clim_",
               year,".Rds")
df_2040 <- setDT(readRDS(Path))
df_2040 <- df_2040[,c("NATCODE", "alb", "aeg", "jap")]
colnames(df_2040) <-c ("NATCODE", "Alb_2040", "Aeg_2040", "Jap_2040")

year = 2080
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/clim_",
               year,".Rds")
df_2060 <- setDT(readRDS(Path))
df_2060 <- df_2060[,c("NATCODE", "alb", "aeg", "jap")]
colnames(df_2060) <-c ("NATCODE", "Alb_2060", "Aeg_2060", "Jap_2060")

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

# Compute diff years -------------------------------------------------
df_join <- df_2004 %>% 
  left_join(df_2020) %>% 
  left_join(df_2040) %>% 
  left_join(df_2060)

df_join$diff_0420 <- df_join$Alb_2020 - df_join$Alb_2004
df_join$diff_2040 <- df_join$Alb_2040 - df_join$Alb_2020
df_join$diff_0440 <- df_join$Alb_2040 - df_join$Alb_2004
df_join$diff_4060 <- df_join$Alb_2060 - df_join$Alb_2040
df_join$diff_2060 <- df_join$Alb_2060 - df_join$Alb_2020
df_join$diff_0460 <- df_join$Alb_2060 - df_join$Alb_2004

# Map Spain municipalities ----------------------------------------------------
library(mapSpain)
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

perim_esp <- esp_get_country()

# Plot map diff ------------------------------------------------------
df_join <- esp_can %>% left_join(df_join)

# Create a palette
library(RColorBrewer)
name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal1 <- rep("0",length(pal) +1 )
pal1[3:6] <- pal[1:4]
pal1[1] <- "#000455"
pal1[2] <- "#0C1290"
pal1[7] <- "#FFFFFF"
pal1[8:13] <- pal[6:11]
pal1[14] = "#74011C"

# Diff maps
diff_1 <- ggplot(df_join) +
  geom_sf(aes(fill = as.factor(diff_0420)), colour = NA) +
  # geom_sf(aes(fill = as.factor(diff_2040)), colour = NA) +
  # geom_sf(aes(fill = as.factor(diff_4060)), colour = NA) +
  # geom_sf(aes(fill = as.factor(diff_2080)), colour = NA) +
  geom_sf(data = perim_esp, fill = NA, alpha = 0.5, color = "grey") +
  geom_sf(data = can_box, lwd = 0.2) + coord_sf(datum = NA) +
  scale_fill_manual(na.value = "#F6F6F6",values = pal1,
                    name = "Difference \n in months",
                    limits = c(min(df_join$diff_0420,
                                   df_join$diff_2040):max(df_join$diff_0420,
                                                          df_join$diff_2040))) +
  theme_minimal() +
  theme(legend.position = "none") +
  guides(fill = guide_legend(
    ncol = 13,  # Set the number of columns
    title.position = "left",  # Position title at the top
    label.position = "bottom"  # Position labels at the bottom
  ))

library(ggpubr)
leg1 <- get_legend(ggplot(df_join) +
             geom_sf(aes(fill = as.factor(diff_0420)), colour = NA) +
             # geom_sf(aes(fill = as.factor(diff_2040)), colour = NA) +
             # geom_sf(aes(fill = as.factor(diff_4060)), colour = NA) +
             # geom_sf(aes(fill = as.factor(diff_2080)), colour = NA) +
             geom_sf(data = perim_esp, fill = NA, alpha = 0.5,
                     color = "grey") +
             geom_sf(data = can_box, lwd = 0.2) + coord_sf(datum = NA) +
             scale_fill_manual(na.value = "#F6F6F6",values = pal1,
                               name = "Difference \n in months",
                               limits = c(min(df_join$diff_0420,
                                              df_join$diff_2040):max(df_join$diff_0420,
                                                                     df_join$diff_2040))) +
             theme_minimal() +
               theme(legend.position = "bottom") +
               guides(fill = guide_legend(
                 ncol = 13,  # Set the number of columns
                 title.position = "left",  # Position title at the top
                 label.position = "bottom"  # Position labels at the bottom
               )))

ggarrange(leg1)

# Suitability maps
name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal[11]
pal[12] = "#74011C"
pal[13] = "#4B0011"

alb_04 <- ggplot(df_join) +
   geom_sf(aes(fill = as.factor(Alb_2004)), colour = NA) +
  # geom_sf(aes(fill = as.factor(Alb_2020)), colour = NA) +
  # geom_sf(aes(fill = as.factor(Alb_2040)), colour = NA) +
  # geom_sf(aes(fill = as.factor(Alb_2060)), colour = NA) +
  geom_sf(data = can_box, lwd = 0.2) + coord_sf(datum = NA) +
  scale_fill_manual(na.value = "#F6F6F6",values = pal,
                    name = "Nº suitable \n months",
                    limits = as.factor(seq(0,12,1))) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(
    ncol = 13,  # Set the number of columns
    title.position = "left",  # Position title at the top
    label.position = "bottom"  # Position labels at the bottom
  ))

leg <- get_legend(ggplot(df_join) +
  geom_sf(aes(fill = as.factor(Alb_2004)), colour = NA) +
  # geom_sf(aes(fill = as.factor(Alb_2020)), colour = NA) +
  # geom_sf(aes(fill = as.factor(Alb_2040)), colour = NA) +
  # geom_sf(aes(fill = as.factor(Alb_2060)), colour = NA) +
  geom_sf(data = can_box, lwd = 0.2) + coord_sf(datum = NA) +
  scale_fill_manual(na.value = "#F6F6F6",values = pal,
                    name = "Nº suitable \n months",
                    limits = as.factor(seq(0,12,1))) +
  theme_minimal() )

gg1 <- ggarrange(alb_04 + ggtitle("a)                      2004"),
                 alb_20 + ggtitle("b)                      2020"),
                 alb_60 + ggtitle("c)                 2041-2060"),
                 alb_80 + ggtitle("d)                 2061-2080"),
          nrow = 1, widths = c(1,1,1,1), common.legend = TRUE)
gg2 <- ggarrange(diff_1 + ggtitle("e)"),
                 diff_2 + ggtitle("f)"),
                 diff_3 + ggtitle("g)"),
                 nrow = 1, widths = c(1,1,1))
ggarrange(gg1,gg2, leg1,nrow = 3, heights = c(0.9,1,0.3))

# Check monthly maps Spain to see difference in 2004 and 2020
year = 2020
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/R0_clim_monthly",
               year,".Rds")
df_2004 <- readRDS(Path)
df_2004 <- df_2004[,c("NATCODE", "month", "dens", "precmean", "tmean", "R0_mon_alb")]
colnames(df_2004)[3:6] <- c("dens_2004" ,"precmean_2004" ,"tmean_2004" , "R0_alb_2004")
year = 2080
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/R0_clim_monthly",
               year,".Rds")
df_2020 <- readRDS(Path)
df_2020 <- df_2020[,c("NATCODE", "month","dens", "precmean", "tmean", "R0_mon_alb")]

Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/monthly_clim_2060.Rds")
df_2020 <- readRDS(Path)
df_2020 <- df_2020[,c("NATCODE", "month","dens", "prec", "tmean", "R0_alb")]
df_2020 <- df_2020 %>%
  mutate(month = recode(month,
                        Jan = 1,
                        Feb = 2,
                        Mar = 3,
                        Apr = 4,
                        May = 5,
                        Jun = 6,
                        Jul = 7,
                        Aug = 8,
                        Sep = 9,
                        Oct = 10,
                        Nov = 11,
                        Dec = 12
  ))

# Join two data sets
df_2020 <- df_2020 %>% left_join(df_2004, by=join_by(NATCODE,month))
df_2020$diff_2004 <- ifelse(df_2020$R0_alb_2004>df_2020$R0_alb ,1,0)
df_2020$diff_prec <- ifelse(df_2020$precmean_2004>df_2020$prec ,1,0)
df_2020$diff_dens <- ifelse(df_2020$dens_2004>df_2020$dens ,1,0)
df_2020$high_temp <- ifelse(df_2020$tmean>26.78 ,1,0)

# Count cases
nrow(df_2020[which(df_2020$diff_2004 == 1 & df_2020$R0_alb <1 & 
                     df_2020$diff_prec == 1),c("NATCODE")])
nrow(df_2020[which(df_2020$diff_2004 == 1 & df_2020$R0_alb <1 & 
                     df_2020$diff_prec == 1 & 
                     df_2020$tmean_2004 < df_2020$tmean),c("NATCODE")])
nrow(df_2020[which(df_2020$diff_2004 == 1 & df_2020$R0_alb <1),c("NATCODE")])

# Plot specific cases
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
NATCODE_l <-df_2020[which(df_2020$diff_2004 == 1 & df_2020$R0_alb <1 & 
                            df_2020$diff_prec == 1 & 
                            df_2020$tmean_2004 < df_2020$tmean),c("NATCODE")]
NATCODE_l$bool_1 <- 1
esp_can <- esp_can %>% left_join(NATCODE_l)
ggplot(esp_can) +
  geom_sf(aes(fill=bool_1), color = NA)

df_2020$bool_2004 <- ifelse(df_2020$R0_alb_2004>1,1,0)
df_2020$bool_2020 <- ifelse(df_2020$R0_mon_alb>1,1,0)
df_2020 <- df_2020 %>% group_by(NATCODE) %>%
  summarise(R0_2004 = sum(bool_2004),
            R0_2020 = sum(bool_2020))

df_2020 <- esp_can %>% left_join(df_2020)

# Suitability maps
name_pal = "RdYlBu"
display.brewer.pal(11, name_pal)
pal <- rev(brewer.pal(11, name_pal))
pal[11]
pal[12] = "#74011C"
pal[13] = "#4B0011"

ggplot(df_2020) +
  geom_sf(aes(fill = as.factor(R0_2004)), colour = NA) +
  # geom_sf(aes(fill = as.factor(Alb_2020)), colour = NA) +
  # geom_sf(aes(fill = as.factor(Alb_2040)), colour = NA) +
  # geom_sf(aes(fill = as.factor(Alb_2060)), colour = NA) +
  geom_sf(data = can_box, lwd = 0.2) + coord_sf(datum = NA) +
  scale_fill_manual(na.value = "#F6F6F6",values = pal,
                    name = "Nº suitable \n months",
                    limits = as.factor(seq(0,12,1))) +
  theme_minimal() +
  theme(legend.position = "none")

# Check European maps decrease in suitability
clim_pop <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_clim_same_coords_",2020,".Rds"))
clim_pop <- clim_pop[,c("month","lon", "lat", "prec", "tmean", "R0_alb")]
time = "2061-2080"
clim_df_61 <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/output/eu_alb_aeg_",time,"_.Rds"))
clim_df_61 <- clim_df_61[,c("month","lon", "lat", "prec", "tmean", "R0_alb")]
colnames(clim_df_61) <- c("month","lon", "lat", "prec_61", "tmean_61", "R0_alb_61")

# Compute differences to check different decrease in suitability
clim_pop <- clim_pop %>% left_join(clim_df_61)
clim_pop$diff <- clim_pop$R0_alb_61 - clim_pop$R0_alb
clim_pop$diff_prec <- clim_pop$prec_61 - clim_pop$prec
clim_pop$diff_tmean <- clim_pop$tmean_61 - clim_pop$tmean
clim_pop$high_tmean <- ifelse(clim_pop$tmean_61 > 27,1,0)

df_aux <- clim_pop[which(clim_pop$diff <0 &
                           clim_pop$R0_alb_61 <1 &
                           clim_pop$R0_alb >=1),]

# Count cases
df_aux[which(df_aux$diff_tmean >0 &
               df_aux$diff_prec<0),]
nrow(unique(df_aux[which(df_aux$diff_tmean <0 |
                           df_aux$diff_prec<0),c("lon", "lat")]))
nrow(unique(df_aux[which(df_aux$diff_tmean < 0 &
                           df_aux$diff_prec>0),c("lon", "lat")]))
nrow(unique(df_aux[which(df_aux$high_tmean == 1),c("lon", "lat")]))

# Plot
df_prec <- unique(df_aux[which(df_aux$diff_tmean >0 &
                                 df_aux$diff_prec<0 &
                                 df_aux$high_tmean == 0),c("lon", "lat")])

df_prec$diff <- 1
time = "2041-2060"
clim_df_41 <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/output/summon_eu_",time,".Rds"))
df_prec <- clim_df_41 %>% left_join(df_prec)
df_prec$diff <- ifelse(is.na(df_prec$diff) & is.na(df_prec$sum_alb) == FALSE,0,
                       ifelse(is.na(df_prec$sum_alb) == FALSE & df_prec$diff ==1,1,NA))
ggplot(df_prec,
       aes(x = lon, y = lat,
           fill = as.factor(diff))) +
  geom_raster() +
  scale_fill_viridis_d(name = "Less rainfall") + theme_minimal()

# Check European maps decrease in suitability for Ae aegpyti
clim_pop <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_clim_same_coords_",2020,".Rds"))
clim_pop <- clim_pop[,c("month","lon", "lat", "prec", "tmean", "R0_aeg")]
colnames(clim_pop) <- c("month","lon", "lat", "prec", "tmean", "R0_alb")
time = "2061-2080"
clim_df_61 <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/output/eu_alb_aeg_",time,"_.Rds"))
clim_df_61 <- clim_df_61[,c("month","lon", "lat", "prec", "tmean", "R0_aeg")]
colnames(clim_df_61) <- c("month","lon", "lat", "prec_61", "tmean_61", "R0_alb_61")

# Compute differences to check different decrease in suitability
clim_pop <- clim_pop %>% left_join(clim_df_61)
clim_pop$diff <- clim_pop$R0_alb_61 - clim_pop$R0_alb
clim_pop$diff_prec <- clim_pop$prec_61 - clim_pop$prec
clim_pop$diff_tmean <- clim_pop$tmean_61 - clim_pop$tmean
clim_pop$high_tmean <- ifelse(clim_pop$tmean_61 > 27,1,0)

df_aux <- clim_pop[which(clim_pop$diff <0 &
                           clim_pop$R0_alb_61 <1 &
                           clim_pop$R0_alb >=1),]

# Plot
df_prec <- unique(df_aux[which(df_aux$diff_tmean >0 &
               df_aux$diff_prec<0),c("lon", "lat")])

df_prec$diff <- 1
time = "2041-2060"
clim_df_41 <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/output/summon_eu_",time,".Rds"))
df_prec <- clim_df_41 %>% left_join(df_prec)
df_prec$diff <- ifelse(is.na(df_prec$diff) & is.na(df_prec$sum_alb) == FALSE,0,
                       ifelse(is.na(df_prec$sum_alb) == FALSE & df_prec$diff ==1,1,NA))
ggplot(df_prec,
       aes(x = lon, y = lat,
           fill = as.factor(diff))) +
  geom_raster() +
  scale_fill_viridis_d(name = "Less rainfall") + theme_minimal()

# Count cases
nrow(unique(df_aux[which(df_aux$diff_tmean <0 |
                           df_aux$diff_prec<0),c("lon", "lat")]))
nrow(unique(df_aux[which(df_aux$diff_tmean < 0 &
                           df_aux$diff_prec>0),c("lon", "lat")]))
nrow(unique(df_aux[which(df_aux$high_tmean == 1),c("lon", "lat")]))
