# Code to join the plots from future and present maps
# also to compute the difference in number of months
rm(list= ls())
library(eurostat)
library(terra)
library(tidyverse)
library(data.table)
library(sf)
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")

# Load data frames
time = "2041-2060"
time = "2061-2080"
clim_df <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/output/summon_eu_",time,".Rds"))
colnames(clim_df) <- c("id","sum_alb_fut","sum_aeg_fut","sum_jap_fut","lon","lat")
clim_pop <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_R0_fitfuture_clim_",2020,".Rds"))
colnames(clim_pop) <- c("id","sum_alb_pres","sum_aeg_pres","sum_jap_pres","lon","lat")

# Join two data frames
clim_df <- clim_df %>% left_join(clim_pop, by = join_by(lon,lat))

# Compute the difference
clim_df$diff_alb <- clim_df$sum_alb_fut - clim_df$sum_alb_pres
clim_df$diff_aeg <- clim_df$sum_aeg_fut - clim_df$sum_aeg_pres

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

# ggplot albopictus
ggplot(clim_df,
       aes(x = lon, y = lat,
           fill = as.factor(diff_alb))) +
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

# ggplot aegypti
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
