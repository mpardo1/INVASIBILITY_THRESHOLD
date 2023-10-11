# code to validate the PA for japonicus in europe
rm(list=ls())
library(sf)
library(ggplot2)
library(tidyverse)
library(terra)
library(parallel)
library(data.table)
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")
# read presence absence data ------------------------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/status_2303.shp"
pa_jap <- read_sf(path)
pa_jap <- pa_jap[which(pa_jap$leave == 1),]
list_eu <- unique(pa_jap$cntryName)[c(1:2,4,6,7:8,10:14,16,19:23,25:26,28:30,32:35,39:42,
                                      45:52,54:56,58:59,61,62,67,68,70,71)]
pa_jap <- pa_jap[which(pa_jap$cntryName %in% list_eu),]
# ggplot(pa_jap) +
#   geom_sf(aes(fill = japonicus)) 

# load data for RM Japonicus Europe 2020
clim_pop <- readRDS(paste0("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/eu_clim_2020.Rds"))

# ------------------------- CLUSTER --------------------------------#
# transform crs raster ----------------------------------------------
# temp_eu <- rast("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/3monthly_aggregated_raster.tif")
# # plot(temp_eu[[1]])
# clim_pop_s <- st_as_sf(clim_pop,
#                        coords = c("lon","lat"),
#                        crs = crs(temp_eu))
# # ggplot(clim_pop_s) + 
# #   geom_sf(aes(color = sum_alb)) + 
# #   scale_fill_viridis_c()
# 
# # transform to match coord system -----------------------------------------
# clim_pop_s <- st_transform(clim_pop_s, crs(pa_jap))
# pa_jap$ind <- seq(1, nrow(pa_jap),1)
# # extract intersection for each geometry-----------------------------------
# inter <- function(pol_id){
#   pnts <- clim_pop_s[,c("geometry", "sum_jap", "id")] %>% mutate(
#     intersection = as.integer(st_intersects(geometry, pa_jap[pol_id,c("geometry")]))
#   )
#   pnts <- pnts[which(is.na(pnts$intersection) == FALSE),c("sum_jap","id")]
#   if(nrow(pnts) > 0){
#     df_out <- data.frame(sum_jap = pnts$sum_jap, pnt_id = pnts$id, geom_id = pol_id)
#     colnames(df_out) <- c("sum_jap", "pnt_id","geom_id")
#     df_out <- df_out[which(is.na(df_out$sum_jap) == FALSE), ]
#     return(df_out)
#   }else{
#     df_out <- data.frame(sum_jap = 0, pnt_id = 0,  geom_id = 0)
#     return(df_out)
#   }
# }

# test
# pol_id = 15
# inter(pol_id)
# ggplot(pa_jap) +
#   geom_sf(aes(fill = japonicus))  +
#   geom_sf(data =pnts[which(pnts$intersection == 1),],
#           aes(color = intersection), color = "red")


# parallelize
# cores = 12
# intersect_p_g <- mclapply(1:nrow(pa_jap), mc.cores = cores,
#                          mc.preschedule = F,inter)
# saveRDS(intersect_p_g, "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/out_inter_dd.Rds")
# ------------------------- CLUSTER --------------------------------#

# read results cluster
# rm(clim_pop, temp_eu)
inter_L <- readRDS("~/INVASIBILITY_THRESHOLD/data/japonicus/pa/out_inter_d.Rds")
inter_df <- data.frame()
for(i in c(1:length(inter_L))){
  df_aux <- inter_L[[i]]
  if(class(df_aux) == "data.frame"){
    colnames(df_aux) <- c("sum_jap", "pnt_id","geom_id")
    inter_df <- rbind(inter_df,df_aux)
  }
}

rm(inter_L)

# remove rows with no intersection and join with climatic data --------------
inter_df <- inter_df[which(inter_df$pnt_id != 0 & inter_df$geom_id != 0),]
check_dup <- inter_df %>% group_by(pnt_id) %>% 
  summarise(geom_id = min(geom_id), n = n())
check_dup$id <- check_dup$pnt_id
check_dup$pnt_id <- NULL
check_dup$n <- NULL
clim_pop <- clim_pop %>% left_join(check_dup)

# group by to have climate data for each region -----------------------------
clim_pop <- clim_pop %>% group_by(geom_id,month) %>%
  summarise(tmean = mean(tmean),
            prec = mean(prec),
            pop = mean(pop))

# compute RM per region -----------------------------------------------------
clim_pop <- setDT(clim_pop)
clim_pop[, R0_jap := mapply(R0_func_jap, tmean, prec, pop)]
clim_pop[, R0_alb := mapply(R0_func_alb, tmean, prec, pop)]
clim_pop$bool_rm <- ifelse(clim_pop$R0_jap>1,1,0)
clim_pop$bool_rm_alb <- ifelse(clim_pop$R0_alb>1,1,0)
clim_pop <- clim_pop %>% group_by(geom_id) %>%
  summarise(jap = sum(bool_rm),
            alb = sum(bool_rm_alb))

# join with pa data ---------------------------------------------------------
pa_jap$geom_id <- seq(1, nrow(pa_jap),1)
clim_pop <- pa_jap %>% left_join(clim_pop)

ggplot(clim_pop) +
  geom_sf(aes(fill = as.factor(jap)), color = NA) +
  scale_fill_viridis_d(option = "magma", direction = -1)

# compare pa japonicus with rm ----------------------------------------------
clim_pop$pa_jap <- ifelse(clim_pop$japonicus != "absent" & 
                            clim_pop$japonicus != "noData", 1,0)

ggplot(clim_pop) +
  geom_sf(aes(fill = as.factor(pa_jap)), color = NA)

# filter countries in europe ------------------------------------------------
unique(clim_pop$cntryName)
country_n <- list("Germany","Switzerland","Hungary" )
country_n <- list("Hungary" )
country_n <- unique(clim_pop_df[which(clim_pop_df$pa_jap == 1),"cntryName"])

# transform and save to data frame --------------------------------------------
clim_pop_df <- as.data.frame(clim_pop)
clim_pop_df$geometry <- NULL
saveRDS(clim_pop_df, "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/clim_pop_df.Rds")

# create plot -----------------------------------------------------------------
unique(clim_pop_df[which(clim_pop_df$pa_jap == 1),"cntryName"])
clim_pop_filt <- clim_pop_df[which(clim_pop$cntryName %in% country_n),]
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


# for albopictus ----------------------------------------------
clim_pop$pa_alb <- ifelse(clim_pop$albopictus != "absent" & 
                            clim_pop$albopictus != "noData", 1,0)
clim_pop_df <- as.data.frame(clim_pop)
clim_pop_df$geometry <- NULL
country_n <- unique(clim_pop_df[which(clim_pop_df$pa_alb == 1),"cntryName"])
clim_pop_filt <- clim_pop_df[which(clim_pop$cntryName %in% country_n),]
clim_pop_filt_g <- clim_pop_filt %>% group_by(alb) %>%
  summarise(sum_1 = sum(pa_alb), n = n())
clim_pop_filt_g$prop_pa <- clim_pop_filt_g$sum_1/clim_pop_filt_g$n
alb_eu_val <- ggplot(clim_pop_filt_g) + 
  geom_point(aes(alb, prop_pa, size = n))+ 
  geom_line(aes(alb, prop_pa)) + 
  ylab("Proportion presence") +
  xlab( "Nº months suitable") + 
  scale_x_continuous(breaks = seq(0,12,1)) +
  theme_bw() + scale_size_continuous(name = "Nº regions",
                                     labels = c("[0,30)","[30,100)","[100,1000)",
                                                "[1000,2000)","[2000,3000]",">3000"),
                                     breaks = c(0,30,100,1000,2000,3000)) +
  theme(legend.position = c(0.2,0.7),
        text = element_text(size = 14))
alb_eu_val

# create a function to compute prop per country ---------------------------
country_n_f <- country_n[c(9,10,11,12,14,16,24)]
clim_pop_filt <- clim_pop_df[which(clim_pop$cntryName %in% country_n_f),]
alb_eu <- clim_pop_filt_g
alb_eu$cntryName <- "whole eu"
clim_pop_filt_g <- clim_pop_filt %>% group_by(alb,cntryName) %>%
  summarise(sum_1 = sum(pa_alb), n = n())
clim_pop_filt_g$prop_pa <- clim_pop_filt_g$sum_1/clim_pop_filt_g$n
ggplot(clim_pop_filt_g) + 
  geom_point(aes(alb, prop_pa, size = n, colour = cntryName))+ 
  geom_line(aes(alb, prop_pa, colour = cntryName)) + 
  ylab("Proportion presence") +
  xlab( "Nº months suitable") + 
  scale_x_continuous(breaks = seq(0,12,1)) +
  theme_bw() + scale_size_continuous(name = "Nº regions",
                                     labels = c("[0,5)","[5,30)","[30,100)","[100,1000)",
                                                "[1000,2000)","[2000,3000]",">3000"),
                                     breaks = c(0,5,30,100,1000,2000,3000)) +
  theme(legend.position = c(0.95,0.6),
        text = element_text(size = 14))

# europe albopictus map ---------------------------------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/status_2303.shp"
pa_jap <- read_sf(path)
pa_jap <- pa_jap[which(pa_jap$leave == 1),]
pa_jap$pa_jap <- ifelse(pa_jap$japonicus != "absent" & 
                          pa_jap$japonicus != "noData", 1,0)  
pa_jap$pa_alb <- ifelse(pa_jap$albopictus != "absent" & 
                          pa_jap$albopictus != "noData", 1,0) 

list_eu <- unique(pa_jap$cntryName)[c(1:2,4,6,7:8,10:14,16,19:23,25:26,28:30,32:35,39:42,
                                      45:52,54:56,58:59,61,62,67,68,70,71)]
pa_jap_eu <- pa_jap[which(pa_jap$cntryName %in% list_eu),]
pa_jap_eu <- ggplot(pa_jap_eu) + 
  geom_sf(aes(fill = as.factor(pa_alb)), color = NA) +
  theme_minimal() + coord_sf(datum = NA) +
  xlim(c(-20,40)) + ylim(c(25,70)) + 
  scale_fill_manual(values = c("#D9EAF8","#377EB8"), name = " ") 
pa_jap_eu

# do a panel ------------------------------------------------------------
ggarrange(pa_jap_eu + ggtitle("A"),
          alb_eu_val + ggtitle("B"))
