# Code to compute R_M for aggregated raster for 2003-2020
# Load pkgs and remove data -----------------------------------------------------
rm(list = ls())
library("data.table")
library("ggplot2")
library("terra")
library("sf")
library("mapSpain")
library("tidyverse")
# library("geodata")
library(parallel)

sft_path <- "~" # Ubuntu or Mac
sft_path <- ".."# Windows
# source("G:/mpardo/INVASIBILITY_THRESHOLD/code/funcR0.R")
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")

# Load aggregated
Path <-  "U:/Colonization/data/CERRA/temp/tmean_monthly_2003-2020.tif"
Path <- "~/Documentos/PHD/2024/Colonization/data/weather/R_M/tmean_monthly_2003-2020.tif"
temp_df <- rast(Path)
Path <-  "U:/Colonization/data/CERRA/rain/rain_monthly_2003-2020.tif"
Path <- "~/Documentos/PHD/2024/Colonization/data/weather/R_M/rain_monthly_2003-2020.tif"
rain_df <- rast(Path)

# plot
plot(rain_df[[1]])
plot(temp_df[[8]])

# Load Spanish municipality maps
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

# Project and crop
temp_df <- terra::project(temp_df,esp_can)
temp_df <- terra::crop(temp_df, esp_can) %>%
  terra::mask(., esp_can)
rain_df <- terra::project(rain_df,esp_can)
rain_df <- terra::crop(rain_df, esp_can) %>%
  terra::mask(., esp_can)

# Plot
plot(rain_df[[6]])

# Intersect sf with raster
esp_can_id <- esp_can["NATCODE"]
esp_can_id$ID <- c(1:nrow(esp_can_id))

# For temperature
temp_ext <- terra::extract(temp_df,esp_can,ID=TRUE)
colnames(temp_ext)[2:ncol(temp_ext)] <- as.character(c(1:12))
temp_ext <- reshape2::melt(temp_ext, id.vars = c("ID"))
temp_ext <- temp_ext %>% group_by(ID, variable) %>% 
  summarise(tmean = mean(value))
temp_ext <- esp_can_id %>% left_join(temp_ext)

# For rainfall
rain_ext <- terra::extract(rain_df,esp_can,ID=TRUE)
colnames(rain_ext)[2:ncol(rain_ext)] <- as.character(c(1:12))
rain_ext <- reshape2::melt(rain_ext, id.vars = c("ID"))
rain_ext <- rain_ext %>% group_by(ID, variable) %>% 
  summarise(prec = mean(value))
rain_ext <- esp_can_id %>% left_join(rain_ext)

# Plot and check
ggplot(rain_ext[temp_ext$variable == 4,]) +
  geom_sf(aes(fill = prec), color = NA) +
  scale_fill_viridis_c()

# Join temp and rain data frames
rain_ext$geometry <- NULL
clim_df <- temp_ext %>% left_join(rain_ext)

# Average human density for 2004-2020
# Add population density 
Path <- "/home/marta/Documentos/PHD/2023/INVASIBILITY/data/pobmun_en/pobmun03.csv"
pop <- read.csv(Path, sep = ",")
pop$cmun <- ifelse(pop$CMUN<10, paste0("00",pop$CMUN),
                     ifelse(pop$CMUN<100, paste0("0",pop$CMUN),
                            as.character(pop$CMUN)))
pop$cpro <- ifelse(pop$CPRO<10,
                     paste0("0",pop$CPRO),as.character(pop$CPRO))
pop$POB <- as.numeric(gsub('\\.','',pop[,5]))
pop <- pop[, c("cmun","cpro", "POB")]
for(i in c(2004:2020)){
  # Add population density 
  Path <- paste0("/home/marta/Documentos/PHD/2023/INVASIBILITY/data/pobmun_en/pobmun",
                 substr(as.character(i),3,4),".csv")
  pop_aux <- read.csv(Path, sep = ",")
  if(ncol(pop_aux) == 1){
    pop_aux <- read.csv(Path, sep = ";")
  }
  
  head(pop_aux)
  pop_aux$cmun <- ifelse(pop_aux$CMUN<10, paste0("00",pop_aux$CMUN),
                       ifelse(pop_aux$CMUN<100, paste0("0",pop_aux$CMUN),
                              as.character(pop_aux$CMUN)))
  pop_aux$cpro <- ifelse(pop_aux$CPRO<10,
                       paste0("0",pop_aux$CPRO),as.character(pop_aux$CPRO))
  print(colnames(pop_aux[,5]))
  pop_aux$POB <- as.numeric(gsub('\\.','',pop_aux[,5]))
  pop_aux <- pop_aux[, c("cmun","cpro", "POB")]
  pop <- rbind(pop_aux, pop)
  pop <- pop %>% group_by(cpro,cmun) %>% 
    summarise(POB = mean(POB))
}

esp_can_pop <- esp_can %>% left_join(pop, by = c("cpro","cmun") )
esp_can_pop[which(is.na(esp_can_pop$POB)),"POB"] <- 0

# Left join with pop
esp_can_pop$geometry <- NULL
clim_df$geometry <- NULL
clim_df <- clim_df %>% left_join(esp_can_pop)
clim_df <- setDT(clim_df)

# Compute R_M
clim_df[, R0_alb := mapply(R0_func_alb, tmean, prec, POB)]
clim_df <- clim_df[,c("NATCODE", "tmean", "prec", "POB", "R0_alb")]
clim_df$bool <- ifelse(clim_df$R0_alb>1,1,0)

# Aggregate yearly
clim_y <- clim_df %>% group_by(NATCODE) %>% 
  summarise(sum_alb = sum(bool))

# Plot map with R_M
clim_y <- esp_can %>% left_join(clim_y)
ggplot(clim_y) +
  geom_sf(aes(fill = as.factor(sum_alb)), color = NA) +
  scale_fill_viridis_d()

saveRDS(clim_y, "~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/R0_avg_2003-2020.Rds")
