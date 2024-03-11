# Load raster
# Load pkgs and remove data -----------------------------------------------------
rm(list = ls())
library("data.table")
library("ggplot2")
library("terra")
library("sf")
library("tidyverse")
library("mapSpain")

# Load raster future pop density: https://ghsl.jrc.ec.europa.eu/ghs_pop2023.php
Path <- "~/INVASIBILITY_THRESHOLD/data/pop/GHS_POP_E2030_GLOBE_R2023A_reprojected_bilinear_025.tif"
pop_world_fut <- rast(Path)
plot(log(pop_world_fut))
plot(pop_world_fut)

# Load population density from: https://sedac.ciesin.columbia.edu/data/collection/gpw-v4
Path <- "~/INVASIBILITY_THRESHOLD/data/gpw_v4_population_density_rev11_2020_2pt5_min.tif"
pop_world <- rast(Path)
plot(log(pop_world))

# Check diff between 2020 pop density and future projections
pop_world <- terra::project(pop_world,pop_world_fut, method="average")
plot(log(pop_world))
pop_dif <- pop_world_fut - pop_world
plot(log(pop_dif))

# Load Spanish municipality maps
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

# Spanish population density
Path <- "/home/marta/INVASIBILITY_THRESHOLD/data/pop/pobmun20.csv"
pop22 <- read.csv(Path, sep = ";")
pop22$cmun <- ifelse(pop22$CMUN<10, paste0("00",pop22$CMUN),
                     ifelse(pop22$CMUN<100, paste0("0",pop22$CMUN),
                            as.character(pop22$CMUN)))
pop22$cpro <- ifelse(pop22$CPRO<10,
                     paste0("0",pop22$CPRO),as.character(pop22$CPRO))
pop22$POB22 <- gsub('\\.','',pop22$TOTAL)
pop22$POB22 <- as.numeric(pop22$POB22)
pop22$POB22 <- as.numeric(pop22$POB20)
esp_can <- esp_can %>% left_join(pop22, by = c("cpro","cmun") )
esp_can[which(is.na(esp_can$POB22)),"POB22"] <- 0

# Transform squared meters (output st_area) to squared km
esp_can$area <- as.numeric(st_area(esp_can))/1000000
esp_can$dens <- esp_can$POB22/esp_can$area
esp_can$ID <- c(1:nrow(esp_can))

# Project EU future density in Spain  munis
pop_esp_fut <- terra::extract(pop_world_fut,esp_can)
clim_df1 <- clim_df1 %>% group_by(ID) %>%
  summarise(dens = mean(value))

