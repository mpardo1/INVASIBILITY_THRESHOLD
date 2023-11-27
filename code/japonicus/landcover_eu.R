# Code to extract the landcover for japonicus
# from Corine landcover: https://land.copernicus.eu/en/products/corine-land-cover/clc2018/download-by-area
# Load libraries ----------------------------------------------------------
g <- gc(reset = T)
rm(list = ls())
library(terra)
library(fs)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(exactextractr)
library(data.table)
library(raster)

# Load raster
Path <- "~/INVASIBILITY_THRESHOLD/data/landcover/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif"
landcover <- rast(Path)
plot(landcover) # Plot the raster
landcover # mixed forest, broaded leave and pastures
df_cat <- levels(landcover)[[1]]
levels(landcover)
landcover_f <- landcover %in% c(25, 18)
landcover_f <- landcover %in% 18
plot(landcover_f)
landcover_f <- landcover %in% 25
plot(landcover_f)

saveRDS(df_cat, "~/INVASIBILITY_THRESHOLD/data/code_landcover.Rds")

# Esp can shapefile --------------------------------------------------
path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/pa/status_2303.shp"
eu <- read_sf(path)
plot(eu[,"locCode"])

# subset raster by interest categories
categories_of_interest_vec <- c(18,21,22,23,25)
# subsetted_raster_p <- landcover %in% categories_of_interest_vec[4]
# plot(subsetted_raster_p)

# Check crs of shapefile and raster
crs(landcover)
crs(eu)

# Intersect the raster with each geometry of esp_can
eu <- st_transform(eu, crs = crs(landcover))
plot(eu[,"locCode"])

# # Intersection function ---------------------------------------------------
# func_prop <- function(nrowinit,nrowend){
#   landcov_fracs <- exact_extract(landcover, eu[c(nrowinit:nrowend),"locCode"],
#                                  function(df) {
#                                    df %>%
#                                      mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
#                                      group_by(locCode, value) %>%
#                                      summarize(freq = sum(frac_total))
#                                  }, summarize_df = TRUE, include_cols = 'locCode', progress = FALSE)
#   saveRDS(landcov_fracs,
#           paste0("~/INVASIBILITY_THRESHOLD/data/landcov_fracs_eu_",
#                  nrowinit,"_",nrowend,".Rds"))
#   return(landcov_fracs)
# }
# 
# # Divide the number of rows in chunks -------------------------------------
# n <- 1000
# sequence <- seq(1,nrow(eu),1)
# chunk_size <- ceiling(length(sequence) / n)
# chunks <- split(sequence, cut(seq_along(sequence),
#                               breaks = chunk_size * 0:n, labels = FALSE))
# 
# # Compute the landcover percentage for each chunk --------------------------
# # Display first and last number in each chunk
# df_perc <- data.frame()
# for (i in 1:length(chunks)) {
#   chunk <- chunks[[i]]
#   first_num <- chunk[1]
#   last_num <- chunk[length(chunk)]
#   df_aux <- func_prop(first_num,last_num)
#   colnames(df_aux) <- c("locCode", "value", "freq")
#   df_perc <- rbind(df_perc,df_aux)
# }
# 
# saveRDS(landcov_fracs,"~/INVASIBILITY_THRESHOLD/data/landcov_fracs_eu_5000_nrow.Rds")

# read cluster files
landcov_fracs1 <- readRDS("/home/marta/INVASIBILITY_THRESHOLD/data/landcov_fracs_eu_1_5000.Rds")
landcov_fracs2 <- readRDS("/home/marta/INVASIBILITY_THRESHOLD/data/landcov_fracs_eu_5000_nrow.Rds")
landcov_fracs <- rbind(landcov_fracs1, landcov_fracs2)

# select only specific landcover type
categories_of_interest <- categories_of_interest_vec[3]
categories_of_interest <- c(23)
filt_land <- landcov_fracs[landcov_fracs$value %in% categories_of_interest,]
saveRDS(filt_land, "~/INVASIBILITY_THRESHOLD/data/landcover_perc_broaded-leaved.Rds")
# filt_land <- readRDS("~/INVASIBILITY_THRESHOLD/data/landcover_perc.Rds")

# Plot Europe map
land_eu <- eu %>% left_join(filt_land)
ggplot(land_eu) +
  geom_sf(aes(fill = freq ), color = NA) +
  scale_fill_viridis_c() + theme_bw()

# Esp can shapefile --------------------------------------------------
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

# plot result
filt_land <- esp_can %>% left_join(filt_land)
filt_land$freq <- ifelse(is.na(filt_land$freq),0,filt_land$freq)
ggplot(filt_land) + 
  geom_sf(aes(fill = freq), color = NA) +
  scale_fill_viridis_c(name = paste0("Percentage \n",
                                     df_cat[df_cat$Value == categories_of_interest,
                                            "LABEL3"])) +
  theme_bw()

