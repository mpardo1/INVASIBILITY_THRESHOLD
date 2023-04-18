##### INITIALIZE #####
rm(list=ls())
.rs.restartR()
library(readxl)
library(tidyverse)
library(comperes)
library(sf)
library(mapSpain)

wd <- "~/INVASIBILITY_THRESHOLD/data/"
setwd(wd)

esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()

##### MITMA #####
municipios <- read.csv("~/INVASIBILITY_THRESHOLD/data/mob/relaciones_municipio_mitma.csv", sep = "|")
muni_ref_17 <- as.data.frame(read.csv("~/INVASIBILITY_THRESHOLD/data/mob/20200221_maestra_1_mitma_municipio.txt.gz", sep = "|"))


# filter for Girona. 67 total dists, 63 total munis (+2 missing)
muni_ref_17_girona <- muni_ref_17[,c("origen", "destino", "viajes", "viajes_km")] %>%
  group_by(origen, destino) %>%
  summarise(viajes = sum(viajes), viajes_km = sum(viajes_km)) # sum over periodo for daily quantities

# create mobility matrices
mat_mob_muni <- long_to_mat(muni_ref_17_girona, "destino", "origen", "viajes") %>%
  replace(is.na(.),0) %>% `diag<-`(., 0)
# heatmap(mat_mob_muni[nrow(mat_mob_muni):1, ], Rowv = NA, Colv = NA)
saveRDS(mat_mob_muni, file = "/home/marta/INVASIBILITY_THRESHOLD/data/mob/output/output_mat_mob_muni.Rds")
mat_eff_muni <- 1 - log(t(t(mat_mob_muni)/colSums(mat_mob_muni)))

# the coefficient m_ij denotes the number of travellers from region j to region i
# that is, fixing a column, I can read how many travellers go from the region indexing the column to
# each of the rest of the regions (indexing the rows). fixing a row, I can see the travellers coming
# into the region indexing the row from each of the rest of the regions (indexing the columns).
# (just like in the RMT model)

##### MAPS: MUNI #####
muni_plot <- ggplot(data = muni_shp) + # plot areas and their centroids
  geom_sf() +
  geom_point(data = muni_centroids, aes(x = X, y = Y), size = 1) +
  theme_minimal() + 
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
muni_plot

# outbound flows
muni_outflows <- muni_ref_17_girona %>% filter(origen != destino) %>% # plot the total outbound flow per area
  group_by(origen) %>% summarise(viajes = sum(viajes))
muni_shp_out <- left_join(muni_shp, muni_outflows, by = c("ID" = "origen"))
muni_plot_out <- ggplot(data = muni_shp_out) +
  geom_sf(aes(fill = viajes)) +
  scale_fill_gradientn(colours = c("#d9ed92", "#76c893", "#34a0a4", "#1a759f", "#184e77"), na.value = "gray90") +
  labs(fill = "Outbound travellers") +
  #geom_point(data = muni_centroids, aes(x = X, y = Y), size = 1) +
  geom_label(data = muni_centroids, aes(X, Y, label = ID), size = 3) +
  theme_minimal() + 
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
muni_plot_out

# inbound flows
muni_inflows <- muni_ref_17_girona %>% filter(origen != destino) %>% # plot the total inbound flow per area
  group_by(destino) %>% summarise(viajes = sum(viajes))
muni_shp_in <- left_join(muni_shp, muni_inflows, by = c("ID" = "destino"))
muni_shp_in <- muni_shp_in[-which(is.na(muni_shp_in$viajes) == TRUE),]
muni_plot_in <- ggplot(data = muni_shp_in) +
  geom_sf(aes(fill = viajes/max(viajes))) +
  scale_fill_gradientn(colours = c("#d9ed92", "#76c893", "#34a0a4", "#1a759f", "#184e77"), na.value = "gray90") +
  labs(fill = "Inbound travellers") +
  #geom_point(data = muni_centroids, aes(x = X, y = Y), size = 1) +
  geom_label(data = muni_centroids, aes(X, Y, label = ID), size = 3) +
  theme_minimal() + 
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
muni_plot_in

# flowmap
#library(remotes)
#library(htmlwidgets)
#install_github("FlowmapBlue/flowmapblue.R")
library(flowmapblue)
muni_loc <- mutate(muni_centroids, NOMBRE = ID)[,c(3,4,1,2)] %>% # generate locations data (id, name, lat, lon)
  st_as_sf(coords = c("X","Y"), crs = 3042) %>% st_transform(crs = 4326) %>% # transform to lat and lon
  cbind(st_coordinates(.)) %>% st_drop_geometry() 
names(muni_loc) <- c("id","name","lon","lat")
muni_flow <- rename(muni_ref_17_girona[,c(1,2,3)], origin = 1, dest = 2, count = 3) # generate flows data (origin, dest, count)
flowmapblue(muni_loc, muni_flow, clustering = TRUE, darkMode = TRUE, animation = FALSE)
# if this does not work, try enabling "Desktop OpenGL" in Tools -> Global options -> General -> Advanced -> Rendering engine
#write.csv(muni_loc, file = "./mob/MITMA/muni_loc.csv", quote = FALSE, row.names = FALSE)
#write.csv(muni_flow, file = "./mob/MITMA/muni_flow.csv", quote = FALSE, row.names = FALSE)


# Take NATCODE from mapSpain package:
muni_shp <- st_read("~/INVASIBILITY_THRESHOLD/data/mob/zonificacion-municipios/municipios_mitma.shp") 
ggplot(muni_shp) + 
  geom_sf()

Path <- "~/INVASIBILITY_THRESHOLD/data/boundaries.gpkg"
muni_shp <- st_read(Path) 

muni_centroids <- st_centroid(muni_shp$geometry) %>% st_geometry() %>% # generate centroids and save their coordinates
  st_coordinates() %>% as.data.frame() %>%
  mutate(ID = muni_shp$ID)
esp_can <- esp_get_munic_siane(moveCAN = FALSE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- paste0("34",esp_can$codauto,esp_can$cpro,esp_can$LAU_CODE)

st_crs(esp_can)

prep_wth <- (muni_centroids %>%  st_as_sf(coords = c("X","Y"),
                                          crs = st_crs(muni_shp),
                                          remove = FALSE) %>% st_transform(st_crs(esp_can))) %>%
  st_join(esp_can,
          join = st_covered_by,
          left = TRUE)  %>% dplyr::select(NATCODE,ID)



mob_June <- readRDS("~/INVASIBILITY_THRESHOLD/output/mob/mobility_June.Rds") 
mob_June <- mob_June %>% group_by(destino) %>%
  summarise(viajes = sum(viajes),viajes_mean = mean(viajes_mean),
            viajes_km = sum(viajes_km), viajes_km_mean = mean(viajes_km_mean))
mob_join <- mob_June %>% left_join(prep_wth, by = c("destino"= "ID"))

Path = "~/INVASIBILITY_THRESHOLD/data/mob/20220401_Viajes_municipios.csv.gz"
muni_ref_17 <- as.data.frame(read.csv(Path, sep = "|"))
muni_ref_17 <- muni_ref_17 %>% group_by(destino) %>%
  summarise(viajes = sum(viajes),
            viajes_km = sum(viajes_km))
Path = "~/INVASIBILITY_THRESHOLD/data/mob/relacion_ine_zonificacionMitma.csv"
rel_MITMA_INE <- as.data.frame(read.csv(Path, sep = "|"))

Path = "~/INVASIBILITY_THRESHOLD/data/mob/nombres_municipios.csv"
rel_MITMA_INE <- as.data.frame(read.csv(Path, sep = "|"))
