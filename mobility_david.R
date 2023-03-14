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
distritos <- read.csv("~/INVASIBILITY_THRESHOLD/data/mob/relaciones_distrito_mitma.csv", sep = "|")
municipios <- read.csv("~/INVASIBILITY_THRESHOLD/data/mob/relaciones_municipio_mitma.csv", sep = "|")

dist_ref_17 <- read.csv("~/INVASIBILITY_THRESHOLD/data/mob/20200301_maestra_1_mitma_distrito.txt.gz", sep = "|")
muni_ref_17 <- read.csv("~/INVASIBILITY_THRESHOLD/data/mob/20200301_maestra_1_mitma_municipio.txt.gz", sep = "|")

#dist_ref_17_2 <- read.csv("./mob/MITMA/20200217_maestra_2_mitma_distrito.txt.gz", sep = "|")
#muni_ref_17_2 <- read.csv("./mob/MITMA/20200217_maestra_2_mitma_municipio.txt.gz", sep = "|")

# filter for Girona. 67 total dists, 63 total munis (+2 missing)
dist_ref_17_girona <- dist_ref_17 %>%
  select(c(2,3,10,11)) %>% group_by(origen, destino) %>% 
  summarise(viajes = sum(viajes), viajes_km = sum(viajes_km)) # sum over periodo for daily quantities


muni_ref_17_girona <- muni_ref_17 %>% 
  select(c(2,3,6,7)) %>% group_by(origen, destino) %>%
  summarise(viajes = sum(viajes), viajes_km = sum(viajes_km)) # sum over periodo for daily quantities

muni_ref_17_girona <- merge(muni_ref_17_girona, municipios, by.x = "origen", by.y = "municipio_mitma")
muni_ref_17_girona <- merge(muni_ref_17_girona, municipios, by.x = "destino", by.y = "municipio_mitma")
muni_ref_17_girona$destino <- NULL
muni_ref_17_girona$origen <- NULL
colnames(muni_ref_17_girona)[3:4] <- c("origen", "destino")

# create mobility matrices
mat_mob_dist <- long_to_mat(dist_ref_17_girona, "destino", "origen", "viajes") %>% 
  # create matrix. m_ij gives number of people going from j to i
  replace(is.na(.),0) %>% `diag<-`(., 0) # set diagonal to 0
heatmap(mat_mob_dist[nrow(mat_mob_dist):1, ], Rowv = NA, Colv = NA)
saveRDS(mat_mob_dist, file = "/home/marta/INVASIBILITY_THRESHOLD/data/mob/output/output_mat_mob_dist.Rds")
mat_eff_dist <- 1 - log(t(t(mat_mob_dist)/colSums(mat_mob_dist)))

mat_mob_muni <- long_to_mat(muni_ref_17_girona, "destino", "origen", "viajes") %>%
  replace(is.na(.),0) %>% `diag<-`(., 0)
heatmap(mat_mob_muni[nrow(mat_mob_muni):1, ], Rowv = NA, Colv = NA)
saveRDS(mat_mob_muni, file = "/home/marta/INVASIBILITY_THRESHOLD/data/mob/output/output_mat_mob_muni.Rds")
mat_eff_muni <- 1 - log(t(t(mat_mob_muni)/colSums(mat_mob_muni)))

# the coefficient m_ij denotes the number of travellers from region j to region i
# that is, fixing a column, I can read how many travellers go from the region indexing the column to
# each of the rest of the regions (indexing the rows). fixing a row, I can see the travellers coming
# into the region indexing the row from each of the rest of the regions (indexing the columns).
# (just like in the RMT model)

##### MAPS: DIST #####
dist_shp <- st_read("~/INVASIBILITY_THRESHOLD/data/mob/zonificacion-distritos/distritos_mitma.shp") %>%
  filter(ID %in% distritos_girona)
dist_centroids <- st_centroid(dist_shp$geometry) %>% st_geometry() %>% # generate centroids and save their coordinates
  st_coordinates() %>% as.data.frame() %>%
  mutate(ID = dist_shp$ID)
#st_as_sf(coords = c("X","Y"), crs = 3857)

dist_plot <- ggplot(data = dist_shp) + # plot areas and their centroids
  geom_sf() +
  geom_point(data = dist_centroids, aes(x = X, y = Y), size = 1) +
  theme_minimal() + 
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
dist_plot

# outbound flows
dist_outflows <- dist_ref_17_girona %>% filter(origen != destino) %>% # plot the total outbound flow per area
  group_by(origen) %>% summarise(viajes = sum(viajes))
dist_shp_out <- left_join(dist_shp, dist_outflows, by = c("ID" = "origen"))
dist_plot_out <- ggplot(data = dist_shp_out) +
  geom_sf(aes(fill = viajes)) +
  scale_fill_gradientn(colours = c("#d9ed92", "#76c893", "#34a0a4", "#1a759f", "#184e77"), na.value = "gray90") +
  labs(fill = "Outbound travellers") +
  # geom_point(data = dist_centroids, aes(x = X, y = Y), size = 1) +
  # geom_label(data = dist_centroids, aes(X, Y, label = ID), size = 3) +
  theme_minimal() + 
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
dist_plot_out

# inbound flows
dist_inflows <- dist_ref_17_girona %>% filter(origen != destino) %>% # plot the total inbound flow per area
  group_by(destino) %>% summarise(viajes = sum(viajes))
dist_shp_in <- left_join(dist_shp, dist_inflows, by = c("ID" = "destino"))
dist_plot_in <- ggplot(data = dist_shp_in) +
  geom_sf(aes(fill = viajes)) +
  scale_fill_gradientn(colours = c("#d9ed92", "#76c893", "#34a0a4", "#1a759f", "#184e77"), na.value = "gray90") +
  labs(fill = "Inbound travellers") +
  #geom_point(data = dist_centroids, aes(x = X, y = Y), size = 1) +
  # geom_label(data = dist_centroids, aes(X, Y, label = ID), size = 3) +
  theme_minimal() + 
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
dist_plot_in

# flowmap
#library(remotes)
#library(htmlwidgets)
#install_github("FlowmapBlue/flowmapblue.R")
library(flowmapblue)
dist_loc <- mutate(dist_centroids, NOMBRE = ID)[,c(3,4,1,2)] %>% # generate locations data (id, name, lat, lon)
  st_as_sf(coords = c("X","Y"), crs = 3042) %>% st_transform(crs = 4326) %>% # transform to lat and lon
  cbind(st_coordinates(.)) %>% st_drop_geometry() 
names(dist_loc) <- c("id","name","lon","lat")
dist_flow <- rename(dist_ref_17_girona[,c(1,2,3)], origin = 1, dest = 2, count = 3) # generate flows data (origin, dest, count)
flowmapblue(dist_loc, dist_flow, clustering = TRUE, darkMode = TRUE, animation = FALSE)
# if this does not work, try enabling "Desktop OpenGL" in Tools -> Global options -> General -> Advanced -> Rendering engine
#write.csv(dist_loc, file = "./mob/MITMA/dist_loc.csv", quote = FALSE, row.names = FALSE)
#write.csv(dist_flow, file = "./mob/MITMA/dist_flow.csv", quote = FALSE, row.names = FALSE)

##### MAPS: MUNI #####
muni_shp <- st_read("~/INVASIBILITY_THRESHOLD/data/mob/zonificacion-municipios/municipios_mitma.shp") 
 muni_centroids <- st_centroid(muni_shp$geometry) %>% st_geometry() %>% # generate centroids and save their coordinates
  st_coordinates() %>% as.data.frame() %>%
  mutate(ID = muni_shp$ID)

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
muni_plot_in <- ggplot(data = muni_shp_in) +
  geom_sf(aes(fill = viajes)) +
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

