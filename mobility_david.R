##### INITIALIZE #####
library(readxl)
library(tidyverse)
library(comperes)
library(sf)

wd <- "E:/yy/ma/brz"
setwd(wd)

municipios_spain <- read_xlsx(paste0(wd,"/mob/municipios_por_provincia.xlsx"))
municipios_girona <- filter(municipios_spain, CPRO == 17)

##### MITMA #####

distritos <- read.csv("./mob/MITMA/relaciones_distrito_mitma.csv", sep = "|")
municipios <- read.csv("./mob/MITMA/relaciones_municipio_mitma.csv", sep = "|")

dist_ref_17 <- read.csv("./mob/MITMA/20200217_maestra_1_mitma_distrito.txt.gz", sep = "|")
muni_ref_17 <- read.csv("./mob/MITMA/20200217_maestra_1_mitma_municipio.txt.gz", sep = "|")

#dist_ref_17_2 <- read.csv("./mob/MITMA/20200217_maestra_2_mitma_distrito.txt.gz", sep = "|")
#muni_ref_17_2 <- read.csv("./mob/MITMA/20200217_maestra_2_mitma_municipio.txt.gz", sep = "|")

# filter for Girona. 67 total dists, 63 total munis (+2 missing)
distritos_girona <- distritos[startsWith(distritos$distrito_mitma, "17"),] %>% pull(distrito_mitma) %>% unique()
dist_ref_17_girona <- filter(dist_ref_17, origen %in% distritos_girona, destino %in% distritos_girona) %>% # keep data with origin and destination girona
  select(c(2,3,10,11)) %>% group_by(origen, destino) %>% summarise(viajes = sum(viajes), viajes_km = sum(viajes_km)) # sum over periodo for daily quantities
municipios_girona <- municipios[startsWith(municipios$municipio_mitma, "17"),] %>% pull(municipio_mitma) %>% unique()
muni_ref_17_girona <- filter(muni_ref_17, origen %in% municipios_girona, destino %in% municipios_girona) %>% # keep data with origin and destination girona
  select(c(2,3,6,7)) %>% group_by(origen, destino) %>% summarise(viajes = sum(viajes), viajes_km = sum(viajes_km)) # sum over periodo for daily quantities

# create mobility matrices
mat_mob_dist <- long_to_mat(dist_ref_17_girona, "destino", "origen", "viajes") %>% # create matrix. m_ij gives number of people going from j to i
  replace(is.na(.),0) %>% `diag<-`(., 0) # set diagonal to 0
heatmap(mat_mob_dist[nrow(mat_mob_dist):1, ], Rowv = NA, Colv = NA)
# write.csv(mat_mob_dist, file = "./mob/mat_mob_dist.csv")
mat_eff_dist <- 1 - log(t(t(mat_mob_dist)/colSums(mat_mob_dist)))

mat_mob_muni <- long_to_mat(muni_ref_17_girona, "destino", "origen", "viajes") %>%
  replace(is.na(.),0) %>% `diag<-`(., 0)
heatmap(mat_mob_muni[nrow(mat_mob_muni):1, ], Rowv = NA, Colv = NA)
# write.csv(mat_mob_muni, file = "./mob/mat_mob_muni.csv")
mat_eff_muni <- 1 - log(t(t(mat_mob_muni)/colSums(mat_mob_muni)))

# the coefficient m_ij denotes the number of travellers from region j to region i
# that is, fixing a column, I can read how many travellers go from the region indexing the column to
# each of the rest of the regions (indexing the rows). fixing a row, I can see the travellers coming
# into the region indexing the row from each of the rest of the regions (indexing the columns).
# (just like in the RMT model)

##### MAPS: DIST #####
dist_shp <- st_read("./mob/MITMA/zonificacion-distritos/distritos_mitma.shp") %>%
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
  #geom_point(data = dist_centroids, aes(x = X, y = Y), size = 1) +
  geom_label(data = muni_centroids, aes(X, Y, label = ID), size = 3) +
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
  geom_label(data = muni_centroids, aes(X, Y, label = ID), size = 3) +
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
muni_shp <- st_read("./mob/MITMA/zonificacion-municipios/municipios_mitma.shp") %>%
  filter(ID %in% municipios_girona)
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

##### INE #####

zonas <- read_xlsx("./mob/INE/areas_de_movilidad_y_poblacion_a_1_ene_2019.xlsx")

zona_2209 <- read_xlsx("./mob/INE/Tabla1.3MovCotidiana-FlujosOD+15p_2209.xlsx")

# filter for Girona. 69 total areas
zonas_girona <- filter(zonas, CPRO == 17)
zonas_girona <- unique(zonas_girona$ID_GRUPO) # keep ids of zonas in girona
zona_2209_girona <- dplyr::filter(zona_2209, `Provincia de residencia` == "Girona", `Provincia de destino` == "Girona") %>% # keep data with origin and destination girona
  select(c(3,7,9)) %>% rename(origen = 1, destino = 2, personas = 3) # keep and rename only relevant columns

# create mobility matrix
mat_mob_zona <- long_to_mat(zona_2209_girona, "destino", "origen", "personas") %>%
  replace(is.na(.),0) %>% `diag<-`(., 0)
heatmap(mat_mob_zona[nrow(mat_mob_zona):1, ], Rowv = NA, Colv = NA)
# write.csv(mat_mob_zona, file = "./mob/mat_mob_zona.csv")
mat_eff_zona <- 1 - log(t(t(mat_mob_zona)/colSums(mat_mob_zona)))

##### MAPS: ZONA #####

zona_shp <- st_read("./mob/INE/shapefiles_celdas_marzo2020/celdas_marzo_2020.shp") %>%
  filter(ID_GRUPO %in% zonas_girona)
zona_centroids <- st_centroid(zona_shp$geometry) %>% st_geometry() %>%
  st_coordinates() %>% as.data.frame() %>%
  mutate(ID_GRUPO = zona_shp$ID_GRUPO)
#st_as_sf(coords = c("X","Y"), crs = 3857)

zona_plot <- ggplot(data = zona_shp) +
  geom_sf() +
  geom_point(data = zona_centroids, aes(x = X, y = Y), size = 1) +
  theme_minimal() + 
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
zona_plot

# outbound flows
zona_outflows <- zona_2209_girona %>% filter(origen != destino) %>%
  group_by(origen) %>% summarise(personas = sum(personas))
zona_shp_out <- left_join(zona_shp, zona_outflows, by = c("ID_GRUPO" = "origen"))
zona_plot_out <- ggplot(data = zona_shp_out) +
  geom_sf(aes(fill = personas)) +
  scale_fill_gradientn(colours = c("#d9ed92", "#76c893", "#34a0a4", "#1a759f", "#184e77"), na.value = "gray90") +
  labs(fill = "Outbound travellers") +
  #geom_point(data = zona_centroids, aes(x = X, y = Y), size = 1) +
  geom_label(data = muni_centroids, aes(X, Y, label = ID), size = 3) +
  theme_minimal() + 
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
zona_plot_out

# inbound flows
zona_inflows <- zona_2209_girona %>% filter(origen != destino) %>%
  group_by(destino) %>% summarise(personas = sum(personas))
zona_shp_in <- left_join(zona_shp, zona_inflows, by = c("ID_GRUPO" = "destino"))
zona_plot_in <- ggplot(data = zona_shp_in) +
  geom_sf(aes(fill = personas)) +
  scale_fill_gradientn(colours = c("#d9ed92", "#76c893", "#34a0a4", "#1a759f", "#184e77"), na.value = "gray90") +
  labs(fill = "Inbound travellers") +
  #geom_point(data = zona_centroids, aes(x = X, y = Y), size = 1) +
  geom_label(data = muni_centroids, aes(X, Y, label = ID), size = 3) +
  theme_minimal() + 
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
zona_plot_in

# flowmap
#library(remotes)
#library(htmlwidgets)
#install_github("FlowmapBlue/flowmapblue.R")
library(flowmapblue)
zona_loc <- left_join(zona_centroids,select(zonas, LITERAL_GRUPO, ID_GRUPO))[,c(3,4,1,2)] %>%
  st_as_sf(coords = c("X","Y"), crs = 3857) %>% st_transform(crs = 4326) %>%
  cbind(st_coordinates(.)) %>% st_drop_geometry() %>% distinct() %>%
  mutate(LITERAL_GRUPO = str_replace_all(LITERAL_GRUPO, "[^[:alnum:]]", " "))
zona_loc[16,2] <- "Cruilles y otros"
names(zona_loc) <- c("id","name","lon","lat")
zona_flow <- rename(zona_2209_girona, origin = 1, dest = 2, count = 3)
flowmapblue(zona_loc, zona_flow, clustering = TRUE, darkMode = TRUE, animation = FALSE)
# if this does not work, try enabling "Desktop OpenGL" in Tools -> Global options -> General -> Advanced -> Rendering engine
# write.csv(zona_loc, file = "./mob/INE/zona_loc.csv", quote = FALSE, row.names = FALSE)
# write.csv(zona_flow, file = "./mob/INE/zona_flow.csv", quote = FALSE, row.names = FALSE)
