# Create animated plot for presence absence Spain

# Load libraries
library(dplyr)
library(tidyr)
# Load data sets
NATCODE_mitma_pa <-readRDS("~/albo_mobility/data/pa_mitma.Rds") # File taken from analyze_stas_mitma.R

# Load Spanish muni
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))


# Function to expand the data frame
expand_years <- function(df) {
  df %>%
    filter(!is.na(year_detec)) %>%   # Filter out rows with NA in year_detec
    rowwise() %>%
    mutate(year_detec = list(year_detec:2023)) %>%
    unnest(year_detec)
}


# Apply the function
expanded_df <- expand_years(NATCODE_mitma_pa)
# NATCODE_mitma_pa[is.na(NATCODE_mitma_pa$year_detec), "year_detec"] <- 0
# NATCODE_mitma_pa_map <- rbind(expanded_df[, c("NATCODE", "year_detec")],
#       NATCODE_mitma_pa[is.na(NATCODE_mitma_pa$year_detec), c("year_detec", NATCODE)])

# Join data sets
NATCODE_mitma_pa_map <- esp_can %>% left_join(expanded_df)

library("sf")
esp_perim <- st_union(esp_can)

# Create animated plots
ggplot(NATCODE_mitma_pa_map) +
  geom_sf(data = esp_perim, color = "grey", alpha = 0.6, size = 0.01) +
  geom_sf(aes(fill = 1), color = NA, linewidth = 0.01) +
  geom_sf(data = can_box) + coord_sf(datum = NA) +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
  theme_void() +
  labs(title = "Year: {current_frame}") +
  transition_manual(as.factor(year_detec))

anim_save(animation = last_animation(), filename = "~/anim.mp4",
                saver = NULL)

# Create fixed plots
NATCODE_mitma_pa_map <- esp_can %>% left_join(NATCODE_mitma_pa)
ggplot(NATCODE_mitma_pa_map) +
  geom_sf(data = esp_perim, color = "grey", alpha = 0.6, size = 0.01) +
  geom_sf(aes(fill = as.factor(year_detec)), color = NA, linewidth = 0.01) +
  scale_fill_viridis_d(name = "Year detection", na.translate = F) +
  geom_sf(data = can_box) + coord_sf(datum = NA)  +
  theme_void()
