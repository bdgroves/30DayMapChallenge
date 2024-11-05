# Load necessary libraries
library(tidyverse)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(janitor)
library(ggtext)  # For rich text annotation

# Load and clean data
migration_data <- read_csv("C:/data/R_Projects/30DayMapChallenge/2024/Day_5_a_Journey/Bald Eagle (Haliaeetus leucocephalus) in the Pacific Northwest.csv", 
                           col_types = cols(
                             `individual-local-identifier` = col_character(),
                             `study-name` = col_character()
                           ))

migration_data <- migration_data %>%
  clean_names()

# Filter data for BACA03 and convert timestamp
baca03_data <- migration_data %>%
  filter(individual_local_identifier == "BACA03") %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))  

# Create a spatial data frame with 'sf'
baca03_sf <- baca03_data %>%
  filter(!is.na(location_long) & !is.na(location_lat)) %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = 4326)  # Set CRS to WGS84

# Get Natural Earth data for North America
world <- ne_countries(scale = "medium", returnclass = "sf")

# Calculate the bounding box around the eagle's path
eagle_bbox <- st_bbox(baca03_sf)  # Get bounding box of eagle data
buffer <- 0.5  # Add a buffer to zoom out slightly for context
xlim <- c(eagle_bbox["xmin"] - buffer, eagle_bbox["xmax"] + buffer)
ylim <- c(eagle_bbox["ymin"] - buffer, eagle_bbox["ymax"] + buffer)

# Plot the journey, centered on the eagle's track with enhancements
ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "gray80") +  # Soft background
  geom_sf(data = baca03_sf, color = "#2c7fb8", size = 0.6) +  # Eagle's path
  geom_sf(data = baca03_sf %>% slice(1), color = "green", size = 3, shape = 21, fill = "green") +  # Start point
  geom_sf(data = baca03_sf %>% slice(n()), color = "red", size = 3, shape = 24, fill = "red") +  # End point
  labs(
    title = "Journey of Bald Eagle BACA03",
    subtitle = "Pacific Northwest Migration Path (2018-2023)",
    caption = "Data Source: Movebank (https://www.movebank.org/)"
  ) +
  coord_sf(xlim = xlim, ylim = ylim) +  # Center on eagle's path
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  annotate("text", x = eagle_bbox["xmin"] + 4.0, y = eagle_bbox["ymax"] - 1.0, 
           label = "British Columbia", color = "black", size = 5, fontface = "italic") +  # Label for British Columbia
  theme_minimal() +
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank(),  # Remove grid lines
    legend.position = "none",      # Remove legend
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 8, hjust = 0)  # Small caption for data source
  )