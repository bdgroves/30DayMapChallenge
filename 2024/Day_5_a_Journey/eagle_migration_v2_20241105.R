# Load necessary libraries
library(tidyverse)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(janitor)
library(ggtext)  # For rich text annotation
library(ggmap)   # For basemap

# Register your Stadia Maps API key
register_google(key = "bea8f404-ca6d-4625-8df1-b8146b87ea9d")

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

# Create a line geometry from the points
baca03_line <- baca03_sf %>%
  summarize(geometry = st_union(geometry)) %>%  # Combine all points into a single line
  st_cast("LINESTRING")  # Cast the geometry to LINESTRING

# Calculate the bounding box around the eagle's path
eagle_bbox <- st_bbox(baca03_sf)  # Get bounding box of eagle data
buffer <- 0.1  # Adjust buffer to prevent exceeding valid coordinate ranges

# Ensure coordinates are in valid order
bbox <- c(
  left = min(eagle_bbox["xmin"] - buffer, eagle_bbox["xmax"] - buffer),
  bottom = min(eagle_bbox["ymin"] - buffer, eagle_bbox["ymax"] - buffer),
  right = max(eagle_bbox["xmin"] + buffer, eagle_bbox["xmax"] + buffer),
  top = max(eagle_bbox["ymin"] + buffer, eagle_bbox["ymax"] + buffer)
)

# Print the bounding box to debug
print(bbox)

# Get OpenStreetMap basemap using Stadia Maps
basemap <- get_stadiamap(bbox = bbox, zoom = 8, maptype = "outdoors")

# Plot the journey, centered on the eagle's track with enhancements
ggmap(basemap) +  # Use ggmap to plot the basemap
  geom_sf(data = baca03_line, aes(geometry = geometry), color = "#2c7fb8", size = 0.6, inherit.aes = FALSE) +  # Connecting line
  geom_sf(data = baca03_sf %>% slice(1), aes(geometry = geometry), color = "green", size = 3, shape = 21, fill = "green", inherit.aes = FALSE) +  # Start point
  geom_sf(data = baca03_sf %>% slice(n()), aes(geometry = geometry), color = "red", size = 3, shape = 24, fill = "red", inherit.aes = FALSE) +  # End point
  labs(
    title = "Journey of Bald Eagle BACA03",
    subtitle = "Pacific Northwest Migration Path (2018-2023)",
    caption = "Data Source: Movebank (https://www.movebank.org/)"
  ) +
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
