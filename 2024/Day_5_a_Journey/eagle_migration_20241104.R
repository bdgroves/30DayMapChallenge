# Load necessary libraries
library(tidyverse)
library(sf)
library(ggspatial)

# Clean up the column names
migration_data <- migration_data %>%
  clean_names()

# Filter data for BACA03 and convert timestamp
baca03_data <- migration_data %>%
  filter(individual_local_identifier == "BACA03") %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))  # Convert to POSIXct if needed

# Convert to sf object with coordinates for mapping
baca03_sf <- baca03_data %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# Check unique coordinates
unique_coords <- baca03_sf %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(long = X, lat = Y) %>%
  distinct()  # Get distinct coordinates for plotting

# Plot the locations on a map using OpenStreetMap as a basemap
map_baca03 <- ggplot() +
  annotation_map_tile("osm") +  # OpenStreetMap basemap
  geom_sf(data = baca03_sf, color = "blue", size = 1, alpha = 0.6) +  # Plot points with some transparency
  labs(
    title = "Coordinates of BACA03",
    x = "Longitude",
    y = "Latitude",
    caption = "Source: Movement Data"
  ) +
  coord_sf(expand = FALSE) +  # Keep the aspect ratio fixed
  theme_minimal()

# Print the map to view it in RStudio
print(map_baca03)

# Save the map as a PNG file
ggsave("map_BACA03_coordinates.png", plot = map_baca03, width = 10, height = 8, dpi = 300)
