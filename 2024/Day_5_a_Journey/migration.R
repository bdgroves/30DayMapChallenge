# Load required libraries
library(sf)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Load North America shapefile from rnaturalearth
north_america <- ne_countries(scale = "medium", continent = "North America", returnclass = "sf")

# Define a bounding box to focus on North America
bbox <- st_as_sfc(st_bbox(c(xmin = -130, ymin = 15, xmax = -60, ymax = 70), crs = st_crs(4326)))

# Define coordinates for a migration path (Mexico to Canada)
migration_points <- data.frame(
  name = c("Mexico", "Texas", "Nebraska", "Dakotas", "Canada"),
  lat = c(19.4326, 29.4241, 41.4925, 46.8797, 52.9399),
  lon = c(-99.1332, -98.4936, -99.9018, -102.5517, -106.3468)
)

# Convert to sf object
migration_path <- st_as_sf(migration_points, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 4326)

# Create the journey map
ggplot() +
  # Add base map
  geom_sf(data = north_america, fill = "antiquewhite", color = "grey50") +
  geom_sf(data = bbox, color = NA, fill = NA) +
  
  # Plot migration path with lines and points
  geom_sf(data = migration_path, color = "dodgerblue", size = 1.2) +
  geom_sf(data = migration_path, aes(color = name), size = 3) +
  geom_path(data = st_coordinates(migration_path), aes(x = X, y = Y), color = "dodgerblue", size = 1) +
  
  # Add labels for each stop
  geom_text(data = migration_points, aes(x = lon, y = lat, label = name), vjust = -1, color = "black", size = 3) +
  
  # Customize map style
  scale_color_manual(values = c("dodgerblue", "orange", "purple", "forestgreen", "coral")) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering) +
  labs(
    title = "A Journey Through North America",
    subtitle = "Simulated Migration Path from Mexico to Canada",
    caption = "Data: Simulated | Visualization: #30DayMapChallenge"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none"
  )



