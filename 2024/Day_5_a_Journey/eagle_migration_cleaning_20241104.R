# Load required libraries
library(tidyverse)
library(sf)
library(janitor)

# Load the migration data
# Replace 'migration_data.csv' with the path to your actual data file
migration_data <- read_csv("C:/data/R_Projects/30DayMapChallenge/2024/Day_5_a_Journey/Bald Eagle (Haliaeetus leucocephalus) in the Pacific Northwest.csv", col_types = cols(
  `individual-local-identifier` = col_character(),
  `study-name` = col_character()
))

# Clean up column names and filter only required columns
migration_data <- migration_data %>%
  clean_names() %>%   # Make column names consistent and snake_case
  select(
    lat = location_lat,       # Rename latitude column to 'lat'
    long = location_long,     # Rename longitude column to 'long'
    event_id = event_id,      # Keep event_id as is
    timestamp = timestamp,    # Keep timestamp as is
    individual_local_identifier,   # Keep individual ID
    study_name                 # Keep study name
  )

# Convert to an sf object for spatial data manipulation
migration_sf <- migration_data %>%
  drop_na(lat, long) %>%  # Drop rows with missing lat/long values
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Glimpse at the cleaned data
glimpse(migration_sf)

# Plot migration paths for each individual in the study
ggplot(migration_sf) +
  geom_sf(aes(color = individual_local_identifier), size = 1) +
  labs(
    title = unique(migration_data$study_name),
    subtitle = "Migratory Paths with Event Tracking",
    x = "Longitude",
    y = "Latitude",
    caption = "Data: Movebank | #30DayMapChallenge"
  ) +
  theme_minimal()

# Count records for each unique individual identifier
individual_counts <- migration_data %>%
  count(individual_local_identifier, name = "record_count")

# Filter data for TERF31
terf31_data <- migration_data %>%
  filter(individual_local_identifier == "TERF31") %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S")) # Convert to POSIXct if needed

# Convert to sf object with coordinates
terf31_sf <- terf31_data %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Plot the journey of TERF31
ggplot() +
  geom_sf(data = terf31_sf, aes(color = timestamp), size = 1, show.legend = TRUE) +
  geom_path(data = terf31_sf, aes(x = long, y = lat, group = individual_local_identifier), color = "blue", linetype = "dashed") +
  scale_color_viridis_c(option = "inferno", name = "Timestamp") +
  labs(
    title = "Journey of TERF31",
    subtitle = "Mapped locations over time",
    x = "Longitude",
    y = "Latitude",
    caption = "Source: Movement Data"
  ) +
  theme_minimal()

# Load libraries
library(tidyverse)
library(sf)
library(janitor)
library(ggspatial) # For basemap layers

# Clean up the column names
migration_data <- migration_data %>%
  clean_names()

# Filter data for TERF31
terf31_data <- migration_data %>%
  filter(individual_local_identifier == "TERF31") %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S")) # Convert to POSIXct if needed

# Convert to sf object with coordinates
terf31_sf <- terf31_data %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Plot the journey of TERF31 with basemap
journey_map <- ggplot() +
  annotation_map_tile("osm") +  # OpenStreetMap basemap
  geom_sf(data = terf31_sf, aes(color = timestamp), size = 1, show.legend = TRUE) +
  geom_path(data = terf31_sf, aes(x = long, y = lat, group = individual_local_identifier), color = "blue", linetype = "dashed") +
  scale_color_viridis_c(option = "inferno", name = "Timestamp") +
  labs(
    title = "Journey of TERF31",
    subtitle = "Mapped locations over time",
    x = "Longitude",
    y = "Latitude",
    caption = "Source: Movement Data"
  ) +
  theme_minimal()

# Print the map to view it in RStudio
print(journey_map)

# Save the map as a PNG or JPEG file
ggsave("journey_map_TERF31.png", plot = journey_map, width = 10, height = 8, dpi = 300)

# Load libraries
library(tidyverse)
library(sf)
library(janitor)
library(ggspatial)

# Clean up the column names
migration_data <- migration_data %>%
  clean_names()

# Filter data for TERF31
terf31_data <- migration_data %>%
  filter(individual_local_identifier == "TERF31") %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))  # Convert to POSIXct if needed

# Convert to sf object with coordinates
terf31_sf <- terf31_data %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Define bounding box to zoom in on the track
bbox <- st_bbox(terf31_sf) %>%  # Calculate bounding box of the track
  st_as_sfc() %>% 
  st_buffer(dist = 0.1)  # Adjust buffer as needed to include nearby area

# Plot the journey of TERF31 with basemap
journey_map <- ggplot() +
  annotation_map_tile("osm") +  # OpenStreetMap basemap
  geom_sf(data = terf31_sf, color = "blue", size = 1) +  # Plot points without timestamp color
  geom_path(data = terf31_sf, aes(x = long, y = lat), color = "blue", linetype = "dashed") +
  labs(
    title = "Journey of TERF31",
    subtitle = "Mapped locations over time",
    x = "Longitude",
    y = "Latitude",
    caption = "Source: Movement Data"
  ) +
  coord_sf(xlim = c(bbox[[1]], bbox[[3]]), ylim = c(bbox[[2]], bbox[[4]]), expand = FALSE) +  # Zoom in on bounding box
  theme_minimal()

# Print the map to view it in RStudio
print(journey_map)

# Save the map as a PNG file
ggsave("journey_map_TERF31_zoomed.png", plot = journey_map, width = 10, height = 8, dpi = 300)


# Load libraries
library(tidyverse)
library(sf)
library(janitor)
library(ggspatial)

# Clean up the column names
migration_data <- migration_data %>%
  clean_names()

# Filter data for TERF31
terf31_data <- migration_data %>%
  filter(individual_local_identifier == "TERF31") %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))  # Convert to POSIXct if needed

# Convert to sf object with coordinates
terf31_sf <- terf31_data %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Check if bbox is correctly created for zooming
if (nrow(terf31_sf) > 1) {
  bbox <- st_bbox(terf31_sf) %>%
    st_as_sfc() %>%
    st_buffer(dist = 0.1)  # Adjust buffer size as needed
} else {
  stop("Insufficient data to create a bounding box for zooming.")
}

# Plot the journey of TERF31 with basemap
journey_map <- ggplot() +
  annotation_map_tile("osm") +  # OpenStreetMap basemap
  geom_sf(data = terf31_sf, color = "blue", size = 1) +  # Plot points without timestamp color
  geom_path(
    data = terf31_sf, aes(x = long, y = lat),  # Ensure correct column names here
    color = "blue", linetype = "dashed"
  ) +
  labs(
    title = "Journey of TERF31",
    subtitle = "Mapped locations over time",
    x = "Longitude",
    y = "Latitude",
    caption = "Source: Movement Data"
  ) +
  coord_sf(xlim = c(st_bbox(bbox)[1], st_bbox(bbox)[3]), ylim = c(st_bbox(bbox)[2], st_bbox(bbox)[4]), expand = FALSE) + 
  theme_minimal()

# Print the map to view it in RStudio
print(journey_map)

# Save the map as a PNG file
ggsave("journey_map_TERF31_zoomed.png", plot = journey_map, width = 10, height = 8, dpi = 300)

# Load libraries
library(tidyverse)
library(sf)
library(janitor)
library(ggspatial)

# Clean up the column names
migration_data <- migration_data %>%
  clean_names()

# Filter data for TERF31
terf31_data <- migration_data %>%
  filter(individual_local_identifier == "TERF31") %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))  # Convert to POSIXct if needed

# Convert to sf object with coordinates
terf31_sf <- terf31_data %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Check if bbox is correctly created for zooming
if (nrow(terf31_sf) > 1) {
  bbox <- st_bbox(terf31_sf) %>%
    st_as_sfc() %>%
    st_buffer(dist = 0.1)  # Adjust buffer size as needed
} else {
  stop("Insufficient data to create a bounding box for zooming.")
}

# Extract coordinates for use in geom_path
terf31_coords <- terf31_sf %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(long = X, lat = Y) %>%
  bind_cols(terf31_data)  # Add back original data for any additional mapping needs

# Plot the journey of TERF31 with basemap
journey_map <- ggplot() +
  annotation_map_tile("osm") +  # OpenStreetMap basemap
  geom_sf(data = terf31_sf, color = "blue", size = 1) +  # Plot points without timestamp color
  geom_path(
    data = terf31_coords, aes(x = long, y = lat),  # Use coordinates for the path
    color = "blue", linetype = "dashed"
  ) +
  labs(
    title = "Journey of TERF31",
    subtitle = "Mapped locations over time",
    x = "Longitude",
    y = "Latitude",
    caption = "Source: Movement Data"
  ) +
  coord_sf(xlim = c(st_bbox(bbox)[1], st_bbox(bbox)[3]), ylim = c(st_bbox(bbox)[2], st_bbox(bbox)[4]), expand = FALSE) + 
  theme_minimal()

# Print the map to view it in RStudio
print(journey_map)

# Save the map as a PNG file
ggsave("journey_map_TERF31_zoomed.png", plot = journey_map, width = 10, height = 8, dpi = 300)

# Load libraries
library(tidyverse)
library(sf)
library(janitor)
library(ggspatial)

# Clean up the column names
migration_data <- migration_data %>%
  clean_names()

# Filter data for TERF31
terf31_data <- migration_data %>%
  filter(individual_local_identifier == "TERF31") %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))  # Convert to POSIXct if needed

# Convert to sf object with coordinates
terf31_sf <- terf31_data %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Check if bbox is correctly created for zooming
if (nrow(terf31_sf) > 1) {
  bbox <- st_bbox(terf31_sf) %>%
    st_as_sfc() %>%
    st_buffer(dist = 0.1)  # Adjust buffer size as needed
} else {
  stop("Insufficient data to create a bounding box for zooming.")
}

# Extract coordinates and bind necessary data
terf31_coords <- terf31_sf %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(long = X, lat = Y) %>%
  bind_cols(select(terf31_data, -long, -lat))  # Remove duplicate `long` and `lat` columns before binding

# Plot the journey of TERF31 with basemap
journey_map <- ggplot() +
  annotation_map_tile("osm") +  # OpenStreetMap basemap
  geom_sf(data = terf31_sf, color = "blue", size = 1) +  # Plot points without timestamp color
  geom_path(
    data = terf31_coords, aes(x = long, y = lat),  # Use cleaned coordinates for the path
    color = "blue", linetype = "dashed"
  ) +
  labs(
    title = "Journey of TERF31",
    subtitle = "Mapped locations over time",
    x = "Longitude",
    y = "Latitude",
    caption = "Source: Movement Data"
  ) +
  coord_sf(xlim = c(st_bbox(bbox)[1], st_bbox(bbox)[3]), ylim = c(st_bbox(bbox)[2], st_bbox(bbox)[4]), expand = FALSE) + 
  theme_minimal()

# Print the map to view it in RStudio
print(journey_map)                                                                                               
ggsave("journey_map_TERF31_zoomed.png", plot = journey_map, width = 10, height = 8, dpi = 300)

# Check if bbox is correctly created for zooming
if (nrow(terf31_sf) > 1) {
  bbox <- st_bbox(terf31_sf) %>%
    st_as_sfc() %>%
    st_buffer(dist = 0.5)  # Increase buffer size to zoom out more
} else {
  stop("Insufficient data to create a bounding box for zooming.")
}

# Plot the journey of TERF31 with basemap
journey_map <- ggplot() +
  annotation_map_tile("osm") +  # OpenStreetMap basemap
  geom_sf(data = terf31_sf, color = "blue", size = 1) +  # Plot points without timestamp color
  geom_path(
    data = terf31_coords, aes(x = long, y = lat),  # Use cleaned coordinates for the path
    color = "blue", linetype = "dashed"
  ) +
  labs(
    title = "Journey of TERF31",
    subtitle = "Mapped locations over time",
    x = "Longitude",
    y = "Latitude",
    caption = "Source: Movement Data"
  ) +
  coord_sf(xlim = c(st_bbox(bbox)[1], st_bbox(bbox)[3]), ylim = c(st_bbox(bbox)[2], st_bbox(bbox)[4]), expand = FALSE) + 
  theme_minimal()

# Print the map to view it in RStudio
print(journey_map)

# Save the map as a PNG file
ggsave("journey_map_TERF31_zoomed_out.png", plot = journey_map, width = 10, height = 8, dpi = 300)


# Create bounding box with a little extra buffer for zoom level
if (nrow(terf31_sf) > 1) {
  bbox <- st_bbox(terf31_sf)
} else {
  stop("Insufficient data to create a bounding box for zooming.")
}

# Manually adjust the limits for better visualization
zoom_factor <- 0.1  # Increase this value to zoom out more
xlim <- c(bbox['xmin'] - zoom_factor, bbox['xmax'] + zoom_factor)
ylim <- c(bbox['ymin'] - zoom_factor, bbox['ymax'] + zoom_factor)

# Plot the journey of TERF31 with basemap
journey_map <- ggplot() +
  annotation_map_tile("osm") +  # OpenStreetMap basemap
  geom_sf(data = terf31_sf, color = "blue", size = 1) +  # Plot points
  geom_path(data = terf31_coords, aes(x = long, y = lat), color = "blue", linetype = "dashed") +  # Journey path
  labs(
    title = "Journey of TERF31",
    subtitle = "Mapped locations over time",
    x = "Longitude",
    y = "Latitude",
    caption = "Source: Movement Data"
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +  # Manually set limits
  theme_minimal()

# Print the map to view it in RStudio
print(journey_map)

# Save the map as a PNG file
ggsave("journey_map_TERF31_zoomed_out.png", plot = journey_map, width = 10, height = 8, dpi = 300)


