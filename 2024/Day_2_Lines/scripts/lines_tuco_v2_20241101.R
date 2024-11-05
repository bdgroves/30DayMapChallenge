# Load necessary libraries
library(dplyr)
library(readr)
library(sf)
library(ggmap)
library(ggplot2)
library(tigris)
library(nhdplusTools)

# Register your Stadia Maps API key
register_stadiamaps(key = "")

# Define the starting point along the Tuolumne River
start_point <- st_sfc(st_point(c(-120.461, 37.664)), crs = 4269)  # near La Grange, CA

# Find the COMID (Catchment ID) at this location
start_comid <- discover_nhdplus_id(start_point)

# Get upstream tributaries up to 1000 km
flowline <- navigate_nldi(
  list(featureSource = "comid", featureID = start_comid),
  mode = "upstreamTributaries",
  distance_km = 1000
)

# Download a subset of NHDPlus data based on these tributaries
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(
  comids = as.integer(flowline$UT$nhdplus_comid),
  output_file = subset_file,
  nhdplus_data = "download",
  flowline_only = FALSE,
  return_data = TRUE,
  overwrite = TRUE
)

# Read downloaded data into R
flowline_data <- subset$NHDFlowline_Network

# Get boundaries for Tuolumne County
tuolumne_county <- counties(state = "CA", cb = TRUE) %>%
  filter(NAME == "Tuolumne")

# Calculate the bounding box for Tuolumne County
county_bbox <- st_bbox(tuolumne_county)
bbox <- c(left = floor(county_bbox["xmin"]), bottom = floor(county_bbox["ymin"]), 
          right = ceiling(county_bbox["xmax"]), top = ceiling(county_bbox["ymax"]))

# Get the Stadia terrain basemap with registered API key
terrain_map <- get_stadiamap(bbox, zoom = 9, maptype = "stamen_terrain")

# Plot with ggmap
ggmap(terrain_map) +
  # Add Tuolumne County boundaries
  geom_sf(data = tuolumne_county, fill = NA, color = "black", size = 0.5, inherit.aes = FALSE) +
  # Overlay Tuolumne River upstream tributaries
  geom_sf(data = flowline_data, color = "blue", size = 0.8, inherit.aes = FALSE) +
  # Add a red point for the starting location
  geom_sf(data = start_point, color = "red", size = 2, shape = 16, inherit.aes = FALSE) +
  # Add titles and captions
  labs(
    title = "Tuolumne River and Upstream Tributaries in Tuolumne County",
    subtitle = "Stadia Maps Basemap with NHDPlus Data Overlay",
    caption = "Data from NHDPlus and TIGRIS, Basemap from Stadia Maps"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

# Save the map
ggsave("tuolumne_county_map.png", dpi = 300)
