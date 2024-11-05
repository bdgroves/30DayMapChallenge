# Load required libraries
library(nhdplusTools)
library(sf)
library(ggplot2)
library(tigris)  # for state and county boundaries
library(dplyr)

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

# Plot using ggplot2
ggplot() +
  # Tuolumne County boundaries
  geom_sf(data = tuolumne_county, fill = "lightgray", color = "black") +
  # Tuolumne River upstream tributaries
  geom_sf(data = flowline_data, color = "blue", size = 0.7) +
  # Starting point
  geom_sf(data = start_point, color = "red", size = 2, shape = 16) +
  # Map labels and themes
  labs(
    title = "Tuolumne River and Upstream Tributaries in Tuolumne County",
    subtitle = "Showing Flowlines from NHDPlus",
    caption = "Data from NHDPlus and TIGRIS"
  ) +
  theme_minimal() +
  # Use the bounding box of Tuolumne County to set limits
  coord_sf(xlim = c(county_bbox[1], county_bbox[3]), ylim = c(county_bbox[2], county_bbox[4])) +
  # Remove axes text and titles
  theme(
    axis.text = element_blank(),   # Remove axis text
    axis.title = element_blank(),   # Remove axis titles
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )

# Save the map if desired
ggsave("tuolumne_county_map_no_axes.png", dpi = 300)
