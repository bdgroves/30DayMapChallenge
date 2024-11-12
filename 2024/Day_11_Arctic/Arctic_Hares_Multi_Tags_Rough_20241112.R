# --- Load Necessary Libraries ---
library(readr)    # For reading CSV files
library(dplyr)    # For data manipulation and cleaning
library(sf)       # For handling spatial (geospatial) data
library(leaflet)  # For creating interactive maps

# --- Load and Prepare the Data ---
# Load the CSV data containing Arctic Hare tracking information.
hare_tracking_data <- read_csv("2024/Day_11_Arctic/Arctic hare Alert - Argos tracking.csv") %>%
  select(timestamp, `location-lat`, `location-long`, `tag-local-identifier`) %>%
  filter(!is.na(`location-lat`) & !is.na(`location-long`)) %>%
  rename(
    lat = `location-lat`,
    long = `location-long`,
    tag_local_identifier = `tag-local-identifier`
  )

# --- Convert Data to Spatial Format ---
# Convert data frame to an sf object with geographic coordinates
hare_sf <- st_as_sf(hare_tracking_data, coords = c("long", "lat"), crs = 4326)

# --- Filter Data for Specific Hares ---
# Filter for specific tag_local_identifiers and extract coordinates from the geometry
tag_ids <- c(153546, 144894, 153525, 153542)  # Tag IDs to include
hare_sf_filtered <- hare_sf %>%
  filter(tag_local_identifier %in% tag_ids) %>%
  mutate(
    lat = st_coordinates(geometry)[, 2],
    long = st_coordinates(geometry)[, 1]
  )

# --- Create Color Palette ---
# Create a color palette based on the tag IDs
color_palette <- colorFactor(palette = "Set1", domain = hare_sf_filtered$tag_local_identifier)

# --- Initialize Interactive Map with Color-Coded Paths and Points ---
# Define and display the map with distinct colors for each tag
map <- leaflet(hare_sf_filtered) %>%
  addTiles()  # Add OpenStreetMap tiles as the base map layer

# Loop through each tag ID, adding lines and points with unique colors
for (tag_id in tag_ids) {
  data_subset <- hare_sf_filtered %>% filter(tag_local_identifier == tag_id)
  
  map <- map %>%
    addPolylines(
      data = data_subset,
      lng = ~long, lat = ~lat,
      color = color_palette(tag_id),
      weight = 2,
      opacity = 0.7
    ) %>%
    addCircleMarkers(
      data = data_subset,
      lng = ~long, lat = ~lat,
      color = color_palette(tag_id),
      radius = 5,
      stroke = FALSE,
      fillOpacity = 0.7,
      popup = ~paste("Timestamp: ", timestamp,
                     "<br>Tag: ", tag_local_identifier,
                     "<br>Latitude: ", lat,
                     "<br>Longitude: ", long)
    )
}

# Add a legend for color-coded tag IDs
map <- map %>%
  addLegend("bottomright", pal = color_palette, values = hare_sf_filtered$tag_local_identifier, title = "Tag ID")

# Display the map
map

# --- Summarize Number of Tags and Records ---
# Generate and display summary of unique tags and their record counts
tag_summary <- hare_tracking_data %>%
  group_by(tag_local_identifier) %>%
  summarise(record_count = n()) %>%
  arrange(desc(record_count))

print(tag_summary)

# --- Create Interactive Map with Title and Save as PNG ---
library(ggplot2)
library(leaflet)
library(RColorBrewer)  # If needed for color palettes

# Define a static version of the map with the title
map_title <- "Day 11: Tracking Arctic Hares on Ellesmere Island – #30DayMapChallenge | Data from MoveBank"

# Define the unique tag identifiers
tag_ids <- unique(hare_sf_filtered$tag_local_identifier)

# Create the map and save it as a PNG
png("Arctic_Hare_Tracking_Day_11.png", width = 800, height = 600)  # Set the dimensions of the PNG

# Create the leaflet map object
map <- leaflet(hare_sf_filtered) %>%
  addTiles()  # Add OpenStreetMap tiles as the base map layer

# Loop over tag_ids and add lines and markers for each tag ID
for (tag_id in tag_ids) {
  data_subset <- hare_sf_filtered %>% filter(tag_local_identifier == tag_id)
  
  map <- map %>%
    addPolylines(
      data = data_subset,
      lng = ~long, lat = ~lat,
      color = color_palette(tag_id),
      weight = 2,
      opacity = 0.7
    ) %>%
    addCircleMarkers(
      data = data_subset,
      lng = ~long, lat = ~lat,
      color = color_palette(tag_id),
      radius = 5,
      stroke = FALSE,
      fillOpacity = 0.7,
      popup = ~paste("Timestamp: ", timestamp,
                     "<br>Tag: ", tag_local_identifier,
                     "<br>Latitude: ", lat,
                     "<br>Longitude: ", long)
    )
}

# Add legend and title
map <- map %>%
  addLegend("bottomright", pal = color_palette, values = hare_sf_filtered$tag_local_identifier, title = "Tag ID") %>%
  addControl(map_title, position = "topright", className = "map-title")  # Add title to the map

# Print the map
map

# Close the PNG device to save the image
dev.off()
