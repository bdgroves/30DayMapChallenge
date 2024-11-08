# Load necessary libraries
library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(tidyverse)

# Load the CSV data
Arctic_hare_Alert_Argos_tracking <- read_csv("2024/Day_11_Arctic/Arctic hare Alert - Argos tracking.csv")

# Convert the tibble (tbl_df) into a regular data frame to avoid compatibility issues with dplyr
hare_tracking_data <- as.data.frame(Arctic_hare_Alert_Argos_tracking)

# Subset the relevant columns and remove rows with NA latitudes or longitudes
hare_tracking_data <- hare_tracking_data %>%
  select(timestamp, `location-lat`, `location-long`, `tag-local-identifier`) %>%
  filter(!is.na(`location-lat`) & !is.na(`location-long`))

# Rename the columns to match leaflet's expected names
colnames(hare_tracking_data) <- c("timestamp", "lat", "long", "tag_local_identifier")

# Convert the dataframe to a spatial object (sf object)
hare_sf <- st_as_sf(hare_tracking_data, coords = c("long", "lat"), crs = 4326)
# Extract the coordinates from the geometry column and add them as separate columns
hare_sf <- hare_sf %>%
  mutate(
    lat = st_coordinates(geometry)[, 2],  # Extract latitude from geometry
    long = st_coordinates(geometry)[, 1]  # Extract longitude from geometry
  )

# Now create the map using leaflet with the extracted lat and long
leaflet(hare_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~long, lat = ~lat,  # Use the extracted lat and long
    popup = ~paste("Timestamp: ", timestamp, 
                   "<br>Tag: ", tag_local_identifier,
                   "<br>Latitude: ", lat,
                   "<br>Longitude: ", long),
    color = ~factor(tag_local_identifier), 
    radius = 3,  # Adjust the radius size for better visibility
    stroke = FALSE,  # Optional: Make the circle borders invisible
    fillOpacity = 0.7  # Optional: Adjust the opacity for better map visibility
  ) %>%
  addLegend("bottomright", 
            pal = colorFactor(rainbow(length(unique(hare_sf$tag_local_identifier)), alpha = 1), 
                              hare_sf$tag_local_identifier),
            values = hare_sf$tag_local_identifier, 
            title = "Tag ID")

# Filter the data for the specific tag_local_identifier (153563)
hare_sf_filtered <- hare_sf %>%
  filter(tag_local_identifier == 153563)

# Now create the map for this specific tag
leaflet(hare_sf_filtered) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~long, lat = ~lat,  # Use the extracted lat and long
    popup = ~paste("Timestamp: ", timestamp, 
                   "<br>Tag: ", tag_local_identifier,
                   "<br>Latitude: ", lat,
                   "<br>Longitude: ", long),
    color = "blue",  # Optional: Color all markers for this tag the same (you can adjust this)
    radius = 5,  # Adjust the radius size for better visibility
    stroke = FALSE,  # Optional: Make the circle borders invisible
    fillOpacity = 0.7  # Optional: Adjust the opacity for better map visibility
  ) %>%
  addLegend("bottomright", 
            pal = colorFactor(rainbow(length(unique(hare_sf_filtered$tag_local_identifier)), alpha = 1), 
                              hare_sf_filtered$tag_local_identifier),
            values = hare_sf_filtered$tag_local_identifier, 
            title = "Tag ID")


# Filter the data for the specific tag_local_identifier (153563)
hare_sf_filtered <- hare_sf %>%
  filter(tag_local_identifier == 153563)

# Define a color palette based on the tag_local_identifier
color_palette <- colorFactor(rainbow(length(unique(hare_sf_filtered$tag_local_identifier)), alpha = 1), 
                             hare_sf_filtered$tag_local_identifier)

# Filter the data for the specific tag_local_identifier (153563)
hare_sf_filtered <- hare_sf %>%
  filter(tag_local_identifier == 153563)

# Create the map with a path connecting the points and using blue color
leaflet(hare_sf_filtered) %>%
  addTiles() %>%
  addPolylines(
    lng = ~long, lat = ~lat,  # Use the longitude and latitude for the path
    color = "#0000FF",  # Set color of the line to blue
    weight = 2,  # Set the thickness of the line
    opacity = 0.7  # Set the opacity of the line
  ) %>%
  addCircleMarkers(
    lng = ~long, lat = ~lat,  # Use the extracted lat and long
    popup = ~paste("Timestamp: ", timestamp, 
                   "<br>Tag: ", tag_local_identifier,
                   "<br>Latitude: ", lat,
                   "<br>Longitude: ", long),
    color = "#0000FF",  # Set color of the points to blue
    radius = 5,  # Adjust the radius size for better visibility
    stroke = FALSE,  # Optional: Make the circle borders invisible
    fillOpacity = 0.7  # Optional: Adjust the opacity for better map visibility
  ) %>%
  addLegend("bottomright", 
            pal = colorFactor(c("#0000FF"), hare_sf_filtered$tag_local_identifier), # Set legend color to blue
            values = hare_sf_filtered$tag_local_identifier, 
            title = "Tag ID")
