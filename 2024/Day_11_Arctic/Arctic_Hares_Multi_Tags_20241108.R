# --- Load Necessary Libraries ---
library(readr)    # For reading CSV files
library(dplyr)    # For data manipulation and cleaning
library(sf)       # For handling spatial (geospatial) data
library(leaflet)  # For creating interactive maps
library(tidyverse) # For general-purpose data manipulation and visualization

# --- Load and Prepare the Data ---
# Load the CSV data containing Arctic Hare tracking information.
# Ensure that the file path is correct based on your directory structure.
Arctic_hare_Alert_Argos_tracking <- read_csv("2024/Day_11_Arctic/Arctic hare Alert - Argos tracking.csv")

# Convert the tibble (tbl_df) into a regular data frame for compatibility with other functions.
# This ensures that the data frame can be manipulated using dplyr functions.
hare_tracking_data <- as.data.frame(Arctic_hare_Alert_Argos_tracking)

# --- Data Cleaning ---
# Subset the relevant columns: timestamp, latitude, longitude, and tag_local_identifier.
# We also remove rows with missing latitude or longitude values to avoid errors during mapping.
hare_tracking_data <- hare_tracking_data %>%
  select(timestamp, `location-lat`, `location-long`, `tag-local-identifier`) %>%
  filter(!is.na(`location-lat`) & !is.na(`location-long`))

# Rename the columns to more easily readable names that are compatible with Leaflet.
# This step ensures that the column names are consistent with Leaflet's expected names.
colnames(hare_tracking_data) <- c("timestamp", "lat", "long", "tag_local_identifier")

# --- Convert Data to Spatial Format ---
# Convert the data frame to a spatial object (sf object).
# This allows us to work with geographical data and visualize it on a map.
hare_sf <- st_as_sf(hare_tracking_data, coords = c("long", "lat"), crs = 4326)

# Extract the coordinates from the geometry column and add them as separate columns for easier access.
# This step ensures that latitudes and longitudes are explicitly available for use in the map.
hare_sf <- hare_sf %>%
  mutate(
    lat = st_coordinates(geometry)[, 2],  # Extract latitude from the geometry
    long = st_coordinates(geometry)[, 1]  # Extract longitude from the geometry
  )

# --- Filter Data for Two Specific Hares (Tag IDs 153546 and 144894) ---
# Filter the data for the two specific tag_local_identifiers.
hare_sf_filtered <- hare_sf %>%
  filter(tag_local_identifier %in% c(153546, 144894))

# --- Create an Interactive Map with Two Hares in Different Colors ---
# Use Leaflet to create an interactive map to display the movement of the two hares.

leaflet(hare_sf_filtered) %>%
  addTiles() %>%  # Add OpenStreetMap tiles as the base map layer
  addPolylines(
    lng = ~long, lat = ~lat,  # Use the longitude and latitude to connect the points
    color = ~ifelse(tag_local_identifier == 153546, "#0000FF", "#FF0000"),  # Blue for hare 153546, Red for 144894
    weight = 2,  # Set the thickness of the line
    opacity = 0.7  # Set the opacity of the line (makes it slightly transparent)
  ) %>%
  addCircleMarkers(
    lng = ~long, lat = ~lat,  # Use the extracted latitudes and longitudes for the points
    popup = ~paste("Timestamp: ", timestamp,  # Popup displays information about each point
                   "<br>Tag: ", tag_local_identifier,
                   "<br>Latitude: ", lat,
                   "<br>Longitude: ", long),
    color = ~ifelse(tag_local_identifier == 153546, "#0000FF", "#FF0000"),  # Blue for hare 153546, Red for 144894
    radius = 5,  # Set the size of the circle markers for visibility
    stroke = FALSE,  # Remove the circle marker borders
    fillOpacity = 0.7  # Set the opacity of the markers (slightly transparent)
  ) %>%
  addLegend("bottomright",  # Add a color legend in the bottom-right corner of the map
            pal = colorFactor(c("#0000FF", "#FF0000"), hare_sf_filtered$tag_local_identifier),  # Blue and Red for the two hares
            values = hare_sf_filtered$tag_local_identifier,  # Values for the legend (unique tag IDs)
            title = "Tag ID")  # Title of the legend

# --- Summarize the Number of Tags and Records ---
# Summarize the number of unique tags and how many records each tag has.
# This helps to understand how many tracking points exist for each unique hare.
tag_summary <- hare_tracking_data %>%
  group_by(tag_local_identifier) %>%  # Group by unique tag_local_identifier
  summarise(record_count = n()) %>%  # Count the number of records for each tag
  arrange(desc(record_count))  # Sort by the number of records in descending order

# View the summary of tag counts
print(tag_summary)
