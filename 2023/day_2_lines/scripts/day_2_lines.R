# Load the required libraries
library(tigris)
library(leaflet)
library(sf)

# Function to colorize roads based on the RTTYP column
colorize_roads <- function(rdtype) {
  case_when(
    rdtype == "I" ~ "blue",      # Interstate highways
    rdtype == "U" ~ "green",     # U.S. highways
    rdtype == "S" ~ "orange",    # State highways
    rdtype == "C" ~ "red",       # County roads
    TRUE ~ "grey"                # Default color for other road types
  )
}

# Download the boundaries of all states
states_sf <- st_as_sf(tigris::states(cb = TRUE))

# Filter for Nevada
nevada_sf <- st_transform(states_sf[states_sf$STUSPS == "NV", ], crs = "+proj=longlat +datum=WGS84")

# Load highway shapefile for Nevada with explicit projection transformation
nevada_highways <- st_read("C:/data/R_Projects/30DayMapChallenge/2023/day_2_lines/data/tl_2022_32_prisecroads/tl_2022_32_prisecroads.shp") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

# Create a leaflet map centered on Nevada
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = nevada_sf, fillColor = "grey", color = "black", weight = 0.5) %>%
  addPolylines(data = nevada_highways,
               color = "blue",  # Adjust the color as needed
               weight = 2) %>%
  setView(lng = -116.4194, lat = 38.8026, zoom = 6)  # Adjust the coordinates and zoom level

# Create a leaflet map centered on Nevada with color-coded roads
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = nevada_sf, fillColor = "grey", color = "black", weight = 0.5) %>%
  addPolylines(data = nevada_highways,
               color = ~colorize_roads(RTTYP),  # Use a function to colorize roads
               weight = 2) %>%
  setView(lng = -116.4194, lat = 38.8026, zoom = 6)  # Adjust the coordinates and zoom level
