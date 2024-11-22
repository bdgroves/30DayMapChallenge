# Load necessary libraries
library(sf)
library(leaflet)
library(tidyverse)

# Load your national parks CSV containing WKT data
nps <- read.csv("C:/data/R_Projects/30DayMapChallenge/2024/Day_24_circles/nps.csv")

# Convert the WKT data to sf objects (polygon geometries)
nps_sf <- st_as_sf(nps, wkt = "WKT")  # Assuming the WKT column for polygons is "WKT...2"

# Ensure the CRS is set to EPSG:4326 (lat/long)
st_crs(nps_sf) <- 4326

# Clean invalid geometries (if any)
nps_sf <- st_make_valid(nps_sf)

# Calculate the area of each park in square kilometers (as polygons have area)
nps_sf$area <- st_area(nps_sf) / 1e6  # Convert area from square meters to square kilometers

# Extract the centroids of the parks (to position the circles)
nps_centroids <- st_centroid(nps_sf)

# Create a size attribute for the circles, based on the area of each park
# The radius of the circle is proportional to the square root of the area
nps_centroids$radius <- sqrt(nps_sf$area) * 500  # Adjust the scaling factor as needed

# Visualize the exaggerated circles using leaflet with area in square km in the popup
leaflet(nps_centroids) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(
    lng = ~st_coordinates(nps_centroids)[,1],  # Longitude of the centroids
    lat = ~st_coordinates(nps_centroids)[,2],   # Latitude of the centroids
    radius = ~radius,  # Use the exaggerated radius
    color = "green",  # Set the circle border color to green
    fillColor = "green",  # Set the fill color to green
    fillOpacity = 0.5,  # Set fill opacity for better visibility
    popup = ~paste(unit_name, "<br>Area: ", round(area, 2), "sq km")  # Add area to the popup
  )