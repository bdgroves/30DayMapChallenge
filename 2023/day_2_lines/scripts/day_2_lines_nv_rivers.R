library(sf)
library(tidyverse)
library(leaflet)

# Read the rivers shapefile
rivers_data <- st_read("C:/data/R_Projects/30DayMapChallenge/2023/day_2_lines/data/nv_rivers/nv_rivers.shp")

# Check the column names in your sf object
# Replace 'PNAME' with the actual column name containing river names
print(names(rivers_data))

# Reproject to WGS84
rivers_data <- st_transform(rivers_data, crs = 4326)

# Create a leaflet map centered on Nevada
map <- leaflet(rivers_data) %>%
  setView(lng = -116.4194, lat = 38.5025, zoom = 6)

# Add OpenStreetMap tiles as the background
map <- map %>%
  addTiles() %>%  
  addProviderTiles("OpenStreetMap.Mapnik")

# Add rivers to the map using addPolylines
map <- map %>%
  addPolylines(color = "blue", weight = 2, popup = ~PNAME)  # Replace 'PNAME' with the actual column name

# Add legend
map <- map %>%
  addLegend("bottomright", colors = "blue", labels = "Rivers", opacity = 1)

# Print the map
map


