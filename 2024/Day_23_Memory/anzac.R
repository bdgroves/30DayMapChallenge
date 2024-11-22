library(osmdata)
library(leaflet)
library(sf)

# Define the bounding box for Sydney
bbox <- c(151.14, -33.9, 151.25, -33.7)  # Approximate bounding box for central Sydney

# Query OSM for memorials tagged as 'war_memorial' in Sydney
osm_query <- opq(bbox) %>%
  add_osm_feature(key = "memorial", value = "war_memorial")

# Extract data (points) from the query result
osm_data <- osmdata_sf(osm_query)

# Extract the memorial points (sf object)
memorials <- osm_data$osm_points

# Convert the coordinates from sf object to a data frame for leaflet compatibility
memorials_df <- as.data.frame(st_coordinates(memorials))

# Add the memorial names to the data frame (from the 'name' column in the sf object)
memorials_df$name <- memorials$name

# Create the leaflet map with circle markers (red circle with black dot)
m <- leaflet(data = memorials_df) %>%
  addTiles() %>%
  
  # Outer red circle marker
  addCircleMarkers(
    ~X, ~Y, 
    popup = ~name,
    radius = 12,  # Size of the outer red circle
    color = "black",  # Border color (black for outer ring)
    fillColor = "red",  # Fill color (red for circle)
    fillOpacity = 1,  # Full opacity for the red fill
    stroke = TRUE,  # Enable stroke for the border
    weight = 2,  # Thickness of the circle border
  ) %>%
  
  # Inner black dot marker
  addCircleMarkers(
    ~X, ~Y, 
    popup = ~name,
    radius = 5,  # Inner dot radius (smaller black circle)
    color = "black",  # Border color (black for inner dot)
    fillColor = "black",  # Fill color (black for dot)
    fillOpacity = 1,  # Full opacity for the black dot
    stroke = TRUE,  # Enable stroke for the black dot border
    weight = 1  # Thickness of the black dot border
  ) %>%
  setView(lng = 151.2093, lat = -33.8688, zoom = 12)

# Show the map
m
