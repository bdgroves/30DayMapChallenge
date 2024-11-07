# Load necessary libraries
install.packages(c("tmap", "sf", "rnaturalearth", "rnaturalearthdata", "RColorBrewer"))

library(tmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

# Define expanded key points along the Pony Express route with reduced labels
route_coords <- data.frame(
  name = c(
    "St. Joseph, MO",  # Missouri
    "Fort Kearny, NE",  # Nebraska
    "Fort Laramie, WY",  # Wyoming
    "Salt Lake City, UT",  # Utah
    "Carson City, NV",  # Nevada
    "Sacramento, CA"  # California
  ),
  lon = c(
    -94.8467, -99.0848, -104.547, -111.8910, -119.7674, -121.4944
  ),
  lat = c(
    39.7675, 40.6994, 42.755, 40.7608, 39.1638, 38.5816
  )
)

# Convert coordinates to an sf object
route_sf <- st_as_sf(route_coords, coords = c("lon", "lat"), crs = 4326)

# Create a line object for the route
route_line <- route_sf %>%
  st_union() %>%
  st_cast("LINESTRING")

# Define the bounding box around the route and add a buffer for more context
bbox_route <- st_bbox(route_sf)
buffer_distance <- 15  # Buffer in degrees (adjust as needed)
expanded_bbox <- bbox_route + c(-buffer_distance, -buffer_distance, buffer_distance, buffer_distance)

# Get USA basemap and crop to expanded bounding box for more states
usa <- ne_countries(scale = "medium", country = "United States of America", returnclass = "sf")
usa_crop <- st_crop(usa, expanded_bbox)

# Get state boundaries for the USA
states <- ne_states(country = "United States of America", returnclass = "sf")
states_crop <- st_crop(states, expanded_bbox)

# Create the vintage-style map with state boundaries and antique styling
vintage_map <- tm_shape(usa_crop) +
  tm_fill(col = "wheat", alpha = 0.7) +  # Parchment color for land
  
  # Add state boundaries
  tm_shape(states_crop) +
  tm_borders(col = "saddlebrown", lwd = 0.7) +  # Antique brown for state borders
  
  # Add the Pony Express route as a dashed line
  tm_shape(route_line) +
  tm_lines(col = "sienna", lwd = 2.5, lty = "dashed") +  # Antique brown, dashed line for route
  
  # Add points for each selected key location along the route
  tm_shape(route_sf) +
  tm_dots(col = "black", size = 0.5) +  # Points for each key location
  tm_text("name", size = 0.6, fontface = "italic", col = "black", just = "left", xmod = 0.5) +  # Labels to the right
  
# Apply the vintage layout settings with title centered slightly lower
tm_layout(
  frame = FALSE,  # Remove the frame for a softer look
  bg.color = "oldlace",  # Background color for a parchment effect
  inner.margins = c(0.05, 0.05, 0.1, 0.05),  # Inner margins for padding
  outer.margins = c(0.1, 0.05, 0, 0.05),  # Adjusted top outer margin for slight downward shift
  legend.show = FALSE,  # Hide legend for vintage style
  title = "Pony Express Route - 1860-1861",  # Title
  title.position = c(0.25, 0.70),  # Center horizontally and position slightly down from top
  title.size = 1.5,  # Title size
  title.fontface = "italic",  # Italic title for vintage look
  fontfamily = "serif"  # Serif font for that old-world feel
)


# Display the map
print(vintage_map)

# Save the map as a PNG file
tmap_save(vintage_map, "pony_express_route_vintage_terrain_map.png")
