# Load required libraries
library(sf)                # For handling spatial data
library(ggplot2)           # For plotting
library(rnaturalearth)     # For natural earth shapefiles
library(rnaturalearthdata) # For natural earth data
library(tmap)              # For thematic maps
library(grid)              # For custom annotations

# Step 1: Download Namibia shapefile
namibia <- ne_states(country = "Namibia", returnclass = "sf")

# Filter Caprivi Strip region (Zambezi) from Namibia
caprivi <- namibia[namibia$name == "Zambezi", ]  # Zambezi is the new name for Caprivi

# Step 2: Create data for conflict locations (you can customize this with actual locations)
conflict_zones <- data.frame(
  name = c("Katima Mulilo", "CLA Training Camp", "Refugee Camp"),
  lat = c(-17.5, -17.15, -18.5),  # Example latitudes, you should get more precise ones
  lon = c(24.2, 24.5, 25.2)       # Example longitudes, use precise locations
)

# Convert the conflict locations into sf object for spatial plotting
conflict_sf <- st_as_sf(conflict_zones, coords = c("lon", "lat"), crs = 4326)

# Step 3: Create the map with labels and conflict description
map <- tm_shape(namibia) +
  tm_borders() +
  tm_fill(col = "lightblue") +
  tm_shape(conflict_sf) +
  tm_dots(col = "red", size = 0.5) +
  tm_layout(
    main.title = "Caprivi Conflict Map, Namibia (1994–1999)", 
    main.title.size = 1, 
    title.position = c("left", "top"),
    legend.show = FALSE,
    frame = FALSE
  )

# Step 4: Adjust region names so they fit within their polygons

# Using tm_text with label justification to ensure names fit inside polygons
map <- map + tm_shape(namibia) +
  tm_text("name", size = 0.7, col = "black", shadow = TRUE, 
          just = c("center", "center"))

# Step 5: Add custom description (conflict information) using grid package
# Use base plotting to add a description to the map.
tmap_save(map, "caprivi_conflict_map.png") # Save the map to a file
grid::grid.text(
  "The Caprivi conflict was an armed conflict between the Namibian government and the Caprivi Liberation Army, a rebel group that waged a brief insurrection in 1999 for the secession of the Caprivi Strip.",
  x = 0.5, y = 0.05, gp = gpar(fontsize = 10, col = "black")
)

# Display the map
map
