# Load libraries
library(sf)
library(ggplot2)
library(rnaturalearth)

# Fetch world data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Transform the world data to Bonne (heart-shaped)
bonne_crs <- "+proj=bonne +lat_1=60 +lon_0=0"  # Define the Bonne projection CRS
world_heart <- st_transform(world, crs = bonne_crs)

# Generate graticules (gridlines) and reproject them into Bonne projection
graticules <- st_graticule(
  lon = seq(-180, 180, by = 30),  # Longitude lines every 30 degrees
  lat = seq(-90, 90, by = 15)    # Latitude lines every 15 degrees
) %>% 
  st_transform(crs = bonne_crs)  # Reproject graticules into Bonne projection

# Plot the Bonne projection map with bolder and darker graticules
ggplot() +
  geom_sf(data = graticules, color = "black", linetype = "solid", size = 0.6) +  # Darker and thicker graticules
  geom_sf(data = world_heart, fill = "lightgoldenrod1", color = "gray20", size = 0.3) +  # Land
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "lightskyblue", color = NA),  # Ocean
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "darkblue")
  ) +
  labs(title = "Bonne Projection with Bold Graticules")
