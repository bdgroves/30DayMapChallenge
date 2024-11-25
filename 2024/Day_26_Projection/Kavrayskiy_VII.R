# Load libraries
library(sf)
library(ggplot2)
library(rnaturalearth)

# Fetch world data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Transform the world data to Kavrayskiy VII projection
kavrayskiy_crs <- "+proj=kav7"  # Kavrayskiy VII projection
world_kavrayskiy <- st_transform(world, crs = kavrayskiy_crs)

# Generate graticules (gridlines) and reproject them into Kavrayskiy VII projection
graticules_kavrayskiy <- st_graticule(
  lon = seq(-180, 180, by = 30),  # Longitude lines every 30 degrees
  lat = seq(-90, 90, by = 15)    # Latitude lines every 15 degrees
) %>% 
  st_transform(crs = kavrayskiy_crs)  # Reproject graticules into Kavrayskiy VII projection

# Plot the Kavrayskiy VII projection map with bolder and darker graticules
ggplot() +
  geom_sf(data = graticules_kavrayskiy, color = "black", linetype = "solid", size = 0.6) +  # Darker and thicker graticules
  geom_sf(data = world_kavrayskiy, fill = "lightgoldenrod1", color = "gray20", size = 0.3) +  # Land
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "lightskyblue", color = NA),  # Ocean
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = "darkblue")
  ) +
  labs(title = "Kavrayskiy VII Projection with Bold Graticules")
