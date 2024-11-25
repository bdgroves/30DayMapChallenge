# Load libraries
library(sf)
library(ggplot2)
library(rnaturalearth)

# Fetch world data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Transform the world data to Azimuthal Equidistant projection
azimuthal_crs <- "+proj=aeqd +lat_0=0 +lon_0=0"  # Azimuthal Equidistant projection
world_azimuthal <- st_transform(world, crs = azimuthal_crs)

# Generate graticules (gridlines) and reproject them into Azimuthal Equidistant projection
graticules_azimuthal <- st_graticule(
  lon = seq(-180, 180, by = 30),  # Longitude lines every 30 degrees
  lat = seq(-90, 90, by = 15)    # Latitude lines every 15 degrees
) %>% 
  st_transform(crs = azimuthal_crs)  # Reproject graticules into Azimuthal Equidistant projection

# Plot the Azimuthal Equidistant projection map with bolder and darker graticules
ggplot() +
  geom_sf(data = graticules_azimuthal, color = "black", linetype = "solid", size = 0.6) +  # Darker and thicker graticules
  geom_sf(data = world_azimuthal, fill = "lightgoldenrod1", color = "gray20", size = 0.3) +  # Land
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "lightskyblue", color = NA),  # Ocean
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = "darkblue")
  ) +
  labs(title = "Azimuthal Equidistant Projection with Bold Graticules")
