# Load libraries
library(sf)
library(ggplot2)
library(rnaturalearth)

# Fetch world data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Transform the projection to Bonne (heart-shaped)
world_heart <- st_transform(
  world, 
  crs = "+proj=bonne +lat_1=45 +lon_0=0"  # Bonne projection parameters
)

# Plot the Bonne projection map
ggplot(data = world_heart) +
  geom_sf(fill = "lightgoldenrod1", color = "gray20", size = 0.3) +  # Land
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "lightskyblue", color = NA),  # Ocean
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  ) +
  labs(title = "Bonne Projection (Heart-Shaped)")
