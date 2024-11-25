# Load libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)

# Fetch world data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
# Set CRS for the world dataset
st_crs(world) <- 4326  # WGS 84, the default geographic CRS

# Transform the projection to Bonne (heart-shaped)
world_heart <- st_transform(
  world, 
  crs = "+proj=bonne +lat_1=30 +lon_0=-0"
)

# Create the map using ggplot2
ggplot(data = world_heart) +
  geom_sf(fill = "lightblue", color = "darkblue", size = 0.2) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightgray", color = NA)) +
  labs(title = "Bonne (heart-shaped) Map Projection")