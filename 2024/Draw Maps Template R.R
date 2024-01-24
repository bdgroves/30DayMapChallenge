# Load required libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)
library(ggspatial)

# Load world data
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_make_valid(world)

# Calculate centroids for world
world_points <- st_centroid(world)
world_points <- cbind(world_points, st_coordinates(st_centroid(world)$geometry))

# Set theme
theme_set(theme_bw())

# Data and basic plot
ggplot(data = world) +
  geom_sf()

# Title, subtitle, and axis labels
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))

# Map color
ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Projection and extent
ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+init=epsg:3035")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(3035))

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

# Scale bar and North arrow
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))

## Scale on map varies by more than 10%, scale bar may be inaccurate

# Country names and other names
ggplot(data = world) +
  geom_sf() +
  geom_text(data = world_points, aes(x = X, y = Y, label = name),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", 
           fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

# Final map
map <- ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_text(data = world_points, aes(x = X, y = Y, label = name),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico",
           fontface = "italic", color = "grey22", size = 6) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Map of the Gulf of Mexico and the Caribbean Sea") +
  theme(
    panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", linewidth = 0.5),
    panel.background = element_rect(fill = "aliceblue")
  )

#Saving the map with ggsave

ggsave("map.pdf")
ggsave("map_web.png", width = 6, height = 6, dpi = "screen")
