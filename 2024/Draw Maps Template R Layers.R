# Load necessary libraries
library(tidycensus)
library(sf)
library(tigris)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(ggspatial)

# Get world geometries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Get state geometries for the original year
states <- tigris::states(year = 2019, class = "sf")

# Get county geometries for the original year
counties <- tigris::counties(year = 2019, class = "sf")

# Create a data frame for site coordinates
sites <- data.frame(
  longitude = c(-80.144005, -80.109), 
  latitude = c(26.479005, 26.83)
)

# Plot world map with state boundaries and site points for the original year
ggplot(data = world) +
  geom_sf(fill = "antiquewhite1") +
  geom_sf(data = states, fill = NA, color = "black") +
  geom_sf(data = counties, fill = NA, color = "blue") +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

# Get state geometries for a different year
state_geometries <- tigris::states(year = 2021, class = "sf")

# Convert to sf object
state_sf <- st_as_sf(state_geometries)

# Plot state boundaries for a different year
ggplot() +
  geom_sf(data = state_sf, fill = NA, color = "black") +
  theme_minimal()

# Extract state centroids
states_cord <- cbind(state_sf, st_coordinates(st_centroid(state_sf)))

# Set nudge_y values for specific states
states_cord$nudge_y <- -1
states_cord$nudge_y[states_cord$NAME %in% c("Florida", "South Carolina")] <- c(0.5, -1.5)

# Plot world map with state centroids and labels
ggplot(data = world) +
  geom_sf() +
  geom_text(data = states_cord, aes(X, Y, label = NAME), size = 5) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

# Filter Florida counties
florida_counties <- counties %>%
  filter(STATEFP == "12") %>%
  mutate(area = as.numeric(st_area(.)))

# Plot map with Florida counties and labels
ggplot() +
  geom_sf(data = florida_counties, aes(fill = area), alpha = 0.4) +
  scale_fill_viridis_c(trans = "sqrt") +
  geom_text(data = states_cord, aes(X, Y, label = NAME), size = 5, fontface = "bold", nudge_y = states_cord$nudge_y) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

# Create data frame for Florida cities
flcities <- data.frame(
  state = rep("Florida", 5),
  city = c("Miami", "Tampa", "Orlando", "Jacksonville", "Sarasota"),
  lat = c(25.7616798, 27.950575, 28.5383355, 30.3321838, 27.3364347),
  lng = c(-80.1917902, -82.4571776, -81.3792365, -81.655651, -82.5306527)
)

# Convert to sf object
flcities <- st_as_sf(flcities, coords = c("lng", "lat"), remove = FALSE, crs = 4326)

# Plot map with Florida counties, labels, and city points
ggplot() +
  geom_sf(data = states_cord, fill = NA) + 
  geom_sf(data = florida_counties, aes(fill = area)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  geom_label(data = states_cord, aes(X, Y, label = NAME), size = 5, fontface = "bold", 
             nudge_y = states_cord$nudge_y) +
  geom_sf(data = flcities) +
  geom_text_repel(data = flcities, aes(x = lng, y = lat, label = city), 
                  fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, -0.25, 
                                                                                 0.5, 0.5, -0.5)) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

# Assuming sites is your data frame with longitude and latitude columns
sites_sf <- st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326)

# Plot final map with all layers
ggplot(data = world) +
  geom_sf(fill = "antiquewhite1") +
  geom_sf(data = florida_counties, aes(fill = area)) +
  geom_sf(data = states_cord, fill = NA) +
  geom_sf(data = sites_sf, size = 4, shape = 23,fill = "darkred") +
  geom_sf(data = flcities, aes(geometry = geometry), color = "black") +
  geom_text_repel(data = flcities, aes(x = lng, y = lat, label = city),
                  fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, -0.25, 0.5, 0.5, -0.5)) +
  geom_label(data = states_cord, aes(x = X, y = Y, label = NAME),  # Explicitly mention x and y aesthetics
             size = 5, fontface = "bold", nudge_y = states_cord$nudge_y) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Observation Sites", subtitle = "(2 sites in Palm Beach County, Florida)") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))
