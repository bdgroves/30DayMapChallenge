# Load required libraries
library(tidyverse)
library(sf)
library(tidycensus)
library(rnaturalearth)
library(tigris)


# Enable tigris caching to avoid repeated downloads
options(tigris_use_cache = TRUE)

# Get Tuolumne County boundary
tuolumne_boundary <- counties(state = "CA", cb = TRUE, class = "sf") %>%
  filter(NAME == "Tuolumne")

# Ensure Tuolumne County boundary has a CRS set
tuolumne_boundary <- st_transform(tuolumne_boundary, 4326)  # Force CRS to WGS 84 (EPSG:4326)

# Get Census place data (cities/towns) for California
city_points <- get_acs(geography = "place", variables = "B01003_001", 
                       state = "CA", geometry = TRUE) %>%
  st_as_sf()

# Ensure Census places have the same CRS as Tuolumne boundary
city_points <- st_transform(city_points, 4326)

# Filter cities/towns within Tuolumne County
city_points_tuolumne <- st_intersection(city_points, tuolumne_boundary)

# Get Natural Earth populated places data
ne_cities <- ne_download(scale = "medium", type = "populated_places", 
                         category = "cultural", returnclass = "sf")

# Ensure Natural Earth places have the same CRS
ne_cities <- st_transform(ne_cities, 4326)

# Filter Natural Earth places to only those within Tuolumne County
ne_cities_tuolumne <- st_intersection(ne_cities, tuolumne_boundary)

# Create a map
ggplot() +
  geom_sf(data = tuolumne_boundary, fill = NA, color = "black", size = 0.6) +  # County outline
  geom_sf(data = city_points_tuolumne, aes(geometry = geometry), color = "blue", size = 2) +  # Census places
  geom_sf(data = ne_cities_tuolumne, aes(geometry = geometry), color = "red", size = 2, shape = 4) +  # Natural Earth places
  theme_minimal() +
  labs(title = "Cities & Towns in Tuolumne County, CA",
       subtitle = "US Census & Natural Earth Data",
       caption = "30DayMapChallenge - Points")

# Save the points as a Shapefile
st_write(city_points_tuolumne, "tuolumne_county_cities.shp")
