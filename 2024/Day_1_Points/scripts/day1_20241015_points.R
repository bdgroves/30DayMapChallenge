library(tidyverse)
library(sf)
library(tidycensus)
library(rnaturalearth)

# Get point data for major cities in the Western USA
city_points <- get_acs(geography = "place", variables = "B01003_001",
                       state = c("CA", "OR", "WA"), geometry = TRUE) %>%
  st_as_sf()

# Get Natural Earth data (points like cities, trees, etc.)
ne_cities <- ne_download(scale = "medium", type = "populated_places", category = "cultural", returnclass = "sf")

ggplot() +
  geom_sf(data = city_points, aes(geometry = geometry), color = "blue", size = 2) +
  theme_minimal() +
  labs(title = "Major Cities in Western USA",
       subtitle = "Data from US Census Bureau",
       caption = "30DayMapChallenge - Points")

st_write(city_points, "western_usa_cities.shp")
