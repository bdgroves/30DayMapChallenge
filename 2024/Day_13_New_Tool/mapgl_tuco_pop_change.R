library(tidycensus)
library(sf)
library(tidyverse)
library(mapgl)
options(tigris_use_cache = TRUE)

# Fetch Population Density for 2000 Census (Decennial)
popdensity_00 <- get_decennial(
  geography = "tract",
  state = "CA",  # California
  county = "Tuolumne",  # Tuolumne County
  variables = "P001001",  # Total population variable
  sumfile = "sf1",  # Summary file
  year = 2000,
  geometry = TRUE
) %>%
  st_transform(26910) %>%  # Use a relevant CRS for California (NAD83 / UTM zone 10N)
  mutate(pop_density = as.numeric(value / (st_area(.) / 2589989.1738453)))  # Area in square miles

# Fetch Population Density for 2022 ACS (American Community Survey)
popdensity_22 <- get_acs(
  geography = "tract",
  state = "CA",  # California
  county = "Tuolumne",  # Tuolumne County
  variables = "B01001_001",  # Total population variable
  year = 2022,
  geometry = TRUE
) %>%
  st_transform(26910) %>%  # Use same CRS
  mutate(pop_density = as.numeric(estimate / (st_area(.) / 2589989.1738453)))  # Area in square miles

# Map for 2000 Census Population Density
map1 <- mapboxgl(bounds = popdensity_00) %>%
  add_fill_extrusion_layer(
    id = "pop2000",
    source = popdensity_00,
    fill_extrusion_color = interpolate(
      "pop_density",
      values = c(0, 48000),
      stops = c("#FFD580", "#FF5733")
    ),
    fill_extrusion_height = get_column("pop_density"),
    fill_extrusion_opacity = 0.9
  ) %>%
  add_continuous_legend(
    "Population density (people/sqmi)<br>2000 Census / 2018-22 ACS",
    values = c("0k", "48k"),
    colors = c("#FFD580", "#FF5733")
  )

# Map for 2022 ACS Population Density
map2 <- mapboxgl(bounds = popdensity_22) %>%
  add_fill_extrusion_layer(
    id = "pop2022",
    source = popdensity_22,
    fill_extrusion_color = interpolate(
      "pop_density",
      values = c(0, 48000),
      stops = c("#FFD580", "#FF5733")
    ),
    fill_extrusion_height = get_column("pop_density"),
    fill_extrusion_opacity = 0.9
  )

# Compare the two maps (2000 and 2022 data)
comp <- compare(map1, map2)

