library(tidycensus)
options(tigris_use_cache = TRUE)
library(mapview)
library(magrittr)
library(tidyverse)

# Load ACS variables for the year 2022
v22 <- load_variables(2022, "acs1", cache = TRUE)
v20 <- load_variables(2020, "acs5", cache = TRUE)

tidycensus::get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "CA",
  geometry = TRUE
) %>%
  mapview::mapview(
    zcol = "estimate",
    layer.name = "Median household income"
  )