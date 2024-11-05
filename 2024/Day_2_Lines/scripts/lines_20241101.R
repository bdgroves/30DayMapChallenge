# Load packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

# Download US states as an sf object and filter for California
us_states <- ne_states(country = "united states of america", returnclass = "sf")
california <- us_states %>% filter(name == "California")

# Download world rivers and filter to California extent
rivers <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")

# Filter to rivers within California
rivers_ca <- st_intersection(rivers, california)

# Plot California with rivers
ggplot() +
  geom_sf(data = california, fill = "lightblue", color = "black") +
  geom_sf(data = rivers_ca, color = "dodgerblue", size = 0.3) +
  theme_minimal() +
  labs(title = "Rivers of California", 
       caption = "Data: Natural Earth") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.caption = element_text(hjust = 1)
  )