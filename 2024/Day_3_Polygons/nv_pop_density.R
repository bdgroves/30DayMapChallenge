# Load required libraries
library(tidyverse)
library(tidycensus)
library(sf)

# Set up tidycensus with your API key
# census_api_key("your_api_key_here", install = TRUE)  # Uncomment and replace with your API key if needed

# Retrieve population data with geometry for Nevada counties
# Using the 2020 Census total population variable (B01003_001)
nv_population <- get_acs(
  geography = "county",
  state = "NV",
  variables = "B01003_001",  # Total population variable
  year = 2020,
  geometry = TRUE
)

# Transform CRS for accurate area calculation and calculate population density
# - Convert area to square kilometers
# - Calculate population density per square kilometer
nv_population <- nv_population %>%
  st_transform(crs = 26911) %>%  # Use UTM zone 11N for Nevada to calculate area in meters accurately
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6,  # Convert area from square meters to km² and make numeric
    pop_density = estimate / area_km2                # Calculate population density (people per km²)
  )

# Create the population density map using Nevada-themed colors and no lat-long labels
nevada_map <- ggplot(nv_population) +
  geom_sf(aes(fill = pop_density), color = "white", size = 0.3) +
  scale_fill_gradientn(
    colors = c("#f4a261", "#e76f51", "#bc6c25", "#264653"),  # Desert-inspired palette
    name = "Pop Density\n(per km²)"
  ) +
  labs(
    title = "Population Density of Nevada by County",
    subtitle = "Data from 2020 Census, visualized with {tidycensus}",
    caption = "Source: US Census Bureau"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major = element_blank(),       # Remove gridlines
    panel.grid.minor = element_blank(),       # Remove minor gridlines
    axis.text = element_blank(),              # Remove latitude and longitude labels
    axis.title = element_blank()              # Remove axis titles
  )

# Display the map
print(nevada_map)

# Save the map to a PNG file for sharing
ggsave("nevada_population_density.png", plot = nevada_map, width = 10, height = 8, dpi = 300)
