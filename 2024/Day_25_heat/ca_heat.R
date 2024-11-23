# Load Required Libraries
library(tidycensus)
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(units)

# Step 1: Fetch Population Data at the Block Group Level
census_api_key("your_api_key_here", install = TRUE)
pop_data <- get_acs(
  geography = "block group",
  variables = "B01003_001E",  # Total population
  state = "California",       # Replace with your state
  geometry = TRUE,            # Fetch spatial data
  year = 2020                 # Use the desired Census year
)

# Step 2: Calculate Population Density with Proper Unit Conversion
pop_density <- pop_data %>%
  mutate(
    area_km2 = as.numeric(set_units(st_area(geometry), km^2)),  # Convert to km^2
    pop_density = estimate / area_km2                          # People per km^2
  )

# Step 3: Handle Missing Values and Cap Outliers
pop_density <- pop_density %>%
  filter(!is.na(pop_density))

density_cap <- quantile(pop_density$pop_density, 0.99, na.rm = TRUE)
pop_density <- pop_density %>%
  mutate(pop_density = ifelse(pop_density > density_cap, density_cap, pop_density),
         log_pop_density = log10(pop_density + 1))  # Log transform

# Step 4: Fetch California Boundaries for Background
ca_boundary <- st_union(st_geometry(pop_data))  # Aggregate all block groups into state outline

# Step 5: Plot the Heat Map
ggplot() +
  # Add California background
  geom_sf(data = ca_boundary, fill = "gray90", color = "gray70") +
  # Overlay population density
  geom_sf(data = pop_density, aes(fill = log_pop_density), color = NA) +
  # Customize fill scale
  scale_fill_viridis(option = "inferno", 
                     name = "Log(Population Density)") +
  # Simplify the theme
  theme_void() +  # Removes axes and gridlines
  theme(
    legend.position = "right",         # Keep legend on the side
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  labs(
    title = "Population Density in California",
    subtitle = "Log-transformed block group-level data",
    caption = "Source: U.S. Census Bureau ACS 2020"
  )
