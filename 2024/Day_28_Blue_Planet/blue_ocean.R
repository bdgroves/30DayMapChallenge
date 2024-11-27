# Install required packages if necessary
# install.packages(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata", "rnaturalearthhires"))

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)

# Load the shapefiles for penguin rookeries and albatross nests
penguin_rookeries <- st_read("C:/data/R_Projects/30DayMapChallenge/2024/Day_28_Blue_Planet/Penguin_colonies.shp")
albatross_nests <- st_read("C:/data/R_Projects/30DayMapChallenge/2024/Day_28_Blue_Planet/Albatross_locations.shp")


# Load Natural Earth data
land <- ne_countries(scale = "large", returnclass = "sf")  # Land boundaries
glaciers <- ne_download(
  scale = "large", type = "glaciated_areas", category = "physical", returnclass = "sf"
)  # Glaciers

# Subset South Georgia Island
south_georgia <- subset(land, admin == "South Georgia and the Islands")

# Coordinates for Grytviken
grytviken_coords <- data.frame(
  lon = -36.50806,
  lat = -54.28139
)

ggplot() +
  # Ocean background
  geom_rect(aes(xmin = -40, xmax = -36, ymin = -55, ymax = -54), fill = "#1f78b4") +
  
  # Land areas (muted gray for general land)
  geom_sf(data = land, fill = "gray80", color = "black", size = 0.3) +
  
  # South Georgia Island in a distinct icy blue
  geom_sf(data = south_georgia, fill = "#a3c4dc", color = "#6b93af", size = 0.5) +
  
  # Glaciers in white with slight transparency
  geom_sf(data = glaciers, fill = "white", color = NA, alpha = 0.8) +
  
  # Add a larger black star for Grytviken
  geom_point(data = grytviken_coords, aes(x = lon, y = lat), color = "black", shape = 8, size = 5) +  # Star shape
  
  # Add the label for Grytviken near the point
  geom_text(data = grytviken_coords, aes(x = lon, y = lat, label = "Grytviken"),
            color = "black", size = 4, fontface = "bold", nudge_x = 0.1, nudge_y = 0.1) +
  
  # Add penguin rookery points (using SlateGrey color)
  geom_sf(data = penguin_rookeries, aes(color = "Penguin Rookeries"), size = 2, shape = 16) +
  
  # Add albatross nest points (using orange color)
  geom_sf(data = albatross_nests, aes(color = "Albatross Nests"), size = 2, shape = 17) +
  
  # Map limits and projection
  coord_sf(xlim = c(-40, -36), ylim = c(-55, -54)) +
  
  # Styling
  theme_void() +  # Clean theme
  labs(
    title = "The Icy Wilderness of South Georgia: A Blue Planet Oasis",  # Updated title
    subtitle = "Wildlife Locations, Glaciers, and Grytviken",
    color = "Wildlife"  # Legend title
  ) +
  theme(
    plot.title = element_text(
      size = 18,   # Increased size for emphasis
      hjust = 0.5, 
      face = "bold", 
      family = "serif",  # Serif font for formality
      color = "black"  # Title in white to stand out
    ),
    plot.subtitle = element_text(
      size = 14, 
      hjust = 0.5, 
      family = "serif",  # Serif font for subtitle
      color = "black"  # Complementing title
    ),
    plot.background = element_rect(fill = "#e0f7fa", color = NA),  # Subtle background
    panel.background = element_rect(fill = "#1f78b4", color = NA),  # Ocean panel background
    legend.position = "bottom",  # Positioning the legend
    legend.title = element_text(
      size = 12, 
      face = "bold", 
      color = "black"  # White text for clarity
    ),
    legend.text = element_text(
      size = 10, 
      color = "black"  # Consistent legend styling
    )
  ) +
  
  # Custom colors for wildlife points
  scale_color_manual(values = c("Penguin Rookeries" = "dodgerblue", "Albatross Nests" = "orange"))

# Save the plot as a PNG file
ggsave("south_georgia_blue_planet.png", 
       width = 10, height = 8, dpi = 300, 
       units = "in",  # Size in inches
       bg = "transparent")  # Transparent background for the plot

