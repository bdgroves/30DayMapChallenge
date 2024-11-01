# Load necessary libraries
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(ggmap)
library(rnaturalearth)
library(png)
library(grid)

# Register your Stadia Maps API key
register_stadiamaps(key = "")

# Load the cleaned data
cleaned_bristlecone_pines <- read_csv("cleaned_bristlecone_pines_output.csv")

# Convert to sf object
cleaned_bristlecone_pines_sf <- cleaned_bristlecone_pines %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Define map boundaries for Western USA
bbox <- c(left = -125, bottom = 31, right = -102, top = 43)

# Get the Stadia terrain basemap with registered API key
terrain_map <- get_stadiamap(bbox, zoom = 6, maptype = "stamen_terrain")

# Define distinct colors for the species
species_colors <- c(
  "Pinus longaeva" = "#006400",   # Dark Green
  "Pinus aristata" = "#B22222",    # Firebrick Red
  "Pinus balfouriana" = "#DAA520"  # Goldenrod
)

# Load image of a bristlecone pine tree (make sure the path is correct)
tree_image <- readPNG("Gnarly_Bristlecone_Pine.png")

# Create a raster object from the image
grob <- rasterGrob(tree_image, interpolate = TRUE)

# Plot the data
ggmap(terrain_map) +
  # Add points for each tree with custom colors
  geom_sf(data = cleaned_bristlecone_pines_sf, aes(color = scientific_name), 
          size = 1.5, inherit.aes = FALSE) +
  
  # Customize map with Wikipedia link in subtitle
  labs(title = "Distribution of Bristlecone Pines in the Western USA",
       subtitle = "For more information on Bristlecone Pines, visit: https://en.wikipedia.org/wiki/Bristlecone_pine",
       color = "Species") +
  scale_color_manual(values = species_colors) +  # Apply custom colors
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title.align = 0.5,  # Center the legend title
    legend.text.align = 0.5,   # Center the legend text
    plot.subtitle = element_text(size = 8),   # Adjust subtitle text size
    axis.text = element_blank(),              # Remove latitude and longitude text
    axis.title = element_blank()              # Remove axis titles
  ) +
  annotation_custom(grob, xmin = -100, xmax = -95, ymin = 45, ymax = 49)  # Adjust position as needed

ggsave("bristlecone_map.png", dpi = 300)

