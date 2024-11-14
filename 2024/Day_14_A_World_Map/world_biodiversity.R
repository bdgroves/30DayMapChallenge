# Load necessary libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rworldmap)

# Load world map data with country boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter relevant columns from biodiversity data and merge with world map
biodiversity_data <- countryExData[, c("ISO3V10", "Country", "BIODIVERSITY")]
world <- merge(world, biodiversity_data, by.x = "iso_a3", by.y = "ISO3V10", all.x = TRUE)

# Plot biodiversity map with 6 bins and Mollweide projection
biodiversity_map <- ggplot(data = world) +
  geom_sf(aes(fill = BIODIVERSITY), color = "white", size = 0.2) +
  scale_fill_fermenter(
    palette = "Greens",          # Color scheme for biodiversity
    direction = 1,               # Light-to-dark gradient
    breaks = seq(0, 100, by = 20), # 6 bins (0, 20, 40, 60, 80, 100)
    na.value = "grey90",         # Set NA values to light grey
    name = "Biodiversity Score"
  ) +
  labs(
    title = "Global Biodiversity Map",
    subtitle = "Biodiversity scores by country (Mollweide projection)"
  ) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),   # Set the background to white
    plot.title = element_text(size = 24, face = "bold", family = "Roboto", hjust = 0.5),   # Large title, bold, global font
    plot.subtitle = element_text(size = 16, family = "Roboto", hjust = 0.5),  # Subtitle with regular font
    plot.caption = element_text(size = 10, family = "Roboto", hjust = 0.5, face = "italic"), # Source caption in italics
    legend.position = "right", 
    legend.text = element_text(family = "Roboto"),   # Legend text in global font
    axis.text = element_blank(),                    # Remove axis text as we are working with a map
    axis.ticks = element_blank()                     # Remove axis ticks
  ) +
  coord_sf(crs = "+proj=moll") +
  # Add data source as a caption
  labs(caption = "Data Source: rnaturalearth, CountryExData, Data as of 2024")

# Save the plot as PNG with appropriate resolution for Twitter
ggsave(
  filename = "biodiversity_map_twitter.png",  # File name
  plot = biodiversity_map,                   # Plot object
  width = 12,                                # Width of the image
  height = 8,                                # Height of the image
  dpi = 300,                                 # Resolution for clarity
  units = "in"                               # Units for dimensions
)

# Display the map
print(biodiversity_map)
