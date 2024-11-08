library(tmap)
library(sf)
library(rnaturalearth)
library(RColorBrewer)

# Load the map data for the United States and filter for Nevada
nevada <- ne_states(country = "United States of America") %>%
  dplyr::filter(name == "Nevada")

# Create a vintage-style map using tmap
tm_shape(nevada) +
  tm_borders(lwd = 2, col = "black") +  # Add black borders
  tm_fill(col = "wheat", alpha = 0.5) +  # Old parchment color for the land
  tm_layout(
    frame = FALSE,  # Remove the frame
    inner.margins = c(0, 0, 0, 0),  # No inner margins
    legend.show = FALSE,  # Hide legend for that vintage look
    bg.color = "oldlace",  # Correct argument for background color
    fontfamily = "serif",  # Old-style serif font
    fontface = "bold",  # Bold font for titles
    title = "Nevada - Vintage Map",  # Add title to map
    title.size = 1.2,  # Size of title
    title.fontface = "italic"  # Italicize title for that antique feel
  )


