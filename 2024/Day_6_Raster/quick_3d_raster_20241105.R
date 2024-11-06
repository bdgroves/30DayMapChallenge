# Load libraries
library(elevatr)
library(raster)
library(tmap)
library(ggplot2)
library(sf)
library(rayshader)

# Define a larger bounding box for Mount Rainier
# Increasing the range to ensure the entire mountain is covered
mountain_coords <- data.frame(
  x = c(-121.9, -121.5),  # Expand the longitude range
  y = c(46.6, 47.0)       # Expand the latitude range
)

# Convert bounding box to sf object with CRS
mountain_bbox <- st_as_sf(mountain_coords, coords = c("x", "y"), crs = 4326)
# Note: bbox should be an area, not just a single point

# Download DEM data for the larger bounding box
dem <- get_elev_raster(locations = mountain_bbox, z = 10, clip = "bbox")

# Convert DEM to matrix for rayshader
elev_matrix <- matrix(
  raster::extract(dem, raster::extent(dem), buffer = 1000),
  nrow = ncol(dem), ncol = nrow(dem)
)

# Define a color palette for elevation
natural_colors <- c("#006400", "#228B22", "#8B4513", "#A0522D", "#CD853F", "#D2B48C", "#FFFFFF")

# Apply the color palette and plot in 3D
elev_matrix %>%
  height_shade(texture = natural_colors) %>%
  plot_3d(elev_matrix, zscale = 25, fov = 0, theta = 135, phi = 45,
          windowsize = c(1000, 800), zoom = 0.75, background = "lightblue",
          shadowcolor = "gray")

# Add ambient lighting and shadows
ray_shade(elev_matrix, zscale = 25, lambert = TRUE) %>%
  add_shadow(ambient_shade(elev_matrix, zscale = 10), 0.5) %>%
  plot_3d(elev_matrix, zscale = 25, fov = 0, theta = 135, phi = 45,
          windowsize = c(1000, 800), zoom = 0.75, background = "lightblue",
          shadowcolor = "gray")

# Check your working directory to see where the file will be saved
getwd()

# Save the snapshot to a specific location
render_snapshot("mountain_3d_color__map.png") # Alternatively, specify a path like "/your/directory/mountain_3d_map.png"
