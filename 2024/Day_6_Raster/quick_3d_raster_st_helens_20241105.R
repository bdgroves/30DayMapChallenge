# Load libraries
library(elevatr)
library(raster)
library(sf)
library(rayshader)

# Define a larger bounding box for Mount St. Helens
mountain_coords <- data.frame(
  x = c(-122.4, -122.0),  # Expanded longitude range to cover the mountain
  y = c(46.1, 46.3)       # Expanded latitude range to cover the mountain
)

# Convert bounding box to sf object with CRS
mountain_bbox <- st_as_sf(mountain_coords, coords = c("x", "y"), crs = 4326)

# Download DEM data for the larger bounding box
dem <- get_elev_raster(locations = mountain_bbox, z = 10, clip = "bbox")

# Convert DEM to matrix for rayshader
elev_matrix <- matrix(
  raster::extract(dem, raster::extent(dem), buffer = 1000),
  nrow = ncol(dem), ncol = nrow(dem)
)

# Define a color palette for natural elevation
natural_colors <- c("#006400", "#228B22", "#8B4513", "#A0522D", "#CD853F", "#D2B48C", "#FFFFFF")

# Apply the color palette and plot in 3D
elev_matrix %>%
  height_shade(texture = natural_colors) %>%
  plot_3d(elev_matrix, zscale = 30, fov = 0, theta = 135, phi = 45,
          windowsize = c(1000, 800), zoom = 0.75, background = "lightblue",
          shadowcolor = "gray")

# Add ambient lighting and shadows
ray_shade(elev_matrix, zscale = 30, lambert = TRUE) %>%
  add_shadow(ambient_shade(elev_matrix, zscale = 15), 0.5) %>%
  plot_3d(elev_matrix, zscale = 30, fov = 0, theta = 135, phi = 45,
          windowsize = c(1000, 800), zoom = 0.75, background = "lightblue",
          shadowcolor = "gray")

# Save the snapshot
render_snapshot("mt_st_helens_3d_map.png")
