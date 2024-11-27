# Load libraries
library(elevatr)
library(raster)
library(ggplot2)
library(sf)
library(rayshader)

# Load San Marino shapefile (replace with your file path)
san_marino_shapefile <- st_read("san_marino.shp")

# Check the CRS of the shapefile
print(st_crs(san_marino_shapefile))

# Extract bounding box of San Marino
san_marino_bbox <- st_bbox(san_marino_shapefile)
print(san_marino_bbox)

# Convert bounding box to sf object with CRS (same CRS as the shapefile)
san_marino_bbox_sf <- st_as_sf(data.frame(
  x = c(san_marino_bbox[1], san_marino_bbox[3]),
  y = c(san_marino_bbox[2], san_marino_bbox[4])
), coords = c("x", "y"), crs = 4326)

# Download DEM data for the bounding box of San Marino (z = 10 is for medium resolution)
dem <- get_elev_raster(locations = san_marino_bbox_sf, z = 10, clip = "bbox")

# Convert the raster object to a dataframe
dem_df <- as.data.frame(dem, xy = TRUE)

# Rename the column with elevation values to 'layer'
names(dem_df)[3] <- "layer"

# Create the DEM matrix for rayshader
dem_matrix <- matrix(dem_df$layer, nrow = length(unique(dem_df$y)), byrow = TRUE)

# Extract the boundary of San Marino
san_marino_boundary <- st_boundary(san_marino_shapefile)
boundary_coords <- st_coordinates(san_marino_boundary)

# Transform boundary coordinates to match DEM's grid size
boundary_x <- (boundary_coords[,1] - min(dem_df$x)) / (max(dem_df$x) - min(dem_df$x)) * ncol(dem_matrix)
boundary_y <- (boundary_coords[,2] - min(dem_df$y)) / (max(dem_df$y) - min(dem_df$y)) * nrow(dem_matrix)

# Set up the 3D plot with rayshader
dem_matrix %>%
  sphere_shade(texture = "imhof1", zscale = 10) %>%
  add_shadow(shadowmap = rayshader::ray_shade(dem_matrix), 0.5) %>%
  add_shadow(shadowmap = rayshader::ambient_shade(dem_matrix), 0) %>%
  plot_3d(dem_matrix, 
          zscale = 50, 
          fov = 0, 
          theta = -45, 
          phi = 45, 
          windowsize = c(1000, 800), 
          zoom = 0.6, 
          water = TRUE, 
          waterdepth = 0, 
          wateralpha = 0.5, 
          watercolor = "lightblue", 
          waterlinecolor = "white", 
          waterlinealpha = 0.5, 
          baseshape = "circle")

# Visualize DEM using ggplot with the boundary overlay
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  geom_sf(data = san_marino_shapefile, fill = NA, color = "red", size = 1) +  # Add boundary in red
  theme_minimal() +
  labs(title = "Digital Elevation Model of San Marino with Boundary")





