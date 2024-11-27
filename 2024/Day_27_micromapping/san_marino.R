# Load libraries
library(elevatr)
library(raster)
library(tmap)
library(ggplot2)
library(sf)
library(rayshader)

# Load San Marino shapefile (replace with your file path)
san_marino_shapefile <- st_read("san_marino.shp")

# Check the CRS of the shapefile
print(st_crs(san_marino_shapefile))

# If necessary, reproject to EPSG:4326 (WGS84)
if (st_crs(san_marino_shapefile)$epsg != 4326) {
  san_marino_shapefile <- st_transform(san_marino_shapefile, 4326)
}

# Extract bounding box of San Marino
san_marino_bbox <- st_bbox(san_marino_shapefile)

# Print the bounding box to check
print(san_marino_bbox)

# Visualize the bounding box to ensure it's correct
bbox_sf <- st_as_sf(data.frame(
  x = c(san_marino_bbox[1], san_marino_bbox[3]),
  y = c(san_marino_bbox[2], san_marino_bbox[4])
), coords = c("x", "y"), crs = 4326)

ggplot() +
  geom_sf(data = san_marino_shapefile, fill = "lightblue") +
  geom_sf(data = bbox_sf, color = "red", size = 1, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Bounding Box of San Marino")

# Convert bounding box to sf object with CRS (same CRS as the shapefile)
san_marino_bbox_sf <- st_as_sf(data.frame(
  x = c(san_marino_bbox[1], san_marino_bbox[3]),
  y = c(san_marino_bbox[2], san_marino_bbox[4])
), coords = c("x", "y"), crs = 4326)

# Download DEM data for the bounding box of San Marino (z = 10 is for medium resolution)
dem <- get_elev_raster(locations = san_marino_bbox_sf, z = 10, clip = "bbox")

# Convert the raster object to a dataframe
dem_df <- as.data.frame(dem, xy = TRUE)

# Check the structure of the dataframe
head(dem_df)

# Rename the column with elevation values to 'layer'
names(dem_df)[3] <- "layer"

# Visualize DEM using ggplot
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Digital Elevation Model of San Marino")


# Create the DEM matrix for rayshader
dem_matrix <- matrix(dem_df$layer, nrow = length(unique(dem_df$y)), byrow = TRUE)

# Set up plotting layout
par(mfrow = c(1, 2))

# First 3D plot (with shading and water effect)
dem_matrix %>%
  sphere_shade(texture = "imhof1", zscale = 10) %>%  # Apply sphere shade with texture
  add_shadow(shadowmap = rayshader::ray_shade(dem_matrix), 0.5) %>%  # Add directional shadow
  add_shadow(shadowmap = rayshader::ambient_shade(dem_matrix), 0) %>%  # Add ambient shadow
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

# Second 3D plot with different zscale or other parameters (optional customization)
dem_matrix %>%
  sphere_shade(texture = "imhof1", zscale = 10) %>%  # Apply sphere shade with texture
  add_shadow(shadowmap = rayshader::ray_shade(dem_matrix), 0.5) %>%  # Add directional shadow
  add_shadow(shadowmap = rayshader::ambient_shade(dem_matrix), 0) %>%  # Add ambient shadow
  plot_3d(dem_matrix, 
          zscale = 40, 
          fov = 30, 
          theta = 90, 
          phi = 45, 
          windowsize = c(1000, 800), 
          zoom = 0.8, 
          water = TRUE, 
          waterdepth = 0, 
          wateralpha = 0.5, 
          watercolor = "lightblue", 
          waterlinecolor = "white", 
          waterlinealpha = 0.5, 
          baseshape = "circle")
