# Load libraries
library(sf)
library(ggplot2)
library(gganimate)
library(rnaturalearth)
library(dplyr)

# Fetch world data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Verify and set CRS for the world dataset
if (is.na(st_crs(world))) {
  st_crs(world) <- 4326  # WGS 84
}

# Ensure geometries are valid
world <- st_make_valid(world)

# Define projections
projections <- list(
  Bonne = "+proj=bonne +lat_1=30 +lon_0=-0",   # Bonne (heart-shaped)
  EqualEarth = "+proj=eqearth",               # Equal Earth
  Mercator = "+proj=merc",                    # Mercator
  Mollweide = "+proj=moll",                   # Mollweide
  Orthographic = "+proj=ortho +lat_0=30 +lon_0=-60", # Orthographic
  Robinson = "+proj=robin",                   # Robinson
  Azimuthal = "+proj=aeqd +lat_0=0 +lon_0=0"  # Azimuthal Equidistant
)

# Apply each projection and combine into one dataset
projection_data <- lapply(names(projections), function(proj_name) {
  tryCatch({
    # Transform world dataset to the specified projection
    transformed <- st_transform(world, crs = projections[[proj_name]])
    
    # Neutralize CRS metadata
    transformed <- st_set_crs(transformed, NA)
    
    # Add a column for the projection name
    transformed <- transformed %>% mutate(projection = proj_name)
    
    return(transformed)
  }, error = function(e) {
    message(sprintf("Projection %s failed: %s", proj_name, e$message))
    return(NULL)
  })
})

# Combine all data while excluding NULL results
animated_world <- bind_rows(projection_data)

# Validate final data structure
if (nrow(animated_world) == 0) {
  stop("No valid data for animation.")
}

# Create the animated map
anim <- ggplot(data = animated_world) +
  geom_sf(aes(fill = projection), color = "darkblue", size = 0.5) +  # Increase size of geometries
  theme_void() +
  labs(
    title = "{closest_state} Projection",
    subtitle = "Exploring different map projections",
    caption = "Data: rnaturalearth"
  ) +
  transition_states(projection, transition_length = 0.5, state_length = 1)  # Faster transitions

# Render and save the animation with a larger output size
animate(anim, fps = 20, width = 1200, height = 900, renderer = gifski_renderer("animated_projections_larger.gif"))
