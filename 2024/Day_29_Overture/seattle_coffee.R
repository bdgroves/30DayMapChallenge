# Load required libraries
library(arrow)
install_arrow()
library(sf)
library(dplyr)
library(tigris)
library(rdeck) # pak::pak("rdeck")
options(tigris_use_cache = TRUE)

# Define the bounding box for Seattle
seattle_bbox <- c(-122.459696, 47.481002, -122.224433, 47.734136) # Rough bounds of Seattle

# Path to the dataset on S3
s3_path <- "s3://overturemaps-us-west-2/release/2024-11-13.0/theme=places/type=place/"

# Load the dataset
places_dataset <- open_dataset(s3_path, format = "parquet")

# Filter data for points of interest in Seattle
seattle_places <- places_dataset |>
  filter(bbox$xmin > seattle_bbox[1],
         bbox$ymin > seattle_bbox[2],
         bbox$xmax < seattle_bbox[3],
         bbox$ymax < seattle_bbox[4]) |>
  select(id, geometry, categories, names) |>
  filter(categories$primary == 'coffee_shop') |> # Example: Filter for coffee shops
  collect() |>
  st_as_sf(crs = 4326)

# Add a readable name for the tooltip
seattle_places$Name <- seattle_places$names$primary

# Set Mapbox token (replace with your actual token)
Sys.setenv(MAPBOX_ACCESS_TOKEN = "your_key")

# Create the map visualization for Seattle
seattle_map <- rdeck(map_style = mapbox_dark(), 
                     initial_view_state = view_state(
                       center = c(-122.335167, 47.608013), # Approximate center of Seattle
                       zoom = 12,
                       pitch = 30
                     )) |> 
  add_scatterplot_layer(
    name = "Seattle Coffee Shops",
    data = seattle_places, 
    get_position = geometry, 
    filled = TRUE,
    get_fill_color = '#29A0B1', # A teal color for coffee shops
    radius_scale = 10,
    pickable = TRUE,
    tooltip = Name
  )

# Display the map
seattle_map




