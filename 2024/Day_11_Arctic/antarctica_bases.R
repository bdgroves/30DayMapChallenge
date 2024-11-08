library(mapgl)
library(sf)
library(dplyr)

# Research base coordinates (updated with additional stations)
stations <- data.frame(
  name = c(
    "McMurdo Station", 
    "Amundsen-Scott South Pole Station", 
    "Concordia Station", 
    "Palmer Station",
    "Vostok Station",
    "Davis Station",
    "Mawson Station",
    "Casey Station",          # Added Casey Station
    "Neumayer Station",       # Added Neumayer Station
    "Scott Base",             # Added Scott Base
    "Maitri Station",         # Added Maitri Station
    "Mirny Station"           # Added Mirny Station
  ),
  lat = c(
    -77.8419,  # McMurdo Station
    -90.0,     # Amundsen-Scott South Pole Station
    -75.1000,  # Concordia Station
    -64.774,   # Palmer Station
    -78.4645,  # Vostok Station
    -68.572,   # Davis Station
    -67.6000,  # Mawson Station
    -66.2833,  # Casey Station
    -70.6500,  # Neumayer Station
    -77.8864,  # Scott Base
    -69.4000,  # Maitri Station
    -66.5667   # Mirny Station
  ),
  lon = c(
    166.6863,  # McMurdo Station
    0.0,       # Amundsen-Scott South Pole Station (fixed longitude)
    123.4000,  # Concordia Station
    -64.050,   # Palmer Station
    106.8714,  # Vostok Station
    77.9667,   # Davis Station
    62.8764,   # Mawson Station
    110.5250,  # Casey Station
    8.2743,    # Neumayer Station
    166.7119,  # Scott Base
    77.0722,   # Maitri Station
    93.5056    # Mirny Station
  )
)

# Assign categories (e.g., by country or research focus)
stations$category <- c(
  "USA",    # McMurdo Station
  "USA",    # Amundsen-Scott South Pole Station
  "France", # Concordia Station
  "USA",    # Palmer Station
  "Russia", # Vostok Station
  "Australia", # Davis Station
  "Australia", # Mawson Station
  "Australia", # Casey Station
  "Germany", # Neumayer Station
  "New Zealand", # Scott Base
  "India",   # Maitri Station
  "Russia"   # Mirny Station
)

# Convert the stations data to an sf object
stations_sf <- st_as_sf(stations, coords = c("lon", "lat"), crs = 4326)

# Map with circle layer and categorized markers
mapboxgl(style = mapbox_style("outdoors"), 
         center = c(0, -90),  # Center on the South Pole
         zoom = 3,            # Zoom to focus on the polar area
         pitch = 75) %>% 
  add_circle_layer(
    id = "stations-layer",
    source = stations_sf,
    circle_color = match_expr(
      "category",
      values = c("USA", "France", "Russia", "Australia", "Germany", "New Zealand", "India"),
      stops = c(
        "#005aa7",  # USA - Blue
        "#0055a4",  # France - Blue
        "#d52b1e",  # Russia - Red
        "#009c44",  # Australia - Green
        "#000000",  # Germany - Black
        "#d71b1b",  # New Zealand - Red
        "#ff9933"   # India - Orange
      )
    ),
    circle_radius = 6,  # Small radius for better visibility
    circle_stroke_color = "#ffffff", # White stroke
    circle_stroke_width = 2,
    circle_opacity = 0.8,  # Slight transparency for a subtle look
    tooltip = "name",  # Tooltip will display the station name
    hover_options = list(circle_radius = 10, circle_color = "#ffff99")  # Hover effect
  ) %>% 
  add_categorical_legend(
    legend_title = "Research Stations",
    values = c("USA", "France", "Russia", "Australia", "Germany", "New Zealand", "India"),
    colors = c(
      "#005aa7",  # USA - Blue
      "#0055a4",  # France - Blue
      "#d52b1e",  # Russia - Red
      "#009c44",  # Australia - Green
      "#000000",  # Germany - Black
      "#d71b1b",  # New Zealand - Red
      "#ff9933"   # India - Orange
    ),
    circular_patches = TRUE
  )
