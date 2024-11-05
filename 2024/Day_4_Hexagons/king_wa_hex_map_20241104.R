# Load required libraries
library(lehdr)
library(sf)
library(mapgl)
library(tigris)
library(dplyr)
library(h3)  # remotes::install_github("crazycapivara/h3-r")
options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)  # Disable s2 for spatial calculations if needed

# Step 1: Get jobs data by block for Washington, focusing on healthcare (CNS16)
wa_wac <- grab_lodes(
  state = "wa",
  year = 2021,
  lodes_type = "wac",
  agg_geo = "block",
  use_cache = TRUE
) %>%
  select(GEOID = w_geocode, healthcare = CNS16)

# Step 2: Grab King County blocks and convert to points
king_blocks <- blocks("WA", "King", year = 2024) %>%
  select(GEOID = GEOID20) %>%
  st_point_on_surface()

# Step 3: Join block points and WAC data, then transform to WGS84 (EPSG:4326) for Mapbox compatibility
king_wac_geo <- left_join(king_blocks, wa_wac, by = "GEOID") %>%
  st_transform(4326)

# Step 4: Get King County boundaries and create H3 hexagons at resolution level 7
king_county <- counties("WA", cb = TRUE) %>%
  filter(NAME == "King")

hexagons <- polyfill(king_county, res = 7)
hex_sf <- h3_to_geo_boundary_sf(hexagons)

# Step 5: Aggregate healthcare job data by hexagon
# - Use spatial join to assign healthcare job counts to each hexagon
hex_health_jobs <- st_join(hex_sf, king_wac_geo) %>%
  st_drop_geometry() %>%  # Drop geometry for faster aggregation
  summarize(health_jobs = sum(healthcare, na.rm = TRUE), .by = h3_index)

# Rejoin summarized data to hexagons with geometry
hex_jobs_sf <- left_join(hex_sf, hex_health_jobs, by = "h3_index")

# Step 6: Create a 3D Mapbox visualization with hexagons colored by health jobs count
hex_map <- mapboxgl(style = mapbox_style("light"),
                    customAttribution = "Data source: <a href='https://github.com/jamgreen/lehdr'>LODES / lehdr R package</a>") %>%
  fit_bounds(hex_jobs_sf, pitch = 60, bearing = 30) %>%
  add_fill_extrusion_layer(
    id = "health-jobs",
    source = hex_jobs_sf,
    fill_extrusion_color = interpolate(
      column = "health_jobs",
      values = c(0, 100, 1000, 5000, 15000),
      stops = c("#f7fbff", "#deebf7", "#9ecae1", "#3182bd", "#08519c")
    ),
    fill_extrusion_height = interpolate(
      column = "health_jobs",
      values = c(0, 15000),
      stops = c(0, 15000)  # Adjust max height as needed for King County data range
    ),
    fill_extrusion_opacity = 0.8,
    tooltip = "health_jobs",
    hover_options = list(
      fill_extrusion_color = "yellow"
    )
  ) %>%
  add_legend(
    legend_title = "Health Care Jobs, 2021 LODES<br><span style='font-size: 80%; font-weight: normal;'>King County, Washington</span>",
    colors = c("#f7fbff", "#deebf7", "#9ecae1", "#3182bd", "#08519c"),
    values = c("0", "100", "1k", "5k", "15k")
  )

# Display the interactive map
hex_map

# Save the map as an HTML file for sharing
htmlwidgets::saveWidget(hex_map, "day-4-hexagons-king-county/index.html", selfcontained = FALSE)
