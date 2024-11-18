library(tidycensus)
library(mapgl)
library(glue)
library(RColorBrewer)

# Step 1: Get the median income data for Nevada (using ACS data)
nevada_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",  # Median Household Income
  state = "NV",
  year = 2022,
  geometry = TRUE
)

# Step 2: Initialize the map, focused on Nevada
nevada_map <- mapboxgl(mapbox_style("outdoors"),
                       bounds = nevada_income)

# Step 3: Adjust the color scale to fit the actual income range (from 14621 to 208917)
low_income <- 14621
high_income <- 208917

# Step 4: Add the fill layer with a continuous green color scale
nevada_map <- nevada_map |>
  add_fill_layer(
    id = "nevada_tracts",
    source = nevada_income,
    fill_color = interpolate(
      column = "estimate",  # This is the column with the median income data
      values = c(low_income, high_income),  # Min and max values for the color scale
      stops = c("lightgreen", "darkgreen"),  # Colors for low and high income values (green shades)
      na_color = "lightgrey"  # Color for missing data
    ),
    fill_opacity = 0.7
  )

# Step 5: Add pop-ups and tooltips to display median income
nevada_income$popup <- glue::glue(
  "<strong>GEOID: </strong>{nevada_income$GEOID}<br><strong>Median Income: </strong>{nevada_income$estimate}"
)

# Step 6: Add tooltips and highlight effects on hover
nevada_map <- nevada_map |>
  add_fill_layer(
    id = "nevada_tracts",
    source = nevada_income,
    fill_color = interpolate(
      column = "estimate",
      values = c(low_income, high_income),
      stops = c("lightgreen", "darkgreen"),  # Green shades for income
      na_color = "lightgrey"
    ),
    fill_opacity = 0.7,
    popup = "popup",  # Display this in the pop-up on click
    tooltip = "estimate",  # Display median income as tooltip
    hover_options = list(
      fill_color = "blue",  # Highlight in yellow on hover
      fill_opacity = 1  # Full opacity when hovered
    )
  )

# Step 7: Add legend to indicate median income ranges
# Use the actual data range for the legend
nevada_map <- nevada_map |>
  add_legend(
    "Median Income in Nevada",
    values = c(low_income, high_income),  # Min and max income values for the legend
    colors = c("lightgreen", "darkgreen"),  # Green shades for the legend
    type = "continuous"
  )

# Step 8: Render the map
nevada_map
