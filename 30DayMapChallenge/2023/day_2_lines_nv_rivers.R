library(leaflet)
library(sf)
library(rnaturalearth)

# Download rivers data for the entire United States
us_rivers <- ne_download(
  scale = 10,
  type = "rivers_lake_centerlines",
  category = "physical",
  returnclass = "sf"
)
