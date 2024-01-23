library(mapboxapi)
library(leaflet)

isos <- mb_isochrone(
  location = "One Bowerman Dr, Beaverton, OR 97005",
  profile = "driving",
  time = 1:45
)

leaflet() %>%
  addMapboxTiles("streets-v11", "mapbox") %>%
  addPolygons(data = isos)

library(sf)
library(fasterize)

isos_proj <- st_transform(isos, 32618)

template <- raster(isos_proj, resolution = 100)

iso_surface <- fasterize(isos_proj, template, field = "time", fun = "min")

iso_surface

pal <- colorNumeric("plasma", isos$time, na.color = "transparent")

nike_map <- leaflet() %>%
  addMapboxTiles(style_id = "light-v9",
                 username = "mapbox",
                 scaling_factor = "0.5x") %>%
  addRasterImage(iso_surface, colors = pal, opacity = 0.5) %>%
  addLegend(values = isos$time, pal = pal,
            title = "Drive-time from<br>Nike HQ")

nike_map

nike_icon <- makeIcon(
  iconUrl = "https://nike.com/favicon.ico",
  iconWidth = 25,
  iconHeight = 25
)

nike_hq <- mb_geocode("One Bowerman Dr, Beaverton, OR 97005")

nike_map %>%
  addMarkers(lng = nike_hq[1], lat = nike_hq[2], icon = nike_icon)

