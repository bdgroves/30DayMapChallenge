library(shiny)
library(mapgl)

ui <- fluidPage(
  titlePanel("Animal Sightings Map"),
  
  # Main panel with map
  mapboxglOutput("map", height = "500px")  # Make sure the height is set
)

server <- function(input, output, session) {
  # Render a simple Mapbox map
  output$map <- renderMapboxgl({
    mapboxgl(
      access_token = "pk.eyJ1IjoiYnJvb2tzZyIsImEiOiJjbHh4d2FqYWQxZWl1MmlvZnF1N2JjZncxIn0.z3NeBZnns3sUZlHFo_jfow",  # Your Mapbox token
      center = c(0, 0),  # Center map at 0,0
      zoom = 2  # Set initial zoom level
    ) %>%
      add_navigation_control()  # Add basic navigation controls (zoom, rotate)
  })
}

shinyApp(ui, server)
