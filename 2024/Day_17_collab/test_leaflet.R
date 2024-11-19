library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("Animal Sightings Map"),
  
  # Main panel with map
  leafletOutput("map", height = "500px")  # Make sure the height is set
)

server <- function(input, output, session) {
  # Render a simple Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>%  # Base map layer
      setView(lng = 0, lat = 0, zoom = 2)  # Initial map center and zoom level
  })
}

shinyApp(ui, server)
