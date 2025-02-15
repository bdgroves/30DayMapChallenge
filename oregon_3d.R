library(shiny)
library(mapgl)

ui <- fluidPage(
  story_map(
    map_id = "map",
    sections = list(
      "intro" = story_section(
        "Introduction",
        "This is a story map."
      ),
      "location" = story_section(
        "Location",
        "Check out this interesting location."
      )
    )
  )
)

server <- function(input, output, session) {
  output$map <- renderMapboxgl({
    mapboxgl(scrollZoom = FALSE)
  })
  
  on_section("map", "location", {
    mapboxgl_proxy("map") |> 
      fly_to(center = c(12.49257, 41.890233), 
             zoom = 17.5,
             pitch = 49,
             bearing = 12.8)
  })
  
}

shinyApp(ui, server)