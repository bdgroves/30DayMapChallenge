volcano_data <- data.frame(
  name = c("Mount Rainier", "Mount Adams", "Mount St. Helens", "Glacier Peak", "Mount Baker"),
  longitude = c(-121.7604, -121.4905, -122.1149, -121.1137, -121.8144),
  latitude = c(46.8523, 46.2029, 46.1914, 48.1120, 48.7768)
)

library(shiny)
library(mapgl)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Washington Cascade Volcanoes Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("volcano", "Select Volcano:", choices = volcano_data$name),
      selectInput("time", "Select Time of Day:", 
                  choices = c("dawn", "day", "dusk", "night"), 
                  selected = "day")
    ),
    
    mainPanel(
      mapboxglOutput("volcano_map", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  output$volcano_map <- renderMapboxgl({
    selected_volcano <- volcano_data[volcano_data$name == input$volcano, ]
    req(selected_volcano)
    
    mapboxgl(
      zoom = 11,
      center = c(selected_volcano$longitude, selected_volcano$latitude), 
      pitch = 75,
      bearing = 95.2,
      access_token = "pk.eyJ1IjoiYnJvb2tzZyIsImEiOiJjbHh4d2FqYWQxZWl1MmlvZnF1N2JjZncxIn0.z3NeBZnns3sUZlHFo_jfow" # Include token here
    ) |>
      set_config_property(
        "basemap",
        "lightPreset",
        input$time
      )
  })
}

# Run the app
shinyApp(ui, server)
