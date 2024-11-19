library(shiny)
library(leaflet)
library(googlesheets4)
library(dplyr)
library(sf)
library(bslib)
library(googledrive)

# Set up Google Sheets authentication
options(
  gargle_oauth_cache = "google_auth",
  gargle_oauth_email = "bdgroves1970@gmail.com"  # Use your email
)

SHEET_ID <- "1_Cx6idGMjOi29W57ZeInISsbhGFKxfbqNnYRcGheCEA"  # Replace with your actual Google Sheet ID

# Functions for reading/writing locations
read_locations <- function() {
  read_sheet(SHEET_ID)
}

append_location <- function(lng, lat) {
  new_location <- data.frame(
    lng = lng,
    lat = lat,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M")
  )
  sheet_append(SHEET_ID, new_location)
  read_locations()
}

# UI with bslib theming
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#2c3e50",
    "enable-rounded" = TRUE,
    "body-bg" = "#f8f9fa"
  ),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .value-box {
        transition: transform 0.2s;
      }
      .value-box:hover {
        transform: translateY(-5px);
      }
      #submit {
        transition: all 0.3s;
      }
      #submit:hover {
        transform: scale(1.05);
      }
    "))
  ),
  
  # Layout
  layout_sidebar(
    sidebar = sidebar(
      card(
        card_header("Instructions"),
        tags$ol(
          tags$li("Drag the marker to your location"),
          tags$li("Click Submit to add your location"),
          tags$li("Select an animal to add an animal marker")
        ),
        hr(),
        selectInput("animal", "Select an animal:",
                    choices = c("None", "Lion", "Tiger", "Elephant")),
        actionButton("submit", "Submit",
                     class = "btn-lg btn-primary w-100",
                     icon = icon("map-pin")
        ),
        htmlOutput("visitorCount"),
        htmlOutput("lastVisitor")
      )
    ),
    
    # Main panel with map
    card(
      full_screen = TRUE,
      leafletOutput("map", height = "calc(100vh - 80px)")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value for locations
  rv <- reactiveVal(read_locations())
  
  # Custom animal icons
  animal_icons <- list(
    lion = makeIcon(iconUrl = "https://upload.wikimedia.org/wikipedia/commons/7/7c/Lion_icon.png", iconWidth = 30, iconHeight = 30),
    tiger = makeIcon(iconUrl = "https://upload.wikimedia.org/wikipedia/commons/0/0b/Tiger_icon.png", iconWidth = 30, iconHeight = 30),
    elephant = makeIcon(iconUrl = "https://upload.wikimedia.org/wikipedia/commons/9/98/Elephant_icon.png", iconWidth = 30, iconHeight = 30)
  )
  
  # Map rendering
  map_output <- reactive({
    locations <- rv()
    
    # Ensure lat and lng are numeric
    locations$lat <- as.numeric(locations$lat)
    locations$lng <- as.numeric(locations$lng)
    
    # Create a Leaflet map
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 1) %>%
      addMarkers(lng = 0, lat = 0, popup = "Drag me to your location!", 
                 layerId = "visitor_marker") %>%
      addCircleMarkers(
        data = locations,
        lat = ~lat, lng = ~lng,
        color = "navy",
        radius = 5,
        stroke = TRUE,
        fillOpacity = 0.8,
        popup = ~timestamp
      )
  })
  
  # Render the map
  output$map <- renderLeaflet({
    map_output()
  })
  
  # Handle submit button click
  observeEvent(input$submit, {
    marker_pos <- input$map_marker_visitor_marker
    if (!is.null(marker_pos)) {
      withProgress(
        message = "Submitting location...",
        value = 0.5,
        {
          locations <- append_location(marker_pos$lng, marker_pos$lat)
          rv(locations)
          Sys.sleep(0.5)  # Small delay for UX
        }
      )
      
      showNotification(
        "Location submitted successfully!",
        type = "message",
        duration = 3
      )
      
      # Add an animal marker if one is selected
      if (input$animal != "None") {
        animal_marker <- switch(input$animal,
                                "Lion" = animal_icons$lion,
                                "Tiger" = animal_icons$tiger,
                                "Elephant" = animal_icons$elephant)
        
        leafletProxy("map") %>%
          addMarkers(lng = marker_pos$lng, lat = marker_pos$lat,
                     icon = animal_marker,
                     popup = paste(input$animal, "spotted!"))
      }
    }
  })
  
  # Update visitor count
  output$visitorCount <- renderText({
    locations <- rv()
    paste("Total visitors: ", nrow(locations))
  })
  
  # Show last visitor time
  output$lastVisitor <- renderText({
    locations <- rv()
    if (nrow(locations) > 0) {
      last_time <- max(locations$timestamp)
      paste("Last visited: ", last_time)
    } else {
      "No visitors yet"
    }
  })
}

shinyApp(ui, server)
