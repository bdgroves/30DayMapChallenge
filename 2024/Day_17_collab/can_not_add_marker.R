library(shiny)
library(mapgl)
library(googlesheets4)
library(dplyr)

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

append_location <- function(lng, lat, species) {
  new_location <- data.frame(
    lng = lng,
    lat = lat,
    species = species,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M")
  )
  sheet_append(SHEET_ID, new_location)
  read_locations()
}

ui <- fluidPage(
  titlePanel("Animal Sightings Map"),
  
  # Input for selecting species
  textInput("species", "Species Observed:", "Enter species name"),
  
  # Main panel with map
  mapboxglOutput("map", height = "500px"),  # Make sure the height is set
  
  # Submit button
  actionButton("submit", "Submit Location", class = "btn-lg btn-primary w-100")
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
  
  # Store the marker's position
  marker_pos <- reactiveVal(c(lng = 0, lat = 0))
  
  # Update marker position when it is dragged
  observeEvent(input$map_marker, {
    marker_pos(c(input$map_marker$lng, input$map_marker$lat))
  })
  
  # Handle submit button click
  observeEvent(input$submit, {
    marker_pos_val <- marker_pos()
    species_val <- input$species
    
    # Ensure species is provided
    if (nchar(species_val) == 0) {
      showNotification("Please enter a species name.", type = "error")
      return()
    }
    
    # Save location and species to Google Sheet
    withProgress(
      message = "Submitting location...",
      value = 0.5,
      {
        locations <- append_location(marker_pos_val[1], marker_pos_val[2], species_val)
      }
    )
    
    showNotification(
      paste("Location for", species_val, "submitted successfully!"),
      type = "message",
      duration = 3
    )
  })
}

shinyApp(ui, server)
