# Load required libraries
library(mapgl)
library(googlesheets4)
library(dplyr)
library(sf)
library(bslib)
library(googledrive)

gs4_auth(
  path = "new-animal-442400-8ec0c0f1767e.json",
  scopes = c("https://www.googleapis.com/auth/spreadsheets", 
             "https://www.googleapis.com/auth/drive")
)


# Sheet ID for accessing the Google Sheet (replace with your own)
SHEET_ID <- "1_Cx6idGMjOi29W57ZeInISsbhGFKxfbqNnYRcGheCEA"

# Function to read locations from Google Sheets
read_locations <- function() {
  read_sheet(SHEET_ID)
}

# Function to append a new location and species to Google Sheets
append_location <- function(lng, lat, species) {
  new_location <- data.frame(
    lng = lng,
    lat = lat,
    species = species,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  sheet_append(SHEET_ID, new_location)
  read_locations()
}

# UI with Bootstrap theming
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#2c3e50",
    "enable-rounded" = TRUE,
    "body-bg" = "#f8f9fa"
  ),
  
  # Custom CSS for styling
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
  
  # Layout with a sidebar and main content area
  layout_sidebar(
    sidebar = sidebar(
      card(
        card_header("Instructions"),
        tags$ol(
          tags$li("Drag the marker to your location."),
          tags$li("Enter the species (e.g., dog, cat)."),
          tags$li("Click 'Submit' to add your location.")
        ),
        hr(),
        textInput("species", "Species (e.g., dog, cat)", value = ""),
        actionButton("submit", "Submit",
                     class = "btn-lg btn-primary w-100",
                     icon = icon("map-pin")
        ),
        htmlOutput("visitorCount"),
        htmlOutput("lastVisitor")
      )
    ),
    
    # Main panel with a full-screen map
    card(
      full_screen = TRUE,
      mapboxglOutput("map", height = "calc(100vh - 80px)")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to store the locations dataset
  rv <- reactiveVal(read_locations())
  
  # Generate the map
  map_output <- reactive({
    locations <- rv()
    
    # Convert locations to sf object
    locations_sf <- st_as_sf(
      locations,
      coords = c("lng", "lat"),
      crs = 4326
    )
    
    mapboxgl(
      center = c(0, 0),
      zoom = 1,
      access_token = "pk.eyJ1IjoiYnJvb2tzZyIsImEiOiJjbHh4d2FqYWQxZWl1MmlvZnF1N2JjZncxIn0.z3NeBZnns3sUZlHFo_jfow" # Replace with your Mapbox access token
    ) %>%
      add_navigation_control() %>%
      add_geolocate_control() %>%
      add_markers(
        data = c(0, 0),  # Initial draggable marker
        draggable = TRUE,
        popup = "Drag me to your location!",
        marker_id = "visitor_marker"
      ) %>%
      add_circle_layer(
        id = "visitors",
        source = locations_sf,
        circle_color = "navy",
        circle_stroke_color = "white",
        circle_radius = 5,
        circle_stroke_width = 1,
        popup = ~paste("Species: ", species, "<br>Time: ", timestamp),
        tooltip = ~paste("Species: ", species, " at ", timestamp)
      )
  })
  
  # Render the map in the UI
  output$map <- renderMapboxgl({
    map_output()
  })
  
  # Handle the submit button click
  observeEvent(input$submit, {
    marker_pos <- input$map_marker_visitor_marker
    species_name <- input$species
    if (!is.null(marker_pos) && species_name != "") {
      withProgress(
        message = "Submitting location...",
        value = 0.5,
        {
          # Append the new location and species, and refresh the dataset
          locations <- append_location(marker_pos$lng, marker_pos$lat, species_name)
          rv(locations)
          Sys.sleep(0.5) # Slight delay for better user experience
        }
      )
      showNotification(
        "Location and species submitted successfully!",
        type = "message",
        duration = 3
      )
    } else {
      showNotification(
        "Please drag the marker and enter a species before submitting.",
        type = "error",
        duration = 3
      )
    }
  })
  
  # Update the total visitor count
  output$visitorCount <- renderText({
    locations <- rv()
    paste("Total visitors:", nrow(locations))
  })
  
  # Display the timestamp of the last visitor
  output$lastVisitor <- renderText({
    locations <- rv()
    if (nrow(locations) > 0) {
      last_time <- max(locations$timestamp)
      paste("Last visited:", last_time)
    } else {
      "No visitors yet."
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
