# Load required libraries
library(googlesheets4)
library(dplyr)
library(leaflet)

# Authenticate with full access to both Sheets and Drive
gs4_auth(scope = c("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"))

# Sheet ID for accessing the Google Sheet
sheet_id <- "1_Cx6idGMjOi29W57ZeInISsbhGFKxfbqNnYRcGheCEA"

# Get the sheet
sheet <- gs4_get(sheet_id)

# Read data from the sheet
data <- read_sheet(sheet)

# View the data (if needed)
print(data)

# Create an interactive map using leaflet
leaflet(data = data) %>%
  addTiles() %>%  # Add base tiles to the map
  addCircleMarkers(
    lat = ~lat,  # Latitude column
    lng = ~lng,  # Longitude column
    color = "blue",  # Marker color
    radius = 5,  # Size of the markers
    popup = ~paste("Species:", species, "<br>Timestamp:", timestamp)  # Info on click
  )

