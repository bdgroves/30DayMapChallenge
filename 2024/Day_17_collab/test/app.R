# Load required libraries
library(shiny)
library(googlesheets4)

# Authenticate with Google Sheets
gs4_auth(
  path = "cellular-nuance-442423-v5-33e3c3f99f58.json",
  scopes = c("https://www.googleapis.com/auth/spreadsheets", 
             "https://www.googleapis.com/auth/drive")
)


# Replace this with your Google Sheet ID
SHEET_ID <- "1WOFaz4KUUnAew2PYheBniW708NU85DvGgpWZicjGR8U"

# Define UI
ui <- fluidPage(
  titlePanel("Google Sheets Basic Example"),
  sidebarLayout(
    sidebarPanel(
      textInput("new_data", "Enter some text:", value = ""),
      actionButton("add_button", "Add to Sheet")
    ),
    mainPanel(
      tableOutput("sheet_data")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Function to read data from the sheet
  read_sheet_data <- function() {
    read_sheet(SHEET_ID)
  }
  
  # Reactive value to store data
  sheet_data <- reactiveVal(read_sheet_data())
  
  # Render the table
  output$sheet_data <- renderTable({
    sheet_data()
  })
  
  # Observe the Add button click
  observeEvent(input$add_button, {
    req(input$new_data) # Ensure input is not empty
    new_row <- data.frame(
      Data = input$new_data,
      Timestamp = Sys.time()
    )
    sheet_append(SHEET_ID, new_row) # Add to Google Sheet
    sheet_data(read_sheet_data())  # Refresh the data
  })
}

# Run the app
shinyApp(ui, server)
