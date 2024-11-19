library(googlesheets4)
library(gargle)

# Set up the OAuth client for authentication
gs4_auth_configure(
  path = "C:/data/R_Projects/client_secret_679482495927-i967ltvlkvh7ej99t0ne824aql67obpa.apps.googleusercontent.com.json"
)

# Authenticate with the required scopes for Google Sheets
gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets.readonly", 
                    "https://www.googleapis.com/auth/spreadsheets"))

# Define the Google Sheet ID (you can also copy this from your sheet URL)
sheet_id <- "1_Cx6idGMjOi29W57ZeInISsbhGFKxfbqNnYRcGheCEA"

# Read the sheet into R
sheet_data <- read_sheet(sheet_id)

# Example of adding data to the sheet
write_sheet(sheet_data, sheet_id)  # This will write data back to the sheet

# New data to append
new_data <- data.frame(
  species = "Elephant",
  sighting_date = Sys.Date(),
  location = "Africa"
)

# Append the new data to the sheet
range_write(sheet_id, data = new_data, range = "A1", col_names = TRUE)

# Updating a specific cell (e.g., updating the value at row 2, column 3)
range_write(sheet_id, data = "New Value", range = "C2")


