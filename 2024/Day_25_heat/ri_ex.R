library(climateR)
library(AOI)

# Define a small AOI (e.g., Fort Collins, CO)
test_aoi <- aoi_get(state = "RI")

# Test GridMET data with a narrow date range and single variable
test_data <- getGridMET(
  AOI = test_aoi,
  varname = "pr",            # Precipitation
  startDate = "2021-01-01",
  endDate = "2021-01-03"     # Only 3 days of data
)

# Check the structure of the returned data
print(test_data)
