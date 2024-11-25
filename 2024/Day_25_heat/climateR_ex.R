# Load Required Libraries
library(AOI)          # For fetching Colorado AOI
library(climateR)     # For requesting climate data
library(dplyr)        # For data manipulation
library(ggplot2)      # For data visualization
library(sf)           # For spatial data processing
library(tidyr)        # For tidying data
library(tidyterra)    # For terra and ggplot integration
library(tigris)       # For geographic boundaries
library(terra)        # For raster data handling

# Step 1: Get Colorado Area of Interest (AOI)
colorado <- aoi_get(state = "CO", county = "all")

# Step 2: Load Cities Data for Colorado
# Reads pre-saved Colorado cities dataset from the `climateR` package
cities <- readRDS(system.file("co/cities_colorado.rds", package = "climateR"))

# Step 3: Request Precipitation Data for Colorado (1991-10-29 to 1991-11-06)
# Using the GridMET dataset via `getGridMET` function
system.time({
  gridmet_pr <- getGridMET(
    AOI = colorado,
    varname = "pr",          # Precipitation
    startDate = "1991-10-29",
    endDate = "1991-11-06"
  )
})
