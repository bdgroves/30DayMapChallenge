library(climateR)
library(terra)
library(sf)
library(dplyr)
library(mapgl)
library(tigris)
library(AOI)
library(terra)
library(climateR)
library(tidyterra)
library(ggplot2)
library(tidyr)
library(sf)

colorado = aoi_get(state = "CO", county = "all")

cities = readRDS(system.file("co/cities_colorado.rds", package = "climateR"))

# Request Data for Colorado (POLYGON(s))
system.time({
  gridmet_pr = getGridMET(AOI = colorado,
                          varname = "pr",
                          startDate = "1991-10-29",
                          endDate  = "1991-11-06")
})

