# Load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(spocc)
library(gridExtra)
library(lubridate)

# Download occurrence data for Dipodomys deserti and Heloderma suspectum
data <- occ(query = c('Dipodomys deserti', 'Heloderma suspectum'), from = c('gbif', 'inat'))
head(data$gbif$data$Dipodomys_deserti, 3)
occ_df <- occ2df(obj = data)

# Enable S2 geometry operations
sf_use_s2()

# Read shapefile and convert to sf object for Southwest states
southwest <- st_read("C:/data/R_Projects/data_experiments/data_raw/southwest_states.shp", quiet = TRUE)
sf_southwest <- st_as_sf(southwest)

# Get occurrence data for Dipodomys californicus
ca_krat <- spocc::occ(query = c('Dipodomys californicus'), 
                      from = c('gbif', 'inat'), 
                      limit = 2000,
                      date = c('2003-01-01', '2023-11-17'))
ca_krat <- occ2df(ca_krat)

# Get occurrence data for Dipodomys deserti
krat <- spocc::occ(query = c('Dipodomys deserti'), 
                   from = c('gbif', 'inat'), 
                   limit = 2000,
                   date = c('2003-01-01', '2023-11-17'))
krat <- occ2df(krat)

# Convert 'longitude' and 'latitude' columns into numbers
krat <- mutate_at(krat, c('longitude', 'latitude'), as.numeric)

# Convert 'longitude' and 'latitude' columns into numbers for ca_krat
ca_krat <- mutate_at(ca_krat, c('longitude', 'latitude'), as.numeric)

# Remove rows with NA for 'longitude' or 'latitude'
krat <- filter_at(krat, vars(longitude, latitude), all_vars(!is.na(.)))
ca_krat <- filter_at(ca_krat, vars(longitude, latitude), all_vars(!is.na(.)))

# Convert longitude/latitude to POINT
krat <- st_as_sf(x = krat, coords = c('longitude', 'latitude'), crs = st_crs(southwest))
ca_krat <- st_as_sf(x = ca_krat, coords = c('longitude', 'latitude'), crs = st_crs(southwest))

# Perform spatial join with Southwest states
krat <- st_join(x = krat, y = southwest, left = FALSE)
ca_krat <- st_join(x = ca_krat, y = southwest, left = FALSE)

# Convert date column to Date type and extract Julian calendar components
krat$date <- as.Date(krat$date)
krat$year <- year(krat$date)
krat$month <- month(krat$date)
krat$day <- day(krat$date)

ca_krat$date <- as.Date(ca_krat$date)
ca_krat$year <- year(ca_krat$date)
ca_krat$month <- month(ca_krat$date)
ca_krat$day <- day(ca_krat$date)

# Plot occurrences of Dipodomys deserti and Dipodomys californicus in the Southwest
k1 <- ggplot() +
  geom_sf(data = southwest) +
  geom_sf(data = krat, aes(color = prov), size = 3) + 
  scale_color_manual(name = 'Provider',
                     values = c(alpha(colour =  'red', alpha = 0.35),
                                alpha(colour = 'blue', alpha = 0.35)),
                     labels = c('GBIF', 'INAT')) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Dipodomys deserti occurrence in the Southwest (2003-2023)') + 
  theme_bw()

k2 <- ggplot() +
  geom_sf(data = southwest) +
  geom_sf(data = ca_krat, aes(color = prov), size = 3) + 
  scale_color_manual(name = 'Provider',
                     values = c(alpha(colour =  'red', alpha = 0.35),
                                alpha(colour = 'blue', alpha = 0.35)),
                     labels = c('GBIF', 'INAT')) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Dipodomys californicus occurrence in the Southwest (2003-2023)') + 
  theme_bw()

# Arrange the plots side by side
side_by_side_plots <- grid.arrange(k1, k2, ncol = 2)
