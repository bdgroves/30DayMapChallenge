library(tidyverse)
library(terra)     # For raster population file
library(tidyterra) # To plot terra rasters with ggplot()
library(sf)        # For French department vector 
library(ggtext)    # To customize plot text

# France map
fr <- sf::read_sf('https://github.com/BjnNowak/bertin_tuto/raw/main/map/france_sport.gpkg')

# Population density in km2
pop <- terra::rast('https://github.com/BjnNowak/bertin_tuto/raw/main/map/france_pop_l93_2.tif')

# Rename raster band to "pop"
names(pop) <- "pop"

ggplot()+
  geom_sf(fr,mapping=aes(geometry=geom))

ggplot()+
  tidyterra::geom_spatraster(data=pop)