library(rnaturalearth)
library(sf)
library(tidyverse)
library(cartogram)
library(ggforce)

world <- ne_countries(scale = 110, type = "countries", returnclass = "sf")%>%
  # Convert WGS84 to projected crs (here Robinson)
  sf::st_transform(world_ne, crs="ESRI:54030")

data <- read_csv('https://raw.githubusercontent.com/BjnNowak/dorling_map/main/data/FAOSTAT_land_use_2020.csv')

clean <- data%>%
  select(iso_a3='Area Code (ISO3)',item=Item,surface=Value)%>%
  # Simplify variable names
  mutate(item=case_when(
    item=="Agricultural land"~"total",
    item=="Arable land"~"crop",
    item=="Permanent meadows and pastures"~"grass"
  ))%>%
  # Pivot from long to wide
  pivot_wider(names_from=item,values_from=surface)

# Join data to world map
map <- world%>%
  left_join(clean)%>%
  drop_na(total)

ggplot(map, aes(fill=total))+
  geom_sf()

# Making Dorling cartogram based on total agricultural land
dorl<-cartogram::cartogram_dorling(
  map, weight='total', k = 5,
  m_weight = 1, itermax = 1000
)

# Set colors
col_world <- "#9CB4BF"
col_back <- "#1D201F"

# Set theme
theme_custom <- theme_void()+
  theme(plot.background = element_rect(fill=col_back,color=NA))

ggplot()+
  # World basemap
  geom_sf(
    world,mapping=aes(geometry=geometry),
    fill=col_world,color=alpha("dimgrey",0.25)
  )+
  # Dorling cartogram
  geom_sf(
    dorl,mapping=aes(geometry=geometry),
    fill=alpha("dimgrey",0.75),color=alpha("white",0.2)
  )+
  theme_custom

# Compute area and radius for each circus of the cartogram
dorl<-dorl%>%
  mutate(
    # Compute area
    ar=as.numeric(st_area(dorl)),
    # Compute radius based on area
    rad=as.numeric(sqrt(ar/pi))
  )

# Extract centroids for each circle
centr <- dorl%>%
  st_centroid()%>%
  st_coordinates()

# Combine data
dorl2 <- tibble(dorl,X=centr[,1],Y=centr[,2])%>%
  arrange(-total)

ggplot()+
  # World basemap
  geom_sf(
    world,mapping=aes(geometry=geometry),
    fill=col_world,color=alpha("dimgrey",0.25)
  )+
  # Draw Dorling cartogram with geom_circle()
  ggforce::geom_circle(
    data = dorl2, aes(x0 = X, y0 = Y, r = rad),
    fill=alpha("dimgrey",0.75),color=alpha("white",0.2)
  )+
  theme_custom


dorl2 <- dorl2 %>%
  mutate(
    ratio_crop = crop/total,
    ratio_grass = grass/total
  )%>%
  mutate(
    rad_crop=sqrt(rad*rad*ratio_crop),
    rad_grass=sqrt(rad*rad*ratio_grass)
  )

col_crop <- "#f2e901"
col_grass <- "#51c26f"

ggplot()+
  # World basemap
  geom_sf(
    world,mapping=aes(geometry=geometry),
    fill=col_world,color=alpha("dimgrey",0.25)
  )+
  # Draw Dorling cartogram with geom_circle()
  ggforce::geom_circle(
    data = dorl2, aes(x0 = X, y0 = Y, r = rad),
    fill=alpha("dimgrey",0.75),color=alpha("white",0.2)
  )+
  # Draw circle for crops (or grass)
  ggforce::geom_circle(
    data = dorl2, aes(x0 = X, y0 = Y, r = rad_crop),
    fill=col_crop,color=NA
  )+
  theme_custom