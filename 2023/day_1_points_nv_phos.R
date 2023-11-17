library(ggplot2)
library(ggsn)
library(sf)
library(dplyr)
library(dataRetrieval)
library(leaflet)

NV_sites <- whatNWISsites(stateCd = "NV", 
                          parameterCd = "00665")
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE),
                crs = 4269)

sf_nv <- st_as_sf(NV_sites, 
                  coords = c("dec_long_va", "dec_lat_va"),
                  crs = 4269)
ggplot() +
  geom_sf(data = usa[ usa$ID == "nevada" ,]) +
  geom_sf(data = sf_nv) + 
  xlab(NULL)+
  ylab(NULL)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  north(sf_nv, symbol=10, location="bottomleft") +
  scalebar(usa[ usa$ID == "nevada" ,],
           dist=100, dist_unit="mi", st.size = 3,
           transform=TRUE, model="WGS84")

pCode <- c("00665")
phWI <- whatNWISdata(stateCd="NV", 
                     parameterCd=pCode)

phWI.1 <- phWI %>% 
  filter(count_nu > 300) %>%
  mutate(period = as.Date(end_date) - as.Date(begin_date)) %>%
  filter(period > 15*365)

phos_NV_data <- readNWISqw(siteNumbers = phWI.1$site_no,
                           parameterCd = pCode)

phos_summary <- phos_NV_data %>% 
  group_by(site_no) %>% 
  summarize(max = max(result_va, na.rm = TRUE),
            count = n()) %>% 
  ungroup() %>% 
  left_join(attr(phos_NV_data, "siteInfo"), 
            by = "site_no")

# Assuming phos_summary has latitude and longitude columns (replace with your actual column names)
phos_summary_map <- phos_summary %>%
  filter(!is.na(dec_lat_va) & !is.na(dec_long_va)) %>%
  leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    lat = ~dec_lat_va,  # Specify the latitude column
    lng = ~dec_long_va,  # Specify the longitude column
    radius = ~sqrt(count),  # Adjust the circle size based on count
    color = "blue",         # Circle color
    fillOpacity = 0.7,       # Circle fill opacity
    label = ~paste("Site: ", site_no, "<br>Max Phos: ", max),
    popup = ~paste("Site: ", site_no, "<br>Max Phos: ", max)
  ) %>%
  addLegend(
    position = "bottomright",
    colors = "blue",
    labels = "Count",
    opacity = 0.7
  ) %>%
  addScaleBar()

# Assuming phos_summary has latitude and longitude columns (replace with your actual column names)
phos_summary_map <- phos_summary %>%
  filter(!is.na(dec_lat_va) & !is.na(dec_long_va)) %>%
  leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    lat = ~dec_lat_va,  # Specify the latitude column
    lng = ~dec_long_va,  # Specify the longitude column
    radius = ~sqrt(count),  # Adjust the circle size based on count
    color = ~ifelse(max > 5, "red", "green"),  # Conditional color based on max value
    fillOpacity = 0.7,       # Circle fill opacity
    label = ~paste("Site: ", site_no, "<br>Max Phos: ", max),
    popup = ~paste("Site: ", site_no, "<br>Max Phos: ", max)
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("green", "red"),  # Add colors for the legend
    labels = c("Max Phos <= 5", "Max Phos > 5"),  # Add labels for the legend
    opacity = 0.7
  ) %>%
  addScaleBar()


