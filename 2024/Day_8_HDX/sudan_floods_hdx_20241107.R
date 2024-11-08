# Load necessary libraries
library(sf)
library(dplyr)

# Load the shapefile
sudan_shapefile <- st_read("C:/data/R_Projects/30DayMapChallenge/2024/Day_8_HDX/Locality_Polygon_Mar20.shp")

# Load flood data by skipping the first row of headers
flood_data <- read.csv("C:/data/R_Projects/30DayMapChallenge/2024/Day_8_HDX/Sudan Floods 2024 data - Floods data2024.csv", skip = 1)

# Rename columns to be more descriptive
colnames(flood_data) <- c("State", "Locality", "HousesDestroyed", "HousesDamaged", 
                          "HouseholdsAffected", "PeopleKilled", "PeopleInjured", 
                          "PeopleAffected")

# Remove rows where State or Locality has non-location information
flood_data <- flood_data %>%
  filter(State != "State" & Locality != "Locality")

# Standardize text in columns for matching
sudan_shapefile <- sudan_shapefile %>%
  mutate(Loc_En = tolower(trimws(Loc_En)))

flood_data <- flood_data %>%
  mutate(Locality = tolower(trimws(Locality)))

# Join the shapefile and flood data
sudan_flood_map <- sudan_shapefile %>%
  left_join(flood_data, by = c("Loc_En" = "Locality"))

# View the result to ensure the join worked
head(sudan_flood_map)

# Check for any rows where flood data columns are NA
na_rows <- sudan_flood_map %>% filter(is.na(HousesDestroyed))
print(na_rows)

# Clean up extra spaces and convert PeopleAffected to numeric
sudan_flood_map <- sudan_flood_map %>%
  mutate(PeopleAffected = as.numeric(gsub(",", "", trimws(PeopleAffected))))

# Load ggplot2 for visualization
library(ggplot2)

# Create the choropleth map with a darker blue gradient
ggplot(data = sudan_flood_map) +
  geom_sf(aes(fill = PeopleAffected), color = "white", size = 0.2) +
  scale_fill_gradient(low = "#5B92E5", high = "#001F3F", na.value = "grey90", 
                      name = "People Affected") +
  labs(
    title = "Flood Impact Across Sudan in 2024 by Locality",
    subtitle = "Mapping the severity of affected populations due to extreme flooding events",
    caption = "#30DayMapChallenge - Day 8 HDX Data"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic")
  )

# Save the map as a PNG file
ggsave("sudan_flood_choropleth_darker_blue.png", width = 10, height = 8, dpi = 300)
