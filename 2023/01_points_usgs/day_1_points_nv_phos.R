library(ggplot2)
library(ggsn)
library(sf)
library(dplyr)
library(dataRetrieval)

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
