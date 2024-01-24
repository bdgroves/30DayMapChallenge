# SpatialAnalysisScript.R

# Load libraries
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library('cowplot')

# Load world map data
world <- ne_countries(scale='medium', returnclass='sf')

# General concepts
# Create a base plot
(g1 <- ggplot() + geom_blank() + theme_void())

# Create a void theme
(g1_void <- g1 + theme(panel.border = element_rect(colour = "black", fill = NA)))

# Combine base plot with void theme
g1 +
  annotation_custom(
    grob = ggplotGrob(g1_void),
    xmin = 0,
    xmax = 3,
    ymin = 5,
    ymax = 10
  ) +
  annotation_custom(
    grob = ggplotGrob(g1_void),
    xmin = 5,
    xmax = 10,
    ymin = 0,
    ymax = 3
  )

# Use cowplot for layout
ggdraw(g1) + draw_plot(g1_void, width = 0.25, height = 0.5, x = 0.02, y = 0.48) +
  draw_plot(g1_void, width = 0.5, height = 0.25, x = 0.75, y = 0.09)

# Several maps side by side or on a grid
plot1 <- (gworld <- ggplot(data = world) +
            geom_sf(aes(fill = region_wb)) +
            geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
                      fill = NA, colour = "black", size = 1.5) +
            scale_fill_viridis_d(option = "plasma") +
            theme(panel.background = element_rect(fill = "azure"),
                  panel.border = element_rect(fill = NA)))

plot2 <- (ggulf <- ggplot(data = world) +
            geom_sf(aes(fill = region_wb)) +
            annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", 
                     fontface = "italic", color = "grey22", size = 6) +
            coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) +
            scale_fill_viridis_d(option = "plasma") +
            theme(legend.position = "none", axis.title.x = element_blank(), 
                  axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
                  panel.border = element_rect(fill = NA)))

# Use cowplot for layout
ggplot() +
  coord_equal(xlim = c(0, 3.3), ylim = c(0, 1), expand = FALSE) +
  annotation_custom(ggplotGrob(plot1), xmin = 0, xmax = 1.5, ymin = 0, ymax = 1) +
  annotation_custom(ggplotGrob(plot2), xmin = 1.5, xmax = 3, ymin = 0, ymax = 1) +
  theme_void()

# Use cowplot for layout
plot_grid(gworld, ggulf, nrow = 1, rel_widths = c(2.3, 1))

# Map insets
usa <- subset(world, admin == "United States of America")

# Create mainland, alaska, and hawaii plots
(mainland <- ggplot(data = usa) +
    geom_sf(fill = "cornsilk") +
    coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)))

(alaska <- ggplot(data = usa) +
    geom_sf(fill = "cornsilk") +
    coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), 
             expand = FALSE, datum = NA))

(hawaii <- ggplot(data = usa) +
    geom_sf(fill = "cornsilk") +
    coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), 
             expand = FALSE, datum = NA))

# Combine maps with insets using cowplot
mainland +
  annotation_custom(
    grob = ggplotGrob(alaska),
    xmin = -2750000,
    xmax = -2750000 + (1600000 - (-2400000))/2.5,
    ymin = -2450000,
    ymax = -2450000 + (2500000 - 200000)/2.5
  ) +
  annotation_custom(
    grob = ggplotGrob(hawaii),
    xmin = -1250000,
    xmax = -1250000 + (-154 - (-161))*120000,
    ymin = -2450000,
    ymax = -2450000 + (23 - 18)*120000
  )

# Several maps connected with arrows
sites <- st_as_sf(data.frame(longitude = c(-80.15, -80.1), latitude = c(26.5, 26.8)), 
                  coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Create maps for Florida, Site A, and Site B
(florida <- ggplot(data = world) +
    geom_sf(fill = "antiquewhite1") +
    geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
    annotate(geom = "text", x = -85.5, y = 27.5, label = "Gulf of Mexico", 
             color = "grey22", size = 4.5) +
    coord_sf(xlim = c(-87.35, -79.5), ylim = c(24.1, 30.8)) +
    xlab("Longitude")+ ylab("Latitude")+
    theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                          linewidth = 0.5),
          panel.background = element_rect(fill = "aliceblue"), 
          panel.border = element_rect(fill = NA)))

(siteA <- ggplot(data = world) +
    geom_sf(fill = "antiquewhite1") +
    geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
    coord_sf(xlim = c(-80.25, -79.95), ylim = c(26.65, 26.95), expand = FALSE) + 
    annotate("text", x = -80.18, y = 26.92, label= "Site A", size = 6) + 
    theme_void() + 
    theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                          size = 0.5),
          panel.background = element_rect(fill = "aliceblue"), 
          panel.border = element_rect(fill = NA)))

(siteB <- ggplot(data = world) + 
    geom_sf(fill = "antiquewhite1") +
    geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
    coord_sf(xlim = c(-80.3, -80), ylim = c(26.35, 26.65), expand = FALSE) +
    annotate("text", x = -80.23, y = 26.62, label= "Site B", size = 6) + 
    theme_void() +
    theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                          size = 0.5),
          panel.background = element_rect(fill = "aliceblue"), 
          panel.border = element_rect(fill = NA)))

# Create arrow data
arrowA <- data.frame(x1 = 18.5, x2 = 23, y1 = 9.5, y2 = 14.5)
arrowB <- data.frame(x1 = 18.5, x2 = 23, y1 = 8.5, y2 = 6.5)

# Plot maps with arrows
ggplot() +
  coord_equal(xlim = c(0, 28), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(florida), xmin = 0, xmax = 20, ymin = 0, ymax = 20) +
  annotation_custom(ggplotGrob(siteA), xmin = 20, xmax = 28, ymin = 11.25, ymax = 19) +
  annotation_custom(ggplotGrob(siteB), xmin = 20, xmax = 28, ymin = 2.5, ymax = 10.25) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, 
               arrow = arrow(), lineend = "round") +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowB, 
               arrow = arrow(), lineend = "round") +
  theme_void()

# Save the final plot to a PDF
ggsave("florida-sites.pdf", width = 10, height = 7)
