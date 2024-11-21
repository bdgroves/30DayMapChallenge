library(rayshader)
library(magick)
library(grid)

# Step 1: Create an Image of the "@" Symbol
symbol <- image_blank(width = 400, height = 400, color = "white") %>%
  image_annotate("@", size = 300, color = "black", gravity = "center")
image_write(symbol, "symbol.png")

# Step 2: Read and Resize Image
img <- image_read("symbol.png") %>% image_convert(type = "grayscale")
img_resized <- image_resize(img, "200x200")

# Step 3: Convert to Numeric Matrix
img_array <- image_data(img_resized)
img_numeric <- as.numeric(img_array[1,,])  # Convert the first channel to numeric
img_matrix <- matrix(img_numeric, nrow = dim(img_array)[2], ncol = dim(img_array)[3]) / 255  # Normalize

# Step 4: Invert for Elevation
elevation_matrix <- 1 - img_matrix

# Step 5: Create Folder to Save Frames
frames <- 30  # Number of frames for the movie
output_folder <- "frames"  # Folder to save the frames

# Create a folder to store frames
if(!dir.exists(output_folder)) dir.create(output_folder)

# Generate frames for rotating the 3D plot
for (i in 1:frames) {
  # Rotate the 3D plot around theta (angle) for each frame
  elevation_matrix %>%
    sphere_shade(texture = "bw") %>%  # Use an interesting texture like "bw"
    plot_3d(elevation_matrix, zscale = 0.05, fov = 0, theta = i * 12, phi = 30,
            windowsize = c(800, 800), zoom = 0.75)
  
  # Save the frame as an image
  rgl::rgl.snapshot(file.path(output_folder, sprintf("frame_%03d.png", i)))
}

# Step 6: Load frames and create a GIF
frames_list <- lapply(1:frames, function(i) {
  image_read(file.path(output_folder, sprintf("frame_%03d.png", i)))
})

# Create a GIF with a delay of 40ms (25fps), which is a factor of 100
image_animate(image_join(frames_list), delay = 40) %>%
  image_write("rotating_symbol.gif")

# Optional: Remove the frames folder after creating the GIF
unlink(output_folder, recursive = TRUE)
