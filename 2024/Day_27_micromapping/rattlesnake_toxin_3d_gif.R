library(bio3d)
library(rgl)
library(magick)  # For creating GIFs from images

# Load the PDB file
pdb_file <- read.pdb("C:/data/R_Projects/30DayMapChallenge/2024/Day_27_micromapping/1MUQ.pdb")

# Extract atoms for the structure
atoms <- pdb_file$atom

# Create a 3D plot of the atoms
open3d()  # Open a new 3D device

# Assign colors to each chain
chain_colors <- rainbow(length(unique(atoms$chain)))
atom_colors <- chain_colors[as.numeric(as.factor(atoms$chain))]

# Plot protein atoms (colored by chain), without axis labels
plot3d(atoms$x, atoms$y, atoms$z,
       col = atom_colors,
       size = 3, type = "s", add = FALSE,
       axes = FALSE, xlab = "", ylab = "", zlab = "", main = "")  # Remove axis labels

# Remove the bounding box
box3d(lwd = 0)  # Turn off the box around the 3D structure

# Customize the 3D view
bg3d("white")  # Set background color to white
view3d(theta = 45, phi = 30, fov = 60)  # Set the viewpoint angle

# Add title and description labels using text3d
text3d(0, 0, 100, "3D Structure of 1MUQ", color = "black", cex = 2)  # Title
text3d(0, 0, 75, "Rattlesnake Toxin - PDB ID: 1MUQ", color = "black", cex = 1.5)  # Description

# Optional: Display water molecules as smaller spheres
water_atoms <- atoms[atoms$resid == "HOH", ]
if (nrow(water_atoms) > 0) {
  plot3d(water_atoms$x, water_atoms$y, water_atoms$z,
         col = "blue", size = 2, type = "s", add = TRUE)
}

# Save frames as PNG images
num_frames <- 36  # Number of frames for full rotation
image_files <- vector("list", num_frames)

for (i in 1:num_frames) {
  # Rotate the molecule
  view3d(theta = i * (360 / num_frames), phi = 30, fov = 60)
  
  # Save the frame
  filename <- paste0("frame_", sprintf("%03d", i), ".png")
  rgl.snapshot(filename)
  
  # Store the image path
  image_files[[i]] <- image_read(filename)
}

# Create a GIF from the saved frames
animation <- image_animate(image_join(image_files), fps = 10)  # Set frames per second
image_write(animation, "1MUQ_rotation.gif")  # Save the GIF

# Optionally, clean up the image files
file.remove(paste0("frame_", sprintf("%03d", 1:num_frames), ".png"))
