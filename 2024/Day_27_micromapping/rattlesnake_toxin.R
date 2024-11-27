library(bio3d)
library(rgl)

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

# Save the final 3D plot as a PNG image without labels and bounding box
rgl.snapshot("1MUQ_3D_visualization_no_labels.png")  # Save as PNG image
