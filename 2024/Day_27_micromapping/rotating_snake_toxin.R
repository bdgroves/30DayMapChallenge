library(raymolecule)

# Directory to save frames
output_dir <- "C:/data/R_Projects/30DayMapChallenge/2024/Day_27_micromapping/frames"
dir.create(output_dir, showWarnings = FALSE)

# Check if the file exists
if(file.exists("C:/data/R_Projects/30DayMapChallenge/2024/Day_27_micromapping/1muq.pdb")) {
  # Loop through angles and save each frame
  for(i in seq(0, 360, by = 10)) {  # Rotate in 10-degree increments
    output_file <- sprintf("%s/frame_%03d.png", output_dir, i)
    read_pdb("C:/data/R_Projects/30DayMapChallenge/2024/Day_27_micromapping/1muq.pdb") %>%
      generate_full_scene() %>%
      render_model(width = 800, height = 800, samples = 300, angle = c(0, i, 0), 
                   filename = output_file)
    cat("Saved frame:", output_file, "\n")
  }
}
library(magick)

# Load all frames
frames <- list.files(output_dir, pattern = "frame_.*\\.png$", full.names = TRUE)
frames_sorted <- frames[order(frames)]  # Ensure frames are in the correct order

# Create the GIF
gif <- image_read(frames_sorted) %>% 
  image_animate(fps = 10, loop = 0)  # 10 frames per second, loop forever

# Save the GIF
gif_output <- "C:/data/R_Projects/30DayMapChallenge/2024/Day_27_micromapping/rotating_molecule_10fps.gif"
image_write(gif, gif_output)

cat("GIF saved to:", gif_output, "\n")
