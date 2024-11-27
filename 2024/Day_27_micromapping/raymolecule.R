library(raymolecule)

#This assumes a hypothetical PDB file in your working directory:
if(file.exists("C:/data/R_Projects/30DayMapChallenge/2024/Day_27_micromapping/1muq.pdb")) {
  read_pdb("C:/data/R_Projects/30DayMapChallenge/2024/Day_27_micromapping/1muq.pdb") %>%
    generate_full_scene() %>%
    render_model()
}


#Specify a width, height, and number of samples for the image (more samples == less noise)
get_example_molecule("caffeine") |>
  read_sdf() |> 
  generate_full_scene() |> 
  render_model(width=800,height=800,samples=1000, clamp_value=10)

library(raymolecule)

# Check if the file exists
if(file.exists("C:/data/R_Projects/30DayMapChallenge/2024/Day_27_micromapping/1muq.pdb")) {
  # Loop through angles and render each frame
  for(i in seq(0, 360, by = 10)) {
    read_pdb("C:/data/R_Projects/30DayMapChallenge/2024/Day_27_micromapping/1muq.pdb") %>%
      generate_full_scene() %>%
      render_model(width = 800, height = 800, samples = 1000, angle = c(0, i, 0))  # Rotate by 'i' degrees around the y-axis
  }
}
