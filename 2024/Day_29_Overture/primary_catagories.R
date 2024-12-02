library(arrow)
library(dplyr)

# Define the S3 path (replace with the actual dataset location)
s3_path <- "s3://overturemaps-us-west-2/release/2024-11-13.0/theme=places/type=place/"

# Open the dataset
places_dataset <- open_dataset(s3_path, format = "parquet")

# Get a distinct list of primary categories
primary_categories <- places_dataset |>
  select(categories) |> 
  distinct(categories$primary) |> 
  collect()

# Print the unique primary categories
print(primary_categories)
