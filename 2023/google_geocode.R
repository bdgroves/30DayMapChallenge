# Load necessary libraries
library("googleway")

# Use the API key from the environment variable
google_api_key <- Sys.getenv("GOOGLE_API_KEY")

flcities <- data.frame(state = rep("Florida", 5), city = c("Miami", 
                                                           "Tampa", "Orlando", "Jacksonville", "Sarasota"))
coords <- apply(flcities, 1, function(x) {
  google_geocode(address = paste(x["city"], x["state"], sep = ", "), 
                 key = google_api_key)
})
flcities <- cbind(flcities, do.call(rbind, lapply(coords, geocode_coordinates)))
