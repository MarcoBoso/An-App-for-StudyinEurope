```{r}
# Load required packages
library(tidyverse)
library(opencage)

Sys.setenv(OPENCAGE_KEY = "10a6f84592f0477098002a75fd72efb2")

# Function to get coordinates with 1-second delay to respect API limits
get_coords_with_message <- function(place) {
  message("Geocoding: ", place)
  Sys.sleep(1)  # Rate limit
  result <- opencage_forward(place)
  
  if (nrow(result$results) == 0) {
    return(tibble(lat = NA, lon = NA))
  } else {
    return(tibble(
      lat = result$results$geometry.lat[1],
      lon = result$results$geometry.lng[1]
    ))
  }
}

unique_institutions <- denmark_university_tbl %>%
  distinct(Institution)

# Step 2: Apply geocoding only to unique institutions
geo_data <- unique_institutions %>%
  mutate(geocode = map(Institution, get_coords_with_message)) %>%
  unnest(geocode)
