
# Load necessary libraries
library(tidygeocoder)
library(dplyr)
library(tibble)
library(sf)


early_data <- read.csv("Data/24_11_25 UK solitary bee museum specimen measurements - raw data.csv")

# Remove empty rows based on label_no
early_data <- early_data[!is.na(early_data$label_no) & early_data$label_no != "", ]

# Create a location string to include the best available resolution for geocoding
early_data$location_string <- apply(early_data[, c("specific_location",
                                   "city_town_village",
                                   "county",
                                   "country")], 
                            1, function(x) {
                              paste(na.omit(trimws(x[x != ""])), collapse = ", ")
                            })

# Extract unique locations to reduce geocoding time
unique_locs <- unique(early_data$location_string)
# Create a dataframe of unique locations
unique_locs_df <- tibble(location_string = unique_locs)


# Geocode unique locations of first 5 locations as a test to see how well the geocoding works
# Make a smaller dataframe
test_locations <- early_data[1:5, ]

geo_test <- geocode(test_locations,
                    address = "location_string",
                    method = 'osm',
                    lat = latitude,
                    long = longitude
                    )


# Now geocode all unique locations
# OS limits request rates; you may need to slow down (mode = "slow")
early_data_geo <- geocode(unique_locs_df,
                          address = "location_string",
                          method = 'osm',
                          lat = latitude,
                          long = longitude
                          )


# rows where latitude is NA
na_early_data_geo <- early_data_geo %>% filter(is.na(latitude) & !is.na(location_string))


# save early_data_geo as a .csv
write.csv(early_data_geo, "Data/early_data_geocoded.csv", row.names = FALSE)

# I then manually entered the lat long coordinates for the NA columns using google maps
# Add the lat long (locations_lat_long.csv) back to the original early_data dataframe

locations_lat_long <- read.csv("Data/locations_lat_long.csv")

# Merge the lat long back to the early_data dataframe
early_data_coords <- early_data %>%
  left_join(locations_lat_long, by = "location_string")


# Set up a workflow for when I have updated early_data 
# To save processing I will add the lat long to the newest dataframe, then isolate the unique locations that do not have a lat long coordinate

latest_data <- read.csv("Data/28_11_25 UK solitary bee museum specimen measurements - raw data.csv")

# Remove empty rows based on label_no
latest_data <- latest_data[!is.na(latest_data$label_no) & latest_data$label_no != "", ]


latest_data$location_string <- apply(latest_data[, c("specific_location",
                                                   "city_town_village",
                                                   "county",
                                                   "country")], 
                                    1, function(x) {
                                      paste(na.omit(trimws(x[x != ""])), collapse = ", ")
                                    })

# Merge the lat long back to the latest_data dataframe

latest_data_coords <- latest_data %>%
  left_join(locations_lat_long, by = "location_string")

# Isolate rows without lat long coordinates
na_latest_data_coords <- latest_data_coords %>% filter(is.na(latitude))

# Extract unique locations without lat long coordinates
latest_na_unique_locs <- unique(na_latest_data_coords$location_string)
latest_na_unique_locs_df <- tibble(location_string = latest_na_unique_locs)

# Geocode the new unique locations without lat long coordinates
latest_geo <- geocode(latest_na_unique_locs_df,
                      address = "location_string",
                      method = 'osm',
                      lat = latitude,
                      long = longitude
                      )

# Add the new geocoded locations to locations_lat_long
locations_lat_long <- bind_rows(locations_lat_long, latest_geo)

# Save the updated locations_lat_long
write.csv(locations_lat_long, "Data/locations_lat_long.csv", row.names = FALSE)

# Manually add lat long for remaining NAs 

# Finally, merge the updated locations_lat_long back to latest_data

locations_lat_long <- read.csv("Data/locations_lat_long.csv")
latest_data_coords <- latest_data %>%
  left_join(locations_lat_long, by = "location_string")

# Save the final latest_data_coords with lat long
# This is for 29th November 2025
write.csv(latest_data_coords, "Data/29_11_25_data_with_coordinates.csv", row.names = FALSE)



