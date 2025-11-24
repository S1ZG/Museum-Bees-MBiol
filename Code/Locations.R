
# Load necessary libraries
library(tidygeocoder)
library(dplyr)
library(tibble)

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
na_rows <- early_data_geo %>% filter(is.na(latitude) & !is.na(location_string))

# requires: sf, dplyr
library(sf)
library(dplyr)

# read local shapefile (download from Natural England or Protected Planet)
local_nature_reserves <- read_sf("Data/Local_Nature_Reserves_(England)___Natural_England.shp")




# clean your names similarly
early_data_geo <- early_data_geo %>%
  mutate(NAME_clean = tolower(trimws(location_string)))

# inner join by cleaned name (or approximate join with stringdist if needed)
matched <- early_data_geo %>%
  left_join(parks_sf %>% select(NAME_clean, geometry), by = "NAME_clean")

# compute centroids for matched polygons
matched_coords <- matched %>%
  filter(!is.na(geometry)) %>%
  mutate(cent = st_centroid(geometry)) %>%
  mutate(coords = purrr::map(cent, st_coordinates)) %>%
  mutate(longitude = purrr::map_dbl(coords, 1), latitude = purrr::map_dbl(coords, 2)) %>%
  st_set_geometry(NULL)  # drop geometry if desired

# update main table and provenance
early_data_geo <- early_data_geo %>%
  left_join(matched_coords %>% select(label_no, latitude, longitude), by = "label_no") %>%
  mutate(
    coordinate_method = case_when(
      !is.na(latitude) & is.na(coordinate_method) ~ "park_shapefile_centroid",
      TRUE ~ coordinate_method
    )
  )


