
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





#Attempt to use local nature reserve data
############


# read local shapefile (download from Natural England or Protected Planet)
local_nature_reserves <- read_sf("Data/Local_Nature_Reserves_(England)___Natural_England.shp")


library(stringr)
library(fuzzyjoin)



# st_crs(local_nature_reserves)               # see CRS (important)
# names(local_nature_reserves)                # find a human-readable name field (e.g., "NAME", "SiteName")
# print sample rows
local_nature_reserves %>% select(1:6) %>% head()




# 1) make cleaned reserve name
local_nature_reserves <- local_nature_reserves %>%
  mutate(
    NAME_clean = tolower(str_squish(NAME))
  )

# 2) make a point-on-surface for each polygon and set that as the active geometry
#    (this ensures the geometry column contains POINTs, not polygons)
reserves_pts <- local_nature_reserves %>%
  mutate(rep_point = st_point_on_surface(geometry)) %>%  # create the point geometry
  st_set_geometry("rep_point")                           # replace active geometry with the point


# 3) transform these point geometries to WGS84
reserves_pts_wgs84 <- st_transform(reserves_pts, crs = 4326)

# 4) extract lon/lat (one row per feature)
coords <- st_coordinates(reserves_pts_wgs84)            # will be n x 2 (X, Y)
reserves_pts_wgs84$longitude <- coords[,1]
reserves_pts_wgs84$latitude  <- coords[,2]

# 5) keep only desired columns as a plain data.frame for joining
reserve_points_small <- reserves_pts_wgs84 %>%
  st_set_geometry(NULL) %>%
  select(NAME_clean, latitude, longitude)







