
# This code is for getting temperature data for the locations of the museum bee specimens

# Load required libraries
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(exactextractr)
library(lubridate)
library(purrr)


# Read in measurement data with geocoded locations

bees <- read.csv("Data/25_02_26_data_with_coordinates.csv")
# Create a column for full name by merging genus and species
bees$full_name <- paste(bees$genus, bees$species)
# Remove rows with "Nomada sp." as full name, 
bees <- bees[bees$full_name != "Nomada sp.", ]
# Remove row with label_no "IGproject0232" - date is out of range
bees <- bees[bees$label_no != "IGproject0232", ]

# Read in species flight period table

flight_periods <- read.csv("Data/flight_periods_table.csv")
flight_periods <- tibble(flight_periods)



# Get temperature data
# Looking at both monthly mean air temperature (tas) and monthly maximum air temperature (tasmax)


file_path_to_tas.nc <- "Data/dap.ceda.ac.uk/badc/ukmo-hadobs/data/insitu/MOHC/HadOBS/HadUK-Grid/v1.3.1.ceda/25km/tas/mon/v20250415/"
file_path_to_tasmax.nc <- "Data/dap.ceda.ac.uk/badc/ukmo-hadobs/data/insitu/MOHC/HadOBS/HadUK-Grid/v1.3.1.ceda/25km/tasmax/mon/v20250415/" 


# Read and stack tas
tas_files <- list.files(file_path_to_tas.nc,full.names = TRUE)[-1] ## dropping 'index'
#Create a SpatRaster stack from all files
tas_list <- map(tas_files, rast)
# Change names to times
tas_list_named <- map(tas_list, function(RS){names(RS)<-  paste0('MONTH_', time(RS));return(RS)})
# Collapse the list
Collated_MonthlyTemps<- rast(tas_list_named)


# Read and stack tasmax
tasmax_files <- list.files(file_path_to_tasmax.nc,full.names = TRUE)[-1]
tasmax_list <- map(tasmax_files, rast)
tasmax_list_named <- map(tasmax_list, function(RS){names(RS)<-  paste0('MONTH_', time(RS));return(RS)})
# Collapse the list
Collated_MonthlyTemps_max<- rast(tasmax_list_named)


# CRS transform bees (lon/lat) to raster CRS and make vect
bees_sf <- st_as_sf(bees, coords = c("longitude","latitude"), crs = 4326, remove = FALSE)
bees_sf <- st_transform(bees_sf, crs = crs(Collated_MonthlyTemps))
bees_vect <- vect(bees_sf)



# Make sure dates from tas and tasmax are aligned
# Determine the layer dates (YYYY_MM) and helper to parse names
parse_layer_dates <- function(rstack){
  nm <- names(rstack)
  # Extract the YYYY-MM substring (captures first "YYYY-MM" in the name)
  ym_str <- sub(".*?(\\d{4}-\\d{2}).*", "\\1", nm)
  # Turn into first-of-month Date objects
  d <- as.Date(paste0(ym_str, "-01"))
  if(any(is.na(d))) stop("Failed to parse some layer names; inspect names(rstack)[1:20].")
  # Make sure all dates are chronological
  if(!all(order(d) == seq_along(d))){
    ord <- order(d)
    rstack <- rstack[[ord]]
    d <- d[ord]
    assign("rstack", rstack, envir = parent.frame()) # not strictly needed here, but we return d
  }
  return(d)
}

layer_dates_mean <- parse_layer_dates(Collated_MonthlyTemps)
layer_dates_max  <- parse_layer_dates(Collated_MonthlyTemps_max)

# Ensure same coverage/order for mean and max
if(!identical(layer_dates_mean, layer_dates_max)){
  stop("Mean and max rasters have different layer dates/order. Align stacks before extraction.")
}
layer_dates <- layer_dates_mean
layer_ym <- format(layer_dates, "%Y-%m")



# Get the year before collection and flight period for each specimen
get_yearbeforeflight <- function(year, start_month, end_month){
  yearbefore <- as.integer(year) - 1
  month_seq <- seq(start_month, end_month)
  sprintf("%04d-%02d", yearbefore, month_seq)
}
bees_meta <- bees %>%
  mutate(.row = row_number()) %>%
  rowwise() %>%
  mutate(
    preflight_period = list(get_yearbeforeflight(year,
                                            flight_periods$start_month[match(full_name, flight_periods$full_name)],
                                            flight_periods$end_month[match(full_name, flight_periods$full_name)]))
  ) %>%
  ungroup()



# The raster is indexed by layer number not by year-month string so:
# Map the YYYY-MM (ym) to the corresponding raster layer indices
map_ym_to_idx <- function(ym_vec){
  vapply(ym_vec, function(ym){
    idx <- which(layer_ym == ym)
    if(length(idx) != 1){
      stop(paste("No exact raster layer match for month:", ym))
    } #stops if there are duplicates or no matches, finds exactly one raster
    idx
  }, integer(1))
}
bees_meta$layer_idx_vec <- map(bees_meta$preflight_period, map_ym_to_idx)
bees_meta$period_key <- map_chr(bees_meta$layer_idx_vec, ~paste(.x, collapse = ","))


# Prepare output columns
bees_meta <- bees_meta %>%
  mutate(mean_preflight_temp = NA_real_,   # mean flight period temp (tas)
         sd_preflight_temp  = NA_real_,   # SD of the temps across the pre-flight period (tas)
         max_preflight_monthly_mean = NA_real_, # The hottest monthly mean temp (tas)
         max_preflight_monthly_max = NA_real_) # The maximum temp within the whole preflight period (tasmax)


# Create a grouped extraction loop
unique_keys <- unique(bees_meta$period_key)  # Group specimens with the same preflight period
for(k in unique_keys){
  rows <- which(bees_meta$period_key == k)
  layer_idx <- as.integer(strsplit(k, ",")[[1]])
  r_mean <- Collated_MonthlyTemps[[layer_idx]]
  r_max <- Collated_MonthlyTemps_max[[layer_idx]]
  
  ex_mean <- terra::extract(r_mean, bees_vect[rows], method = "bilinear")
  ex_max <- terra::extract(r_max, bees_vect[rows], method = "bilinear")
  
  #standardise the extract output into a matrix of points x months
  drop_id <- function(ex){
    if(is.null(ex)) return(null)
    if(ncol(ex) == 1) return(matrix(NA_real_, nrow = nrow(ex), ncol = length(layer_idx)))
    as.matrix(ex[, -1, drop = FALSE])
  }
  #apply drop_id() and fix vector cases
  vals_mean <- drop_id(ex_mean)
  vals_max <- drop_id(ex_max)
  if(is.null(vals_mean)) vals_mean <- matrix(NA_real_, nrow = length(rows), ncol = length(layer_idx))
  if(is.vector(vals_mean) && length(vals_mean) == length(layer_idx)) {
    vals_mean <- matrix(vals_mean, nrow = length(rows), ncol = length(layer_idx), byrow = TRUE)
  }
  if(is.vector(vals_max) && length(vals_max) == length(layer_idx)) {
    vals_max <- matrix(vals_max, nrow = length(rows), ncol = length(layer_idx), byrow = TRUE)
  }
  
  bees_meta$mean_preflight_temp[rows] <- rowMeans(vals_mean, na.rm = TRUE)
  bees_meta$sd_preflight_temp[rows]   <- apply(vals_mean, 1, sd, na.rm = TRUE)
  bees_meta$max_preflight_monthly_mean[rows] <- apply(vals_mean, 1, max, na.rm = TRUE)
  bees_meta$max_preflight_monthly_max[rows] <- apply(vals_max, 1, max, na.rm = TRUE)
}





