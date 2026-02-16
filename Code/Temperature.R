
# This code is for getting temperature data for the locations of the museum bee specimens

# Load required libraries
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(exactextractr)


# Read in measurement data with geocoded locations

latlong_data_practice <- read.csv("Data/22_01_26_data_with_coordinates.csv")


# Get temperature data
# Looking at both monthly mean air temperature (tas) and monthly maximum air temperature (tasmax)

# Monthly mean air temperature (tas)


## ChangeNamesToTimes
List_Rasters2<- map(List_Rasters, function(RS){names(RS)<-  paste0('MONTH_', time(RS));return(RS)})



# Match locations to temperatures