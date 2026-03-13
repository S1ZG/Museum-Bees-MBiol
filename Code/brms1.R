
#Attempt at brms


#install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
library(cmdstanr)
library(tidyverse)
library(brms)
library(posterior)
library(here)
library(dplyr)

# For brms to run, need a C++ compiler (Rtools on Windows, Xcode on Mac)

# CmdStanR requires a working installation of CmdStan which can be installed with CmdStanR if you have a suitable C++ toolchain:
#check_cmdstan_toolchain()
# Should then be able ti install CmdStan
#install_cmdstan(cores = 2)

bees_temps <- read_csv(here("Data/26_02_26_bees_temps.csv"))

# Shorten measurement names
bees_temps <- bees_temps %>%
  rename(
    HW = HW_mm,
    ITD = intertegular_distance_mm,
    FW = FW_length_mm,
    tibia = tibia_length_mm
    )

# Create reference table with list of species names
sp_names <- unique(bees_temps$full_name)

# Rescale year
bees_temps$year_rescaled <- (bees_temps$year - 1800) / 100
# Scaled to "centuries since 1800" to improve model stability


# Create an empty list to store species models
species_models <- vector("list", length(sp_names))
names(species_models) <- sp_names

# Loop for one brms model per species
for (i in seq_along(sp_names)) {
  sp <- sp_names[i]
  species_data <- subset(bees_temps, full_name == sp)
}



# Fit the model
species_models[[sp]] <- brm(
  log(ITD) ~ year_rescaled + mean_preflight_temp + max_preflight_temp + sex + latitude,
  data = species_data,
  family = gaussian(), # As I have log transformed
  refresh = 0)

# Fit for all species
for(i in 1:nrow(species_data)){
  print(sp[i])
  bees_temps %>%
    filter(sp == species_data$sp[i]) -> batchDATA
  update( species_models, newdata = batchDATA, refresh = 0) -> MODEL_FIT
  save(MODEL_FIT, file = paste("ModelFits", sp[i]))
}















--------------








# Create an empty list to store species models
species_models <- vector("list", length(sp_names))
names(species_models) <- sp_names

# Loop for one brms model per species
for (i in seq_along(sp_names)) {
  sp <- sp_names[i]
  species_data <- subset(bees_temps, full_name == sp)
}



# Fit the model
species_models[[sp]] <- brm(
  log(ITD) ~ year_rescaled + mean_preflight_temp + max_preflight_temp + sex + latitude,
  data = species_data,
  family = gaussian(), # As I have log transformed
  refresh = 0)

# Fit for all species
for(i in 1:nrow(species_data)){
  print(sp[i])
  bees_temps %>%
    filter(sp == species_data$sp[i]) -> batchDATA
  update( species_models, newdata = batchDATA, refresh = 0) -> MODEL_FIT
  save(MODEL_FIT, file = paste("ModelFits", sp[i]))
}









