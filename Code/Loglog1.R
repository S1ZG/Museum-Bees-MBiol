
# Load in data and clean
bees_temps <- read.csv(here("Data/17_03_26_bees_temps_5km.csv"))
# May want to remove sp 99, very small head measurement, need to check, and 232 (1873), no others are that early to compare to
bees_temps <- bees_temps[bees_temps$label_no != "IGproject0099" & bees_temps$label_no != "IGproject0232", ]

# Take out all specimens with a latitude over 54
bees_temps <- bees_temps[bees_temps$latitude <= 54, ]

# Log transform measurements
bees_temps <- bees_temps %>%
  mutate(
    log_ITD = log(intertegular_distance_mm),
    log_HW  = log(HW_mm),
    log_FW = log(FW_length_mm),
    log_tibia = log(tibia_length_mm)
  )
