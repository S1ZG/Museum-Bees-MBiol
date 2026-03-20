
# This code is for running LMs and AIC analyses

library(ggplot2)


bees_temps <- read_csv(here("Data/17_03_26_bees_temps_5km.csv"))
# May want to remove sp 99, very small head measurement, need to check, and 232 (1873), no others are that early to compare to
bees_temps <- bees_temps[bees_temps$label_no != "IGproject0099" & bees_temps$label_no != "IGproject0232", ]


# Shorten measurement names
bees_temps <- bees_temps %>%
  rename(
    HW = HW_mm,
    ITD = intertegular_distance_mm,
    FW = FW_length_mm,
    tibia = tibia_length_mm
  )


# Rescale year
bees_temps$year_rescaled <- (bees_temps$year - 1800) / 100
# Scaled to "centuries since 1800" to improve model stability











