
# This code is for running LMs and AIC analyses

library(ggplot2)
library(dplyr)


bees_temps <- read_csv(here("Data/17_03_26_bees_temps_5km.csv"))
# May want to remove sp 99, very small head measurement, need to check, and 232 (1873), no others are that early to compare to
bees_temps <- bees_temps[bees_temps$label_no != "IGproject0099" & bees_temps$label_no != "IGproject0232", ]

# replace all ecologies named "cavity nester" in bees_temps with "cavity_nester"
bees_temps$ecology <- gsub("cavity nester", "cavity_nester", bees_temps$ecology)
# (have replaced in excel now, make sure to update)

# Shorten measurement names and rename full_name to sp
bees_temps <- bees_temps %>%
  rename(
    HW = HW_mm,
    ITD = intertegular_distance_mm,
    FW = FW_length_mm,
    tibia = tibia_length_mm,
    sp = full_name
  )

bees_temps <- bees_temps %>%
  mutate(
    log_ITD = log(ITD),
    log_HW  = log(HW),
    log_FW = log(FW),
    log_tibia = log(tibia)
  )


# Rescale year
bees_temps$year_rescaled <- (bees_temps$year - 1800) / 100
# Scaled to "centuries since 1800" to improve model stability


# Check for collinearity between latitude and temp
cor(bees_temps$latitude, bees_temps$mean_preflight_temp)
# -0.09594196 - very weak


# Probably won't do this as each species is individual, and compared against one of the species (would need to decide which)
model1 <- lm(log_ITD ~ year_rescaled + mean_preflight_temp + sex + sp + latitude, data = bees_temps)

summary(model1)


# Accounts for species differences without estimating each separately
# Maybe good for Q1
library(lme4)
model2 <- lmer(log_ITD ~ year_rescaled + mean_preflight_temp + sex + latitude +
                 (1 | sp),
               data = bees_temps)

summary(model2)


# With ecology, for Q2
# Set ecology reference level
bees_temps$ecology <- as.factor(bees_temps$ecology)
bees_temps$ecology <- relevel(bees_temps$ecology, ref = "excavator")
model3 <- lmer(log_ITD ~ year_rescaled * ecology + 
                 mean_preflight_temp * ecology +
                 sex + 
                 latitude +
                 (1 | sp),
               data = bees_temps)

summary(model3)









