
# This code is for running LMs and AIC analyses

library(ggplot2)
library(dplyr)


bees_temps <- read_csv(here("Data/17_03_26_bees_temps_5km.csv"))
# May want to remove sp 99, very small head measurement, need to check, and 232 (1873), no others are that early to compare to
bees_temps <- bees_temps[bees_temps$label_no != "IGproject0099" & bees_temps$label_no != "IGproject0232", ]


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


model1 <- lm(log_ITD ~ year_rescaled + mean_preflight_temp + sex + sp, data = bees_temps)

summary(model1)

library(lme4)
model2 <- lmer(log_ITD ~ year_rescaled + mean_preflight_temp + sex +
                 (1 | sp),
               data = bees_temps)

summary(model2)











