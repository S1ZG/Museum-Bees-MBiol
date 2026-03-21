
# This code is for running LMs and AIC analyses

library(ggplot2)
library(dplyr)
#install.packages("ggh4x")
library(ggh4x)

bees_temps <- read_csv(here("Data/17_03_26_bees_temps_5km.csv"))
# May want to remove sp 99, very small head measurement, need to check, and 232 (1873), no others are that early to compare to
bees_temps <- bees_temps[bees_temps$label_no != "IGproject0099" & bees_temps$label_no != "IGproject0232", ]

# replace all ecologies named "cavity nester" in bees_temps with "cavity_nester"
bees_temps$ecology <- gsub("cavity nester", "cavity_nester", bees_temps$ecology)
# (have replaced in excel now, make sure to update)


# Take out all specimens with a latitude over 54
bees_temps <- bees_temps[bees_temps$latitude <= 54, ]


# Shorten measurement names and rename full_name to sp
bees_temps <- bees_temps %>%
  rename(
    HW = HW_mm,
    ITD = intertegular_distance_mm,
    FW = FW_length_mm,
    tibia = tibia_length_mm
    #,sp = full_name
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


# Separate LM for each species
model1 <- lm(log_ITD ~ year_rescaled + mean_preflight_temp + sex + sp + latitude, data = bees_temps)









# Plots

# Plot function to set window size for each plot:
# (AI assistance in producing this)
make_species_plot <- function(df, xvar, yvar, x_span, y_span) {
  species_levels <- levels(factor(df$full_name))
  
  x_scales <- lapply(species_levels, function(sp) {
    d <- df %>% filter(full_name == sp)
    xmid <- mean(d[[xvar]], na.rm = TRUE)
    scale_x_continuous(limits = c(xmid - x_span / 2, xmid + x_span / 2))
  })
  
  y_scales <- lapply(species_levels, function(sp) {
    d <- df %>% filter(full_name == sp)
    ymid <- mean(d[[yvar]], na.rm = TRUE)
    scale_y_continuous(limits = c(ymid - y_span / 2, ymid + y_span / 2))
  })
  
  ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]], colour = sex)) +
    geom_point(alpha = 0.6, size = 1.5) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ full_name, scales = "free") +
    ggh4x::facetted_pos_scales(
      x = x_scales,
      y = y_scales
    ) +
    labs(
      x = xvar,
      y = yvar,
      colour = "Sex"
    ) +
    theme_minimal(base_size = 11)
}



# Temp and ITD
make_species_plot(
  df = bees_temps,
  xvar = "mean_preflight_temp",
  yvar = "log_ITD",
  x_span = 6,
  y_span = 0.7
)

# Year and ITD
make_species_plot(
  df = bees_temps,
  xvar = "year_rescaled",
  yvar = "log_ITD",
  x_span = 1.65,
  y_span = 0.8
)


# Temp and ITD
make_species_plot(
  df = bees_temps,
  xvar = "max_preflight_temp",
  yvar = "log_ITD",
  x_span = 12,
  y_span = 0.8
)




