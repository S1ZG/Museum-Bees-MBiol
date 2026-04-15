

# This code is for running LMs and AIC analyses

library(ggplot2)
library(dplyr)
#install.packages("ggh4x")
library(ggh4x)
library(here)


# Load in data and clean
bees_temps <- read.csv(here("Data/14_04_26_bees_temps_5km.csv"))

# Log transform measurements
bees_temps <- bees_temps %>%
  mutate(
    log_ITD = log(intertegular_distance_mm),
    log_HW  = log(HW_mm),
    log_FW = log(FW_length_mm),
    log_tibia = log(tibia_length_mm)
  )


# Rescale year
bees_temps$year_rescaled <- (bees_temps$year - 1800) / 100
# Scaled to "centuries since 1800" to improve model stability

# Check for collinearity between latitude and temp
cor(bees_temps$latitude, bees_temps$mean_preflight_temp)
# -0.01543477 - very weak


library(dplyr)

bees_temps <- bees_temps %>%
  mutate(
    ecology = factor(ecology,
                     levels = c("excavator", "cavity nester", "kleptoparasite"))
  ) %>%
  arrange(ecology) %>%
  mutate(
    full_name = factor(full_name, levels = unique(full_name))
  )





#
#





# Plot 

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








# Run separate LM for each species

# Ensure variables are factors
bees_temps <- bees_temps %>%
  mutate(
    sex = factor(sex),
    ecology = factor(ecology),
    full_name = factor(full_name)
  )

species_list <- levels(bees_temps$full_name)

models_main <- list()

for (sp in species_list) {
  df_sp <- bees_temps %>% filter(full_name == sp)
  
  # Base model
  m1 <- lm(log_ITD ~ sex + mean_preflight_temp + year_rescaled + latitude, 
           data = df_sp)
  models_main[[sp]] <- m1
}







library(forcats)
library(broom)
# Plot coefficients, grouped by ecology

# Create look-up table for each species' ecology
species_eco <- bees_temps %>%
  distinct(full_name, ecology) %>%
  arrange(ecology, full_name)

# Coefficient table
coef_table <- bind_rows(lapply(species_list, function(sp) {
  tidy(models_main[[sp]]) %>%
    mutate(species = sp)
})) %>%
  left_join(species_eco, by = c("species" = "full_name"))

# Order species within ecology
species_order <- species_eco %>%
  arrange(ecology, full_name) %>%
  pull(full_name)

coef_table <- coef_table %>%
  mutate(
    species = factor(species, levels = species_order),
    ecology = factor(ecology)
  )

# PLOT: Mean preflight temperature effect by species, grouped by ecology
coef_table %>%
  filter(term == "mean_preflight_temp") %>%
  ggplot(aes(x = estimate, y = species)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - std.error,
                     xmax = estimate + std.error),
                 height = 0.2) +
  facet_grid(ecology ~ ., scales = "free_y", space = "free_y") +
  theme_minimal() +
  labs(x = "Effect of temperature on log(ITD)", y = NULL)


# PLOT: Year effect by species, grouped by ecology
coef_table %>%
  filter(term == "year_rescaled") %>%
  ggplot(aes(x = estimate, y = species)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - std.error,
                     xmax = estimate + std.error),
                 height = 0.2) +
  facet_grid(ecology ~ ., scales = "free_y", space = "free_y") +
  theme_minimal() +
  labs(x = "Effect of Year on log(ITD)", y = NULL)





# Q1: Has the body size of UK solitary bees declined?
coef_table %>%
  filter(term == "year_rescaled") %>%
  summarise(
    mean_slope = mean(estimate),
    n_negative = sum(estimate < 0),
    n_positive = sum(estimate > 0)
  )















# log-log
# Using log_ITD as the body size proxy

m_hw <- lm(log_HW ~ log_ITD, data = df_sp)
m_tibia <- lm(log_tibia ~ log_ITD, data = df_sp)
m_fw <- lm(log_FW ~ log_ITD, data = df_sp)

# with ecology
m_eco_hw <- lm(log_HW ~ log_ITD * ecology, data = bees_temps)
m_eco_tibia <- lm(log_tibia ~ log_ITD * ecology, data = bees_temps)
m_eco_fw <- lm(log_FW ~ log_ITD * ecology, data = bees_temps)


# Head width
ggplot(bees_temps, aes(x = log_ITD, y = log_HW, color = ecology)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

# Tibia length
ggplot(bees_temps, aes(x = log_ITD, y = log_tibia, color = ecology)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

# Forewing length
ggplot(bees_temps, aes(x = log_ITD, y = log_FW, color = ecology)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
































# Plot 

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
  xvar = "log_ITD",
  yvar = "log_HW",
  x_span = 1,
  y_span = 0.5
)

# Temp and ITD
make_species_plot(
  df = bees_temps,
  xvar = "log_ITD",
  yvar = "log_tibia",
  x_span = 1,
  y_span = 0.5
)

# Temp and ITD
make_species_plot(
  df = bees_temps,
  xvar = "log_ITD",
  yvar = "log_FW",
  x_span = 1,
  y_span = 0.5
)
