
# This code is for running LMs and AIC analyses

library(ggplot2)
library(dplyr)
#install.packages("ggh4x")
library(ggh4x)
library(here)


# Load in data
bees_temps <- read.csv(here("Data/14_04_26_bees_temps_5km.csv"))

# Shorten measurement names
bees_temps <- bees_temps %>%
  rename(
    HW = HW_mm,
    ITD = intertegular_distance_mm,
    FW = FW_length_mm,
    tibia = tibia_length_mm
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
#

# Check for collinearity between latitude and temp
cor(bees_temps$latitude, bees_temps$mean_preflight_temp)
# -0.01543477 - very weak


# Separate LM for each species

# Ensure variables are factors
bees_temps <- bees_temps %>%
  mutate(
    sex = factor(sex),
    ecology = factor(ecology),
    full_name = factor(full_name)
  )

species_list <- levels(bees_temps$full_name)

models_main <- list()
models_interactions <- list()

for (sp in species_list) {
  df_sp <- bees_temps %>% filter(full_name == sp)
  
  # Base model
  m1 <- lm(log_ITD ~ sex + mean_preflight_temp + year_rescaled + latitude, 
           data = df_sp)
  # Interaction model
  m2 <- lm(log_ITD ~ sex*mean_preflight_temp + sex*year_rescaled + latitude, 
           data = df_sp)
  
  models_main[[sp]] <- m1
  models_interactions[[sp]] <- m2
}


# Compare models to see if interaction is needed using AIC

aic_results <- lapply(species_list, function(sp) {
  AIC(models_main[[sp]], models_interactions[[sp]])
})

names(aic_results) <- species_list
aic_results



#
#

library(forcats)
library(broom)

# Plot coefficients grouped by ecology

# Create look-up table for each species' ecology
species_eco <- bees_temps %>%
  distinct(full_name, ecology) %>%
  arrange(ecology, full_name)

# coefficient table
coef_table <- bind_rows(lapply(species_list, function(sp) {
  tidy(models_interactions[[sp]]) %>%
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

# PLOT: Sex difference in temperature slopes by species, grouped by ecology
coef_table %>%
  filter(term == "sexmale:mean_preflight_temp") %>%
  ggplot(aes(x = estimate, y = species)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - std.error,
                     xmax = estimate + std.error),
                 height = 0.2) +
  facet_grid(ecology ~ ., scales = "free_y", space = "free_y") +
  theme_minimal() +
  labs(x = "Difference in temperature slopes between sexes", y = NULL)


#
#
#
#

# Plot male and female slopes separately

library(purrr)
library(emmeans)
library(forcats)

# Extract sex-specific temperature slopes from each species model
temp_slopes <- map_dfr(names(models_main), function(sp) {
  mod <- models_main[[sp]]
  
  eco <- bees_temps %>%
    filter(full_name == sp) %>%
    pull(ecology) %>%
    unique() %>%
    as.character()
  
  slopes <- summary(
    emtrends(mod, specs = ~ sex, var = "mean_preflight_temp"),
    infer = c(TRUE, TRUE)
  )
  
  slopes %>%
    as.data.frame() %>%
    mutate(
      species = sp,
      ecology = eco
    )
})

# optional: order species
temp_slopes <- temp_slopes %>%
  mutate(
    species = fct_relevel(species, species_list),
    sex = factor(sex, levels = c("female", "male"))
  )

# plot sex-specific slopes, grouped by ecology
ggplot(temp_slopes,
       aes(x = mean_preflight_temp.trend, y = species, color = sex)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5, colour = "grey60") +
  geom_errorbarh(
    aes(xmin = lower.CL, xmax = upper.CL),
    position = position_dodge(width = 0.55),
    height = 0.18,
    linewidth = 0.7
  ) +
  geom_point(
    position = position_dodge(width = 0.55),
    size = 2.4
  ) +
  facet_grid(ecology ~ ., scales = "free_y", space = "free_y") +
  labs(
    x = "Effect of temperature on log(ITD)",
    y = NULL,
    color = "Sex"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0),
    legend.position = "bottom"
  )












#
#
#
#
#
#
# Using emmeans instead of tidy?

library(tidyverse)
library(emmeans)
library(forcats)
library(here)

# ----------------------------
# Data prep
# ----------------------------
bees_temps <- read.csv(here("Data/17_03_26_bees_temps_5km.csv")) %>%
  filter(
    label_no != "IGproject0099",
    label_no != "IGproject0232",
    latitude <= 54
  ) %>%
  mutate(
    sex = factor(sex),
    ecology = factor(ecology),
    full_name = factor(full_name),
    year_rescaled = (year - 1800) / 100
  ) %>%
  rename(
    HW = HW_mm,
    ITD = intertegular_distance_mm,
    FW = FW_length_mm,
    tibia = tibia_length_mm
  ) %>%
  mutate(
    log_ITD = log(ITD),
    log_HW  = log(HW),
    log_FW  = log(FW),
    log_tibia = log(tibia)
  )

species_eco <- bees_temps %>%
  distinct(full_name, ecology) %>%
  arrange(ecology, full_name)

species_list <- species_eco$full_name

# ----------------------------
# Fit models
# ----------------------------
models_main <- list()
models_interactions <- list()

for (sp in species_list) {
  df_sp <- bees_temps %>% filter(full_name == sp)
  
  models_main[[sp]] <- lm(
    log_ITD ~ sex + mean_preflight_temp + year_rescaled + latitude,
    data = df_sp
  )
  
  models_interactions[[sp]] <- lm(
    log_ITD ~ sex * mean_preflight_temp + sex * year_rescaled + latitude,
    data = df_sp
  )
}

# ----------------------------
# Helper: extract emtrends slopes
# ----------------------------
extract_trends <- function(model_list, species_eco, species_list, specs_formula) {
  imap_dfr(model_list, function(mod, sp) {
    emtrends(mod, specs = specs_formula, var = "mean_preflight_temp", infer = c(TRUE, TRUE)) %>%
      as.data.frame() %>%
      mutate(species = sp)
  }) %>%
    left_join(species_eco, by = c("species" = "full_name")) %>%
    mutate(
      species = factor(species, levels = species_list),
      ecology = factor(ecology)
    )
}

# ----------------------------
# 1) Pooled temperature slope per species
#    (from the non-interaction model)
# ----------------------------
temp_main <- extract_trends(
  model_list = models_main,
  species_eco = species_eco,
  species_list = species_list,
  specs_formula = ~ 1
)

ggplot(temp_main, aes(x = mean_preflight_temp.trend, y = species)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
  geom_point() +
  geom_errorbarh(
    aes(xmin = lower.CL, xmax = upper.CL),
    height = 0.2
  ) +
  facet_grid(ecology ~ ., scales = "free_y", space = "free_y") +
  theme_minimal() +
  labs(x = "Effect of temperature on log(ITD)", y = NULL)

# ----------------------------
# 2) Sex-specific temperature slopes per species
#    (from the interaction model)
# ----------------------------
temp_sex <- extract_trends(
  model_list = models_interactions,
  species_eco = species_eco,
  species_list = species_list,
  specs_formula = ~ sex
) %>%
  mutate(sex = factor(sex, levels = c("female", "male")))

ggplot(temp_sex, aes(x = mean_preflight_temp.trend, y = species, color = sex)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5, colour = "grey60") +
  geom_errorbarh(
    aes(xmin = lower.CL, xmax = upper.CL),
    position = position_dodge(width = 0.55),
    height = 0.18,
    linewidth = 0.7
  ) +
  geom_point(
    position = position_dodge(width = 0.55),
    size = 2.4
  ) +
  facet_grid(ecology ~ ., scales = "free_y", space = "free_y") +
  labs(
    x = "Effect of temperature on log(ITD)",
    y = NULL,
    color = "Sex"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0),
    legend.position = "bottom"
  )


















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













# Not like this all pooled together, would need to do each species separately then look at the slopes and see if ecology explains trends in slopes
# Possibly even just running the LM again but for the other parts of the body? See if ecology explains trends in slopes?

ggplot(bees_temps, aes(x = log_ITD, y = log_HW, color = ecology)) +
  geom_point(alpha = 0.4) + # make it so each species has its own line
  geom_smooth(method = "lm")

  
  
  
  
  geom_smooth(method = "lm")



ggplot(bees_temps, aes(x = log_ITD, y = log_tibia, color = ecology)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm")



