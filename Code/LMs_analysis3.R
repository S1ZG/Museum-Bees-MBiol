
# This code is for running LMs and AIC analyses

library(ggplot2)
library(dplyr)
#install.packages("ggh4x")
library(ggh4x)
library(here)
library(tidyverse)
library(emmeans)
library(forcats)


# Load in data
bees_temps <- read.csv(here("Data/26_04_26_bees_temps_5km.csv"))

# Set up
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



# Plots
# (AI assistance in producing this)

# Set species order so they are organised by ecology and consistent across plots
species_order <- c(
  "Anthidium manicatum",
  "Megachile centuncularis",
  "Hylaeus communis",
  "Hylaeus hyalinatus",
  "Andrena chrysosceles",
  "Andrena wilkella",
  "Colletes succintus",
  "Lasioglossum fulvicorne",
  "Nomada flava",
  "Nomada goodeniana",
  "Nomada ruficornis",
  "Sphecodes geoffrellus"
)

bees_temps$full_name <- factor(bees_temps$full_name, levels = species_order)

# Function for plots where I can set the window size:
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
    
    ymin <- ymid - y_span / 2
    ymax <- ymid + y_span / 2
    
    scale_y_continuous(
      limits = c(ymin, ymax),
      breaks = seq(
        floor(ymin / 0.2) * 0.2,
        ceiling(ymax / 0.2) * 0.2,
        by = 0.2
      )
    )
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

# Mean temp and ITD
make_species_plot(
  df = bees_temps,
  xvar = "mean_preflight_temp",
  yvar = "log_ITD",
  x_span = 6,
  y_span = 0.7
)+
  labs(
    title = "Effect of temperature on body size",
    x = "Mean pre-flight temperature (°C)",
    y = "log(Intertegular distance)"
  )


# Max temp and ITD
#make_species_plot(
#  df = bees_temps,
#  xvar = "max_preflight_temp",
#  yvar = "log_ITD",
#  x_span = 12,
#  y_span = 0.8
#)



# Function for year plot (set scale for x-axis)
make_species_plot_year_centered <- function(df, xvar, yvar, x_span, y_span, year_center = 1.5) {
  species_levels <- levels(factor(df$full_name))
  
  x_scales <- lapply(species_levels, function(sp) {
    d <- df %>% filter(full_name == sp)
    
    if (xvar == "year_rescaled") {
      xmid <- year_center
    } else {
      xmid <- mean(d[[xvar]], na.rm = TRUE)
    }
    
    xmin <- xmid - x_span / 2
    xmax <- xmid + x_span / 2
    
    if (xvar == "year_rescaled") {
      scale_x_continuous(
        limits = c(xmin, xmax),
        breaks = seq(
          floor(xmin / 0.5) * 0.5,
          ceiling(xmax / 0.5) * 0.5,
          by = 0.5
        ),
        labels = function(x) round(x * 100 + 1800)
      )
    } else {
      scale_x_continuous(
        limits = c(xmin, xmax)
      )
    }
  })
  
  y_scales <- lapply(species_levels, function(sp) {
    d <- df %>% filter(full_name == sp)
    ymid <- mean(d[[yvar]], na.rm = TRUE)
    
    ymin <- ymid - y_span / 2
    ymax <- ymid + y_span / 2
    
    scale_y_continuous(
      limits = c(ymin, ymax),
      breaks = seq(
        floor(ymin / 0.2) * 0.2,
        ceiling(ymax / 0.2) * 0.2,
        by = 0.2
      )
    )
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

# Plot for Year and ITD
make_species_plot_year_centered(
  df = bees_temps,
  xvar = "year_rescaled",
  yvar = "log_ITD",
  x_span = 1.5,
  y_span = 0.8
) +
  labs(
    title = "Effect of year on body size",
    x = "Year",
    y = "log(Intertegular distance)"
  )



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





# Sex-specific effects of temperature and year on log(ITD) using emtrends from interaction models

# Ecology lookup
species_eco <- bees_temps %>%
  distinct(full_name, ecology) %>%
  arrange(ecology, full_name)

species_list <- species_eco$full_name

# Helper to extract sex-specific slopes for any predictor
extract_trends <- function(model_list, var_name, specs_formula = ~ sex) {
  df <- do.call(rbind, lapply(names(model_list), function(sp) {
    mod <- model_list[[sp]]
    
    emtrends(
      mod,
      specs = specs_formula,
      var = var_name,
      infer = c(TRUE, TRUE)
    ) %>%
      as.data.frame() %>%
      mutate(species = sp)
  }))
  
  df %>%
    left_join(species_eco, by = c("species" = "full_name")) %>%
    mutate(
      species = factor(species, levels = species_list),
      ecology = factor(ecology),
      sex = factor(sex, levels = c("female", "male"))
    )
}

# Helper to plot the slopes
plot_trends <- function(df, slope_col, xlab) {
  ggplot(df, aes(x = .data[[slope_col]], y = species, color = sex)) +
    geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5, colour = "grey60") +
    geom_errorbar(
      aes(xmin = lower.CL, xmax = upper.CL),
      orientation = "y",
      position = position_dodge(width = 0.55),
      width = 0.18,
      linewidth = 0.7
    ) +
    geom_point(
      position = position_dodge(width = 0.55),
      size = 2.4
    ) +
    facet_grid(ecology ~ ., scales = "free_y", space = "free_y") +
    labs(x = xlab, y = NULL, color = "Sex") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      strip.text.y = element_text(angle = 0),
      legend.position = "bottom"
    )
}

# Sex-specific slopes for temperature
temp_sex <- extract_trends(models_interactions, "mean_preflight_temp")
plot_trends(temp_sex, "mean_preflight_temp.trend", "Effect of temperature on log(ITD)")

# Sex-specific slopes for year
year_sex <- extract_trends(models_interactions, "year_rescaled")
plot_trends(year_sex, "year_rescaled.trend", "Effect of year on log(ITD)")





# Allometry (refine)


library(dplyr)
library(broom)
library(ggplot2)

# 1. Centre temperature
bees_temps <- bees_temps %>%
  mutate(
    temp_c = mean_preflight_temp - mean(mean_preflight_temp, na.rm = TRUE)
  )

# 2. Helper function to fit + extract interaction term
extract_allometry <- function(response_var) {
  
  models <- bees_temps %>%
    split(.$full_name) %>%
    lapply(function(df_sp) {
      formula <- as.formula(paste(response_var, "~ log_ITD * temp_c"))
      lm(formula, data = df_sp)
    })
  
  bind_rows(lapply(names(models), function(sp) {
    tidy(models[[sp]]) %>%
      filter(term == "log_ITD:temp_c") %>%
      mutate(species = sp)
  })) %>%
    mutate(trait = response_var)
}

# 3. Run for each trait
coef_allometry <- bind_rows(
  extract_allometry("log_HW"),
  extract_allometry("log_FW"),
  extract_allometry("log_tibia")
)

# 4. Add ecology + ordering + effect size
coef_allometry <- coef_allometry %>%
  left_join(species_eco, by = c("species" = "full_name")) %>%
  mutate(
    ecology = factor(ecology),
    species = factor(species, levels = rev(species_order)),
    trait = factor(trait, levels = c("log_HW", "log_FW", "log_tibia")),
    slope_change_2SD = estimate * 2
  )

# 5. Plot (facetted by trait)
ggplot(coef_allometry, aes(x = estimate, y = species, colour = ecology)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = estimate - std.error,
                     xmax = estimate + std.error),
                 width = 0.2) +
  facet_wrap(~ trait, scales = "free_x") +
  theme_minimal() +
  labs(
    x = "Change in allometric slope with temperature",
    y = "Species",
    colour = "Ecology"
  ) +
  scale_color_manual(values = c("#E7B800", "#a6754b", "#b275eb"))

# 6. View effect sizes
coef_allometry %>%
  select(species, ecology, trait, estimate, std.error, slope_change_2SD)



