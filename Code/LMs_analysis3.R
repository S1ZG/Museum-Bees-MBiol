
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
bees_temps <- read.csv(here("Data/14_04_26_bees_temps_5km.csv"))

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
  "Megachile centuncularis",
  "Hylaeus hyalinatus",
  "Hylaeus communis",
  "Anthidium manicatum",
  "Lasioglossum fulvicorne",
  "Colletes succintus",
  "Andrena wilkella",
  "Andrena chrysosceles",
  "Sphecodes geoffrellus",
  "Nomada ruficornis",
  "Nomada goodeniana",
  "Nomada flava"
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




# Using emmeans to plot the effect of mean temperature on logITD


# Helper: extract emtrends slopes
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

# 1) Pooled temperature slope per species
#    (from the non-interaction model)
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


# Helper: extract emtrends slopes
extract_trends <- function(model_list, species_eco, species_list, specs_formula,
                           weights = "proportional") {
  imap_dfr(model_list, function(mod, sp) {
    emtrends(
      mod,
      specs = specs_formula,
      var = "mean_preflight_temp",
      infer = c(TRUE, TRUE),
      weights = weights
    ) %>%
      as.data.frame() %>%
      mutate(species = sp)
  }) %>%
    left_join(species_eco, by = c("species" = "full_name")) %>%
    mutate(
      species = factor(species, levels = species_list),
      ecology = factor(ecology)
    )
}

# Overall temperature slope per species, but from the sex-dependent model
temp_overall <- extract_trends(
  model_list = models_interactions,
  species_eco = species_eco,
  species_list = species_list,
  specs_formula = ~ 1,
  weights = "proportional"   # or "equal" if you want female + male averaged equally
)

ggplot(temp_overall, aes(x = mean_preflight_temp.trend, y = species)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
  geom_point() +
  geom_errorbarh(
    aes(xmin = lower.CL, xmax = upper.CL),
    height = 0.2
  ) +
  facet_grid(ecology ~ ., scales = "free_y", space = "free_y") +
  theme_minimal() +
  labs(x = "Overall effect of temperature on log(ITD)", y = NULL)


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




# Same but for year and logITD


# ----------------------------
# Helper: extract emtrends slopes
# ----------------------------
extract_trends_var <- function(model_list, species_eco, species_list, specs_formula, var_name) {
  imap_dfr(model_list, function(mod, sp) {
    emtrends(mod, specs = specs_formula, var = var_name, infer = c(TRUE, TRUE)) %>%
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
# 1) Pooled year slope per species
# ----------------------------
year_main <- extract_trends_var(
  model_list = models_main,
  species_eco = species_eco,
  species_list = species_list,
  specs_formula = ~ 1,
  var_name = "year_rescaled"
)

ggplot(year_main, aes(x = year_rescaled.trend, y = species)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
  geom_point() +
  geom_errorbarh(
    aes(xmin = lower.CL, xmax = upper.CL),
    height = 0.2
  ) +
  facet_grid(ecology ~ ., scales = "free_y", space = "free_y") +
  theme_minimal() +
  labs(x = "Effect of year on log(ITD)", y = NULL)

# ----------------------------
# 2) Sex-specific year slopes per species
# ----------------------------
year_sex <- extract_trends_var(
  model_list = models_interactions,
  species_eco = species_eco,
  species_list = species_list,
  specs_formula = ~ sex,
  var_name = "year_rescaled"
) %>%
  mutate(sex = factor(sex, levels = c("female", "male")))

ggplot(year_sex, aes(x = year_rescaled.trend, y = species, color = sex)) +
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
    x = "Effect of year on log(ITD)",
    y = NULL,
    color = "Sex"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0),
    legend.position = "bottom"
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



m <- lm(log_ITD ~ log_HW * mean_preflight_temp, data = df_sp)

m


m0 <- lm(log_ITD ~ log_HW, data = df_sp)
df_sp$allom_resid <- resid(m0)

lm(allom_resid ~ mean_preflight_temp, data = df_sp)
