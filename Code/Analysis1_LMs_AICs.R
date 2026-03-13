
# This code is for running my LMs and AIC analyses

library(ggplot2)


bees <- read.csv("Data/26_02_26_bees_with_temps.csv")

# Log transform measurements
bees$log_HW  <- log(bees$HW_mm)
bees$log_ITD <- log(bees$intertegular_distance_mm)
bees$log_tibia <- log(bees$tibia_length_mm)
bees$log_FW <- log(bees$FW_length_mm)

# Rescale year
bees$year_rescaled <- (bees$year - 1800) / 100
# Scaled to "centuries since 1800" to improve model stability


# Convert sex into a factor
bees$sex <- as.factor(bees$sex)






# uhhhhh idk
# 
# 
# 

# Model LMs for each species, and create models with both mean temp and max temp to compare


model_mean <- lm(log_ITD ~ sex + year_rescaled + latitude + mean_preflight_temp, data = bees)

model_max <- lm(log_ITD ~ sex + year_rescaled + latitude + max_preflight_monthly_max, data = bees)













library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)

# read (your path)
bees <- read.csv("Data/26_02_26_bees_with_temps.csv")

# transforms you already have (keeps your pipeline)
bees <- bees %>%
  mutate(
    log_HW    = log(HW_mm),
    log_ITD   = log(intertegular_distance_mm),
    log_tibia = log(tibia_length_mm),
    log_FW    = ifelse(is.na(FW_length_mm), NA, log(FW_length_mm)),
    year_rescaled = (year - 1800) / 100,
    sex = as.factor(sex)
  )

# Choose response (change to log_HW, log_tibia etc. if you prefer)
response_var <- "log_ITD"

# Select predictors you wanted displayed
predictors <- c("mean_preflight_temp", "max_preflight_monthly_max", "latitude")

# make a long dataframe for plotting (one predictor per row)
plot_df <- bees %>%
  select(full_name, sex, !!sym(response_var), all_of(predictors)) %>%
  pivot_longer(
    cols = all_of(predictors),
    names_to = "predictor",
    values_to = "predictor_value"
  ) %>%
  filter(!is.na(predictor_value), !is.na(!!sym(response_var))) %>%
  # keep species as factor with stable ordering
  mutate(full_name = factor(full_name))

# Quick scatter + lm per species × predictor, with separate lines for sexes
p <- ggplot(plot_df, aes(x = predictor_value, y = !!sym(response_var))) +
  geom_point(aes(shape = sex), alpha = 0.5, size = 1.6) +
  # separate linear fits per sex
  geom_smooth(aes(linetype = sex, group = interaction(sex, full_name)),
              method = "lm", se = FALSE) +
  facet_grid(rows = vars(full_name), cols = vars(predictor), scales = "free_x") +
  labs(x = "Predictor value", y = response_var, 
       title = "Per-species regressions: predictors (cols) × species (rows)\nSeparate lines for each sex") +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0),
    panel.spacing = unit(0.5, "lines"),
    axis.title = element_text(size = 10)
  )

print(p)

# ---------------------------------------------------------------------
# If you want numeric model summaries: fit LMs per species × predictor,
# and also optionally per-sex models. Save tidy results.
# ---------------------------------------------------------------------

# function to fit lm for a species and predictor (optionally split by sex)
fit_models <- function(df, response, predictor, by_sex = FALSE) {
  if (!by_sex) {
    mod <- lm(as.formula(paste(response, "~", predictor)), data = df)
    broom::tidy(mod) %>% mutate(predictor = predictor, species = unique(df$full_name), sex = NA)
  } else {
    # returns a row per sex
    df %>% group_by(sex) %>% group_modify(~ {
      mod <- lm(as.formula(paste(response, "~", predictor)), data = .x)
      broom::tidy(mod) %>% mutate(predictor = predictor, species = unique(.x$full_name), sex = unique(.x$sex))
    }) %>% ungroup()
  }
}

# loop over species & predictors
model_results <- plot_df %>%
  group_by(full_name) %>%
  group_modify(~ {
    species_df <- .x
    do.call(rbind, lapply(predictors, function(pred) {
      # drop NA predictor rows
      s_df <- species_df %>% filter(predictor == pred)
      if (nrow(s_df) < 10) return(NULL)  # skip tiny samples
      # two options: combined model and per-sex models
      rbind(
        fit_models(s_df, response_var, "predictor_value", by_sex = FALSE),
        fit_models(s_df, response_var, "predictor_value", by_sex = TRUE)
      )
    }))
  }) %>% ungroup()

# glance at result
model_results %>% arrange(species, predictor, sex) %>% head(20)

# Optional: check correlations between mean and max preflight temps
cor_df <- bees %>% select(mean_preflight_temp, max_preflight_monthly_max) %>% filter(complete.cases(.))
cor_val <- cor(cor_df$mean_preflight_temp, cor_df$max_preflight_temp)
cat("Correlation mean vs max preflight temp:", round(cor_val, 3), "\n")

# If correlation is high (say > 0.6), avoid including both in the same model without checking VIF.
# Example VIF check if you do a joint model later:
# library(car)
# mod_joint <- lm(log_ITD ~ mean_preflight_temp + max_preflight_temp + sex + latitude + year_rescaled, data = bees)
# car::vif(mod_joint)




















# Q1: lmer hierarchical models (simple, clear)
# Assumes your data.frame is named `bees`. If not, read it:
# bees <- read.csv("path/to/your_bees_file.csv")

library(lme4)
library(lmerTest)   # gives p-values for lmer
library(broom.mixed) # tidy summaries (optional)
library(ggplot2)

# 1. Ensure key variables exist ------------------------------------------------
if(!"log_ITD" %in% names(bees)) bees$log_ITD <- log(bees$intertegular_distance_mm)

# rescale year as you described (centuries since 1800)
if(!"year_rescaled" %in% names(bees)) bees$year_rescaled <- (bees$year - 1800) / 100

# center temperature predictors (improves interpretability)
bees$mean_preflight_temp_c <- bees$mean_preflight_temp - mean(bees$mean_preflight_temp, na.rm = TRUE)
bees$max_preflight_monthly_max_c <- bees$max_preflight_monthly_max - mean(bees$max_preflight_monthly_max, na.rm = TRUE)

# make sure sex is a factor and species column exists
bees$sex <- factor(bees$sex)
bees$full_name <- as.factor(bees$full_name)

# optionally ensure there's a location grouping (county or similar) for another random effect
if(!"county" %in% names(bees)) bees$county <- factor(ifelse(is.na(bees$county), "unknown", bees$county))

# keep only complete cases for the variables in the model
keep_vars <- c("log_ITD","year_rescaled","mean_preflight_temp_c","max_preflight_monthly_max_c","sex","latitude","full_name","county")
df <- bees[complete.cases(bees[, intersect(names(bees), keep_vars)]), keep_vars]

# 2. Fit hierarchical models ---------------------------------------------------
# Model A: year + mean temperature
m_mean <- lmer(log_ITD ~ year_rescaled + mean_preflight_temp_c + sex + latitude +
                 (1 + year_rescaled | full_name) + (1 | county),
               data = df, REML = FALSE)

# Model B: year + max_preflight_monthly_max (your corrected variable)
m_max  <- lmer(log_ITD ~ year_rescaled + max_preflight_monthly_max_c + sex + latitude +
                 (1 + year_rescaled | full_name) + (1 | county),
               data = df, REML = FALSE)

# 3. Quick model summaries & comparison ---------------------------------------
summary(m_mean)    # fixed-effects estimates + random effects
summary(m_max)

# Compare AIC (lower is better)
AIC(m_mean, m_max)

# If you want likelihood-ratio test (careful: nested models only):
# anova(m_mean, m_max)

# 4. Extract species-specific year slopes (random slopes) ---------------------
# population-level (fixed) slope for year:
fixef(m_max)["year_rescaled"]

# species-level slopes: fixed + random slope per species
species_coefs <- coef(m_max)$full_name   # data.frame with intercept and slope per species
# keep only the year slope
species_slopes <- data.frame(species = rownames(species_coefs),
                             slope_year = species_coefs[,"year_rescaled"])
head(species_slopes)

# 5. Simple diagnostic checks -------------------------------------------------
# Residual vs fitted
plot(resid(m_max) ~ fitted(m_max), main = "Residuals vs Fitted (m_max)")
abline(h = 0, lty = 2)

# QQ of residuals
qqnorm(resid(m_max)); qqline(resid(m_max))

# 6. Optional: quick plot of species slopes -----------------------------------
ggplot(species_slopes, aes(x = slope_year)) +
  geom_histogram(binwidth = 0.005) +
  labs(title = "Distribution of species-specific year slopes",
       x = "Slope of year_rescaled (per species)",
       y = "Count")

# 7. Save species slopes for downstream analysis
# write.csv(species_slopes, "species_year_slopes.csv", row.names = FALSE)







