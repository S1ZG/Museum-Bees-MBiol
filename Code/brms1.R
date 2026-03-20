
#Attempt at brms


#install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
library(cmdstanr)
library(tidyverse)
library(brms)
library(posterior)
library(here)
library(dplyr)

# For brms to run, need a C++ compiler (Rtools on Windows, Xcode on Mac)

# CmdStanR requires a working installation of CmdStan which can be installed with CmdStanR if you have a suitable C++ toolchain:
#check_cmdstan_toolchain()
# Should then be able ti install CmdStan
#install_cmdstan(cores = 2)

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

# Create reference table with list of species names
sp_names <- data.frame(species = unique(bees_temps$full_name))

# Rescale year
bees_temps$year_rescaled <- (bees_temps$year - 1800) / 100
# Scaled to "centuries since 1800" to improve model stability

Set2 = paste0('Sp.', sp_names$species)


# Define and compile model
CoreModel <- bees_temps %>%
  brm(log(ITD) ~ year_rescaled + mean_preflight_temp + max_preflight_temp + sex + latitude,
    data = ., refresh = 0)

# Refit model for each species
for(i in 1:nrow(sp_names)){
  print(Set2[i])
  bees_temps %>%
    filter(full_name == sp_names$species[i]) -> batchDATA
  update(CoreModel, newdata = batchDATA, refresh = 0) -> MODEL_FIT
  save(MODEL_FIT, file = paste("ModelFits/", Set2[i]))
}



# Correlations between predictors

# For each species, calculate the correlation between predictors

map_df(sp_names$species, function(sp_i){
  bees_temps %>%
    filter(species == sp_i) %>%
    select(year_rescaled, latitude, mean_preflight_temp, max_preflight_temp)
    cor ->x
  return(data.frame(Species = sp_i,
                    YvL = x[1,2],
                    YvMeanT = x[1,3],
                    YvMaxT = x[1,4],
                    LvMeanT = x[2,3],
                    LvMaxT = x[2,4]))
}) -> SpeciesLvlCorrs






















### CHAT:
library(tidyverse)
library(brms)
library(posterior)
library(here)

bees_temps <- read_csv(here("Data/17_03_26_bees_temps_5km.csv"))

# May want to remove sp 99, very small head measurement, need to check, and 232 (1873), no others are that early to compare to
bees_temps <- bees_temps[bees_temps$label_no != "IGproject0099" & bees_temps$label_no != "IGproject0232", ]


bees_temps <- bees_temps %>%
  rename(
    HW = HW_mm,
    ITD = intertegular_distance_mm,
    FW = FW_length_mm,
    tibia = tibia_length_mm
  ) %>%
  mutate(
    year_rescaled = (year - 1800) / 100,
    log_ITD = log(ITD),
    sex = as.factor(sex)
  )

sp_names <- tibble(full_name = unique(bees_temps$full_name))
Set2 <- paste0("Sp.", make.names(sp_names$full_name))

dir.create("ModelFits", showWarnings = FALSE)

SpeciesLvlCorrs <- map_df(sp_names$full_name, function(sp_i){
  
  df <- bees_temps %>%
    filter(full_name == sp_i) %>%
    select(log_ITD, year_rescaled, latitude,
           mean_preflight_temp, max_preflight_temp) %>%
    drop_na()
  
  x <- cor(df)
  
  tibble(
    Species = sp_i,
    ITD_vs_Year   = x["log_ITD", "year_rescaled"],
    ITD_vs_Lat    = x["log_ITD", "latitude"],
    ITD_vs_MeanT  = x["log_ITD", "mean_preflight_temp"],
    ITD_vs_MaxT   = x["log_ITD", "max_preflight_temp"],
    Year_vs_Lat   = x["year_rescaled", "latitude"],
    Year_vs_MeanT = x["year_rescaled", "mean_preflight_temp"],
    Year_vs_MaxT  = x["year_rescaled", "max_preflight_temp"],
    Lat_vs_MeanT  = x["latitude", "mean_preflight_temp"],
    Lat_vs_MaxT   = x["latitude", "max_preflight_temp"]
  )
})


species_models <- vector("list", length = nrow(sp_names))
names(species_models) <- sp_names$full_name

for(i in seq_len(nrow(sp_names))) {
  
  sp_i <- sp_names$full_name[i]
  message("Fitting: ", sp_i)
  
  batchDATA <- bees_temps %>%
    filter(full_name == sp_i) %>%
    drop_na(log_ITD, year_rescaled, mean_preflight_temp, max_preflight_temp, sex, latitude)
  
  species_models[[sp_i]] <- brm(
    log_ITD ~ year_rescaled + mean_preflight_temp + max_preflight_temp + sex + latitude,
    data = batchDATA,
    refresh = 0,
    chains = 4,
    cores = 4,
    iter = 2000,
    file = file.path("ModelFits", Set2[i])
  )
}


Posterior_Summaries <- map_df(names(species_models), function(sp_i){
  
  fit <- species_models[[sp_i]]
  
  fixef_df <- as.data.frame(fixef(fit, probs = c(0.167, 0.833))) %>%
    rownames_to_column("term") %>%
    mutate(Species = sp_i)
  
  fixef_df
})


Posterior_Summaries %>%
  filter(term %in% c("year_rescaled", "mean_preflight_temp", "max_preflight_temp")) %>%
  ggplot(aes(x = Estimate, y = Species)) +
  geom_point() +
  geom_errorbarh(aes(xmin = Q16.7, xmax = Q83.3), height = 0.2) +
  facet_wrap(~term, scales = "free_x") +
  theme_minimal() +
  labs(
    x = "Posterior estimate",
    y = "Species"
  )


























