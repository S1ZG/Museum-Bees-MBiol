
library(ggplot2)


# Read in the dataset

bees_explore <- read.csv("Data/17_03_26_bees_temps_5km.csv")

# Create a column for full name by merging genus and species
#bees$full_name <- paste(bees$genus, bees$species)

# Remove the only genus level Nomada specimens
#bees <- bees[bees$full_name != "Nomada sp.", ]

# May want to remove sp 99, very small head measurement, need to check, and 232 (1873), no others are that early to compare to
bees_explore <- bees_explore[bees_explore$label_no != "IGproject0099" & bees_explore$label_no != "IGproject0232", ]


# Shorten measurement names
bees_explore <- bees_explore %>%
  rename(
    HW = HW_mm,
    ITD = intertegular_distance_mm,
    FW = FW_length_mm,
    tibia = tibia_length_mm
  )



# Plot the number of bee specimens measured from each year

ggplot(bees_explore, aes(x = year)) +
  geom_bar() +
  ggtitle("Coverage of Measured Specimens Across Years") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year Collected") +
  ylab("Number of Specimens")


# Visualise the data with each measurement over time


# All species together but log transformed
ggplot(bees_explore, aes(x = year, y = log(ITD), color = full_name))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Log Intertegular Distance (ITD) over Time") 

# z-transformed
bees_explore$z_ITD <- scale(bees_explore$ITD)

ggplot(bees_explore, aes(x = year, y = z_ITD, color = full_name))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("z-Intertegular Distance (ITD) over Time") 








----------------

bees_explore <- bees_explore %>%
  group_by(full_name) %>%
  mutate(log_ITD_centered = log(ITD) - mean(log(ITD), na.rm = TRUE)) %>%
  ungroup()

ggplot(bees_explore, aes(x = year, y = log_ITD_centered, color = full_name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

----

library(dplyr)

bees_explore <- bees_explore %>%
  mutate(log_ITD = log(ITD)) %>%
  group_by(full_name) %>%
  mutate(log_ITD_centered = log_ITD - mean(log_ITD, na.rm = TRUE)) %>%
  ungroup()

library(ggplot2)

ggplot(bees_explore, aes(x = year, y = log_ITD_centered, color = full_name)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Change in Body Size (ITD) Over Time (Centred Within Species)",
    y = "Centered log(ITD)",
    x = "Year"
  ) +
  theme_minimal()

------------


# Starting with intertegular distance (ITD)

ggplot(bees_explore, aes(x = year, y = intertegular_distance_mm, color = full_name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Intertegular Distance (ITD) over Time by Species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year Collected") +
  ylab("Intertegular Distance (mm)") +
  labs(color = "Species")



# Then head width (HW)

ggplot(bees_explore, aes(x = year, y = HW_mm, color = full_name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Head Width (HW) over Time by Species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year Collected") +
  ylab("Head Width (mm)") +
  labs(color = "Species")


# Then forewing (FW) length

ggplot(bees_explore, aes(x = year, y = FW_length_mm, color = full_name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Forewing Length over Time by Species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year Collected") +
  ylab("Forewing Length (mm)") +
  labs(color = "Species")


# And Tibia length

ggplot(bees_explore, aes(x = year, y = tibia_length_mm, color = full_name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Tibia Length over Time by Species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year Collected") +
  ylab("Tibia Length (mm)") +
  labs(color = "Species")


# Now plot all 12 species in separate graphs on one plot, and have males and females in different colours with different lines on each plot for each species


library(ggplot2)
library(dplyr)

bees_explore <- read.csv("Data/17_03_26_bees_temps_5km.csv")

# Remove the two records you mentioned
bees_explore <- bees_explore %>%
  filter(label_no != "IGproject0099",
         label_no != "IGproject0232") %>%
  mutate(sex = as.factor(sex),
         full_name = as.factor(full_name))

ggplot(bees_explore, aes(x = year, y = intertegular_distance_mm,
                         color = sex, fill = sex)) +
  geom_point(alpha = 0.7, size = 0.5) +
  geom_smooth(aes(group = sex), method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ full_name, ncol = 3) +
  labs(
    title = "Intertegular Distance (ITD) over Time by Species and Sex",
    x = "Year Collected",
    y = "Intertegular Distance (mm)",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )














library(ggplot2)
library(dplyr)
library(tidyr)

bees_explore <- read.csv("Data/17_03_26_bees_temps_5km.csv")

bees_explore <- bees_explore %>%
  filter(label_no != "IGproject0099",
         label_no != "IGproject0232") %>%
  mutate(
    sex = as.factor(sex),
    full_name = as.factor(full_name)
  )

# Put the 4 measurements into one column
bees_long <- bees_explore %>%
  pivot_longer(
    cols = c(intertegular_distance_mm, HW_mm, tibia_length_mm), #, FW_length_mm removed
    names_to = "measurement",
    values_to = "value"
  ) %>%
  mutate(
    measurement = recode(measurement,
                         intertegular_distance_mm = "ITD",
                         HW_mm = "Head width",
                         FW_length_mm = "Forewing length",
                         tibia_length_mm = "Tibia length")
  )

ggplot(bees_long, aes(x = year, y = value, color = sex)) +
  geom_point(alpha = 0.7, size = 0.5) +
  geom_smooth(aes(group = sex), method = "lm", se = FALSE, linewidth = 0.3) +
  facet_grid(full_name ~ measurement, scales = "free_y") +
  labs(
    title = "Bee Measurements Over Time by Species and Sex",
    x = "Year Collected",
    y = "Measurement (mm)",
    color = "Sex"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    strip.text.y = element_text(angle = 0)
  )



# Without FW









library(ggplot2)
library(dplyr)
library(tidyr)

bees_explore <- read.csv("Data/17_03_26_bees_temps_5km.csv")

bees_explore <- bees_explore %>%
  filter(label_no != "IGproject0099",
         label_no != "IGproject0232") %>%
  mutate(
    sex = as.factor(sex),
    full_name = as.factor(full_name)
  )

bees_long <- bees_explore %>%
  pivot_longer(
    cols = c(intertegular_distance_mm, HW_mm, FW_length_mm, tibia_length_mm),
    names_to = "measurement",
    values_to = "value"
  ) %>%
  mutate(
    measurement = recode(measurement,
                         intertegular_distance_mm = "ITD",
                         HW_mm = "Head width",
                         FW_length_mm = "Forewing length",
                         tibia_length_mm = "Tibia length")
  )

make_ecology_plot <- function(data, ecology_name) {
  ggplot(data %>% filter(ecology == ecology_name),
         aes(x = year, y = value, color = sex)) +
    geom_point(alpha = 0.7) +
    geom_smooth(aes(group = sex), method = "lm", se = FALSE, linewidth = 0.8) +
    facet_grid(full_name ~ measurement, scales = "free_y") +
    labs(
      title = ecology_name,
      x = "Year Collected",
      y = "Measurement (mm)",
      color = "Sex"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      strip.text.y = element_text(angle = 0)
    )
}

p1 <- make_ecology_plot(bees_long, "cavity nester")
p2 <- make_ecology_plot(bees_long, "excavator")
p3 <- make_ecology_plot(bees_long, "kleptoparasite")

p1
p2
p3






