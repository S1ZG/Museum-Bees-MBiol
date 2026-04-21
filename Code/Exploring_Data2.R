
library(ggplot2)
library(dplyr)


# Read in the dataset

bees_explore <- read.csv("Data/14_04_26_bees_temps_5km.csv")


# Shorten measurement names
bees_explore <- bees_explore %>%
  rename(
    HW = HW_mm,
    ITD = intertegular_distance_mm,
    FW = FW_length_mm,
    tibia = tibia_length_mm
  )


# Set species order so they are organised by ecology and consistent across plots
species_order <- c(
  "Anthidium manicatum",
  "Hylaeus communis",
  "Hylaeus hyalinatus",
  "Megachile centuncularis",
  "Andrena chrysosceles",
  "Andrena wilkella",
  "Colletes succintus",
  "Lasioglossum fulvicorne",
  "Nomada flava",
  "Nomada goodeniana",
  "Nomada ruficornis",
  "Sphecodes geoffrellus"
)
bees_explore$full_name <- factor(bees_explore$full_name, levels = species_order)


# Plot the number of bee specimens measured from each year

ggplot(bees_explore, aes(x = year)) + geom_bar() + ggtitle("Specimen Collection Year Coverage") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Year Collected") + ylab("Number of Specimens")

# With no. of each sex shown
ggplot(bees_explore, aes(x = year, fill = sex)) +
  geom_bar() +
  ggtitle("Specimen Collection Year Coverage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year Collected") +
  ylab("Number of Specimens")



# Specimens per year coloured by sex ratio

year_sex <- bees_explore %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    n_female = sum(sex == "female"),
    prop_female = n_female / n_total
  )
ggplot(year_sex, aes(x = year, y = n_total, fill = prop_female)) +
  geom_col() +
  scale_fill_gradient2(low = "#4cc7c3", mid = "#b0b0b0", high = "#ed5a5a", midpoint = 0.5,
                      name = "Proportion female") +
  ggtitle("Specimens per Year (coloured by sex ratio)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year Collected") +
  ylab("Number of Specimens")



# Plot the number of specimens of each sex for each species
ggplot(bees_explore, aes(x = full_name, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Number of Specimens by Sex per Species",
    x = "Species",
    y = "Number of Specimens",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Visualise the data with each measurement over time


# All species together but log transformed
ggplot(bees_explore, aes(x = year, y = log(ITD), color = full_name))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Log Intertegular Distance (ITD) over Time") 



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








