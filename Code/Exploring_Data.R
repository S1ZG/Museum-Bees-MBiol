
library(ggplot2)


# Read in the dataset

bees <- read.csv("Data/13_02_26 UK solitary bee museum specimen measurements - raw data.csv")

# Create a column for full name by merging genus and species
bees$full_name <- paste(bees$genus, bees$species)

# Remove the only genus level Nomada specimens
bees <- bees[bees$full_name != "Nomada sp.", ]

# May want to remove sp 99, very small head measurement, need to check, and 232 (1873), no others are that early to compare to

bees <- bees[bees$label_no != "IGproject0099" & bees$label_no != "IGproject0232", ]


# Plot the number of bee specimens measured from each year

ggplot(bees, aes(x = year)) +
  geom_bar() +
  ggtitle("Coverage of Measured Specimens Across Years") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year Collected") +
  ylab("Number of Specimens")


# Visualise the data with each measurement over time

# Starting with intertegular distance (ITD)

ggplot(bees, aes(x = year, y = intertegular_distance_mm, color = full_name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Intertegular Distance (ITD) over Time by Species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year Collected") +
  ylab("Intertegular Distance (mm)") +
  labs(color = "Species")


# Then head width (HW)

ggplot(bees, aes(x = year, y = HW_mm, color = full_name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Head Width (HW) over Time by Species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year Collected") +
  ylab("Head Width (mm)") +
  labs(color = "Species")


# Then forewing (FW) length

ggplot(bees, aes(x = year, y = FW_length_mm, color = full_name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Forewing Length over Time by Species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year Collected") +
  ylab("Forewing Length (mm)") +
  labs(color = "Species")


# And Tibia length

ggplot(bees, aes(x = year, y = tibia_length_mm, color = full_name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Tibia Length over Time by Species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year Collected") +
  ylab("Tibia Length (mm)") +
  labs(color = "Species")



