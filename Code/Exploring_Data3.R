
library(ggplot2)
library(dplyr)


# Read in the dataset

bees_explore <- read.csv("Data/14_04_26_bees_temps_5km.csv")


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


# Summary statistics - mean size and variation in size per species
# Using ITD as proxy for body size








