
# This code is for running a PCA to get a general 'size' for the bee specimens based on the body measurements taken in the museum.



# Read in the data
bees_temps <- read.csv("Data/26_02_26_bees_with_temps.csv")

# Log-transform all traits as they are on different ranges
bees_temps$log_HW  <- log(bees_temps$HW_mm)
bees_temps$log_ITD <- log(bees_temps$intertegular_distance_mm)
bees_temps$log_tibia <- log(bees_temps$tibia_length_mm)
bees_temps$log_FW <- log(bees_temps$FW_length_mm)


# Look at PCA with and without FW length

# No FW
pca_noFW <- prcomp(
  bees_temps[, c("log_HW", "log_ITD", "log_tibia")],
  center = TRUE,
  scale. = TRUE
)
# Check variance explained
summary(pca_noFW)
# Check loadings
pca_noFW$rotation
# Extract PC1
bees_temps$PC1_noFW <- pca_noFW$x[,1]


# With FW
pca_withFW <- prcomp(
  bees_temps[, c("log_HW", "log_ITD", "log_tibia", "log_FW")],
  center = TRUE,
  scale. = TRUE
)
# Check variance explained
summary(pca_withFW)
# Check loadings
pca_withFW$rotation 
# Extract PC1
bees_temps$PC1_withFW <- pca_withFW$x[,1]

# Compare with and without FW
cor(bees_temps$PC1_noFW, bees_temps$PC1_withFW)
# 0.9967183 so they are essentially identical size axes

# % variance PC1 no FW = 94.6%, with = 94.2%
# As they are so close (providing almost no new information) and I have concerns about the reliability of the measurement, I will exclude FW.







