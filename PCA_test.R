library(readr)
library(tidyverse)
library(ggplot2)

# Imports Allium data
Allium_data <- read.csv('/Users/haydenwright/Desktop/R_files/Allium_acuminatum/Allium_data.csv')
# Creates subset of Allium_data omitting data that will not be analyzed
Allium_data_norm <- Allium_data[-c(2:5,7,10:22,25,28:34,36,40:42,45:51,54)]
# makes any negative values positive in the subset to be normalized
Allium_data_norm[-1] <- abs(Allium_data_norm[-1])

# Function to min-max normalize data
normalize <- function(x) {
  ((x - min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE)))
}

# Scales all the data in Allium_data_norm
Allium_data_norm[-1] <- as.data.frame(lapply(Allium_data_norm[-1], normalize))

# Runs PCA
results <- prcomp(na.exclude(Allium_data_norm[-1]), scale. = TRUE)

# Makes negative eigenvectors postive
results$rotation <- -1*results$rotation
results$x <- -1*results$x

# Graphs results
biplot(results, scale = 1)

# Calculates percentage of variation explained by each PC
results$sdev^2 / sum(results$sdev^2)