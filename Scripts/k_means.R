library(readr)
library(tidyverse)
library(ggplot2)
library(cluster)
library(stats)
library(factoextra)

# Imports Allium data
allium_data <- read.csv('/Users/haydenwright/Desktop/R_files/Allium_acuminatum/Allium_data.csv', row.names = 1)
allium_data$distance <- sqrt((allium_data$latitude - 46.67)^2 + (allium_data$longitude + 120.55)^2)
# Creates subset of Allium_data with only continuous variables
allium_data <- allium_data[c(21,23,34,54)]
allium_data <- na.exclude(allium_data)

norm.allium_data <- scale(allium_data)

#Function to min-max normalize data
normalize <- function(x) {
  ((x - min(x)) / (max(x) - min(x)))
}

# Scales all the data in Allium_data_norm
norm.allium_data <- normalize(norm.allium_data)

k2 <- kmeans(norm.allium_data, 2)

fviz_cluster(k2, norm.allium_data)

allium_data$cluster <- k2$cluster