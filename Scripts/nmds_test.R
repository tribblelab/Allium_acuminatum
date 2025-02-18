library(readr)
library(tidyverse)
library(ggplot2)
library(vegan)
library(stats)
library(cluster)
library(factoextra)

# Imports Allium data
allium_data <- read.csv('/Users/haydenwright/Desktop/R_files/Allium_acuminatum/Allium_data.csv', row.names = 1)
allium_data$distance <- sqrt((allium_data$latitude - 46.67)^2 + (allium_data$longitude + 120.55)^2)
# Creates subset of Allium_data with only continuous variables
allium_data <- allium_data[c(21,23,34,54)]
allium_data <- na.exclude(allium_data)
allium_data <- abs(allium_data)

norm.allium_data <- scale(allium_data)

# Function to min-max normalize data
normalize <- function(x) {
  ((x - min(x)) / (max(x) - min(x)))
}

# Normalizes the continuous data columns in Allium_data_norm
norm.allium_data <- normalize(norm.allium_data)

set.seed(42)

z <- metaMDS(norm.allium_data,
             autotransform = FALSE,
             distance = "bray",
             engine = "monoMDS",
             k = 5,
             weakties = TRUE,
             model = "global",
             maxit = 300,
             try = 20,
             trymax = 100)


sp <- wascores(x = z$points, w = norm.allium_data, expand = TRUE)

gof <- goodness(object = z)

plot(z, display = "sites", type = "none")
points(z, display = "sites", cex = 2*gof/mean(gof))

stressplot(object = z, lwd = 5)

plot(z, display = "sites", type = "t")

plot(z, display = "sites", type = "n",
     xaxt = "n", xlab = "", yaxt = "n", ylab = "")
points(z, col = "red")
