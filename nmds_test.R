library(readr)
library(tidyverse)
library(ggplot2)
library(vegan)

# Imports Allium data
Allium_data <- read.csv('/Users/haydenwright/Desktop/R_files/Allium_acuminatum/Allium_data.csv')
# Creates subset of Allium_data that will be normalized
Allium_data_norm <- Allium_data[-c(1:5,7,10:22,25,28:34,36,40:42,45:51,54)]
# makes any negative values positive in the subset to be normalized
Allium_data_norm <- abs(Allium_data_norm)

# Function to min-max normalize data
normalize <- function(x) {
  ((x - min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE)))
}

# Scales all the data in Allium_data_norm
Allium_data_norm <- as.data.frame(lapply(Allium_data_norm, normalize))

#set.seed(42)

z <- metaMDS(comm = Allium_data_norm,
             autotransform = FALSE,
             distance = "bray",
             engine = "monoMDS",
             na.rm = TRUE,
             k = 3,
             w = FALSE,
             wascores = FALSE,
             weakties = TRUE,
             model = "global",
             maxit = 300,
             try = 20,
             trymax = 100)

gof <- goodness(object = z)

plot(z, display = "sites", type = "none")
points(z, display = "sites", cex = 2*gof/mean(gof))

stressplot(object = z, lwd = 5)

plot(z, display = "sites")
