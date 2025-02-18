library(readr)
library(tidyverse)
library(ggplot2)
library(factoextra)

# Imports Allium data
allium_data <- read.csv('/Users/haydenwright/Desktop/R_files/Allium_acuminatum/Allium_data.csv', row.names = 1)
# Creates subset of Allium_data with only continuous variables
allium_data <- allium_data[c(21,23,34,51:52)]
allium_data <- na.exclude(allium_data)

scaled.allium_data <- scale(allium_data)

# Runs PCA
results <- prcomp(scaled.allium_data)

# Makes negative eigenvectors postive
results$rotation <- -1*results$rotation
results$x <- -1*results$x

# Graphs results
biplot(results, scale = 1)

# Calculates percentage of variation explained by each PC
results$sdev^2 / sum(results$sdev^2)

S <- cov(scaled.allium_data)
S %>% round(2)


S.eigen <- eigen(S)

S.eigen$values %>% round(3) # Eigenvalues


S.eigen.prop <- S.eigen$values / sum(S.eigen$values)
S.eigen.prop %>% round(3) # rounding for display


S.eigen$vectors %>% round(2) # 1 vector per eigenvalue


loadings <- S.eigen$vectors[ , 1:2] %>%
  data.frame(row.names = colnames(allium_data)) %>%
  rename("PC1" = X1, "PC2" = X2) %>%
  round(digits = 3)
loadings


PC.scores <- scaled.allium_data %*% S.eigen$vectors

cor(PC.scores) %>% round(3)

barplot(S.eigen.prop)
abline(h = mean(S.eigen.prop),
       col = "red")