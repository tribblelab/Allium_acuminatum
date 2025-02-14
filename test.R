library(readr)
library(tidyverse)
library(ggplot2)

# Imports Allium data
Allium_data <- read.csv('/Users/haydenwright/Desktop/R_files/Allium_acuminatum/Allium_data.csv')
# Creates subset of Allium_data that will be normalized
Allium_data_norm <- Allium_data[-c(1:5,7,10:22,25,28:34,36,40:42,45:51,54)]
# makes any negative values positive in the subset to be normalized
Allium_data_norm <- abs(Allium_data_norm)

# Splits Allium data into within range and outside of range
within_range <- subset(Allium_data,within_lat_long_range==1)
outside_range <- subset(Allium_data,within_lat_long_range==0)

# Runs T-test for one trait
t.test(within_range$tepal_inner_outer_ratio_avg,outside_range$tepal_inner_outer_ratio_avg)

# Creates empty dataframe for results
results <- data.frame()

# Run Chi-squared for each catagorical trait and combine into results dataframe
outer_tepal_lanceolate_acuminate <- chisq.test(table(Allium_data$within_lat_long_range,Allium_data$outer_tepal_lanceolate_acuminate), correct=FALSE)
tepal_spread_spreading <- chisq.test(table(Allium_data$within_lat_long_range,Allium_data$tepal_spread_spreading), correct=FALSE)
results <- rbind(outer_tepal_lanceolate_acuminate,tepal_spread_spreading)
tepal_spread_curved <- chisq.test(table(Allium_data$within_lat_long_range,Allium_data$tepal_spread_curved), correct=FALSE)
results <- rbind(results,tepal_spread_curved)
inner_tepal_edges_ruffled <- chisq.test(table(Allium_data$within_lat_long_range,Allium_data$inner_tepal_edges_ruffled), correct=FALSE)
results <- rbind(results,inner_tepal_edges_ruffled)
inner_tepal_edges_beaded <- chisq.test(table(Allium_data$within_lat_long_range,Allium_data$inner_tepal_edges_beaded), correct=FALSE)
results <- rbind(results,inner_tepal_edges_beaded)
bulb.coat_squarish <- chisq.test(table(Allium_data$within_lat_long_range,Allium_data$bulb.coat_squarish), correct=FALSE)
results <- rbind(results,bulb.coat_squarish)
bulb.coat_irregular <- chisq.test(table(Allium_data$within_lat_long_range,Allium_data$bulb.coat_irregular), correct=FALSE)
results <- rbind(results,bulb.coat_irregular)
ovary_cresting <- chisq.test(table(Allium_data$within_lat_long_range,Allium_data$ovary_cresting), correct=FALSE)
results <- rbind(results,ovary_cresting)
ovary_long_projections <- chisq.test(table(Allium_data$within_lat_long_range,Allium_data$ovary_long_projections), correct=FALSE)
results <- rbind(results,ovary_long_projections)

# Function to min-max normalize data
normalize <- function(x) {
  ((x - min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE)))
}

# Scales all the data in Allium_data_norm
Allium_data_norm <- as.data.frame(lapply(Allium_data_norm, normalize))

# Runs a PCA analysis
results <- prcomp(na.exclude(Allium_data_norm), scale. = TRUE)
results$rotation <- -1*results$rotation
results$x <- -1*results$x


temp <- lapply(Allium_data$tepal_inner_outer_ratio_avg[!is.na(Allium_data$tepal_inner_outer_ratio_avg)]
, min_max_norm)

normalized_allium_data <- Allium_data
normalized_allium_data$tepal_inner_outer_ratio_avg <- normalize(Allium_data$tepal_inner_outer_ratio_avg)

(Allium_data$tepal_inner_outer_ratio_avg - min(Allium_data$tepal_inner_outer_ratio_avg[!is.na(Allium_data$tepal_inner_outer_ratio_avg)])) / (max(Allium_data$tepal_inner_outer_ratio_avg[!is.na(Allium_data$tepal_inner_outer_ratio_avg)]) - min(Allium_data$tepal_inner_outer_ratio_avg[!is.na(Allium_data$tepal_inner_outer_ratio_avg)]))

(Allium_data[is.numeric(Allium_data)] - min(Allium_data[is.numeric(Allium_data)]) / (max(Allium_data[is.numeric(Allium_data)] - min(Allium_data[is.numeric(Allium_data)])

library("vegan")
cascadeKM(na.exclude(Allium_data_norm), inf.gr = 2, sup.gr = 8)
plot(cascadeKM(na.exclude(Allium_data_norm), inf.gr = 2, sup.gr = 8))
