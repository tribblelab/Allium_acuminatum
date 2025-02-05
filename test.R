library(readr)
library(tidyverse)
library(ggplot2)

# Imports Allium data
Allium_data <- read.csv('/Users/haydenwright/Desktop/R_files/Allium_acuminatum/Allium_data.csv')

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
