library(readr)
library(tidyverse)
library(ggplot2)

Allium_data <- read_csv("Allium_data.csv", na = "NA",
                        col_types = cols(fits_new = col_factor(levels = c("No", "Unsure", "Probably", "Yes")),
                                         outer_tepal_lanceolate_acuminate = col_factor(levels = c("No", "Yes")), 
                                         tepal_spread_spreading = col_factor(levels = c("No", "Some", "Yes")), 
                                         tepal_spread_curved = col_factor(levels = c("No", "Some", "Yes")), 
                                         tepal_spread_twisted = col_factor(levels = c("No", "Yes")), 
                                         inner_tepal_edges_ruffled = col_factor(levels = c("No", "Some", "Yes")), 
                                         inner_tepal_edges_beaded = col_factor(levels = c("No", "Some", "Yes")), 
                                         inner_tepal_edges_denticulate = col_factor(levels = c("No", "Some", "Yes")), 
                                         inner_tepal_edges_undulate = col_factor(levels = c("No", "Some", "Yes")), 
                                         inner_tepal_edges_scalloped = col_factor(levels = c("No", "Some", "Yes")), 
                                         inner_tepal_edges_rolled = col_factor(levels = c("No", "Yes")), 
                                         `bulb-coat_squarish` = col_factor(levels = c("No", "Some", "Yes")), 
                                         `bulb-coat_irregular` = col_factor(levels = c("No", "Yes")), 
                                         `bulb-coat_sinuous` = col_factor(levels = c("No", "Yes")), 
                                         `bulb-coat_variable` = col_factor(levels = c("No", "Yes")), 
                                         ovary_cresting = col_factor(levels = c("No", "Yes")), 
                                         ovary_long_projections = col_factor(levels = c("No", "Yes")),
                                         within_radius = col_factor(levels = c("No", "Yes"))))

#Creates subsets of data that will be analyzed
Allium_data <- Allium_data[-c(2:5,7,10:21,23,25,28:34,36:37,40:42,45:51,54:55)] # with lat and long seperate

#Sets row names to the accession nos.
Allium_data <- column_to_rownames(Allium_data, 'accession')

# Calculates distances from center
Allium_data$dist_from_center <- sqrt((Allium_data$latitude - 46.67)^2 + (Allium_data$longitude + 120.55)^2)

Allium_data$within_radius <- factor(Allium_data$dist_from_center <= 0.85)
levels(Allium_data$within_radius) <- c("No", "Yes")

# Splits Allium data into within range and outside of range
within_range <- subset(Allium_data,within_radius=="Yes")
outside_range <- subset(Allium_data,within_radius=="No")

# Runs T-test for one trait
t.test(within_range$tepal_inner_outer_ratio_avg,outside_range$tepal_inner_outer_ratio_avg)

# Creates empty dataframe for results
results <- list()

# Run Chi-squared for each catagorical trait and combine into results
results$outer_tepal_lanceolate_acuminate <- chisq.test(table(Allium_data$within_radius,Allium_data$outer_tepal_lanceolate_acuminate))
results$tepal_spread_spreading <- chisq.test(table(Allium_data$within_radius,Allium_data$tepal_spread_spreading))
results$tepal_spread_curved <- chisq.test(table(Allium_data$within_radius,Allium_data$tepal_spread_curved))
results$inner_tepal_edges_ruffled <- chisq.test(table(Allium_data$within_radius,Allium_data$inner_tepal_edges_ruffled))
results$inner_tepal_edges_beaded <- chisq.test(table(Allium_data$within_radius,Allium_data$inner_tepal_edges_beaded))
results$bulb_coat_squarish <- chisq.test(table(Allium_data$within_radius,Allium_data$`bulb-coat_squarish`))
results$bulb_coat_irregular <- chisq.test(table(Allium_data$within_radius,Allium_data$`bulb-coat_irregular`))
results$ovary_cresting <- chisq.test(table(Allium_data$within_radius,Allium_data$ovary_cresting))
results$ovary_long_projections <- chisq.test(table(Allium_data$within_radius,Allium_data$ovary_long_projections))

#creates histogram for inner_outer_tepal_ratio_avg
p <- ggplot() +
  geom_histogram(aes(x = within_range$tepal_inner_outer_ratio_avg), alpha = 0.5) +
  geom_histogram(aes(x = outside_range$tepal_inner_outer_ratio_avg), alpha = 0.5)

#plots the histogram
plot(p)

#creates histogram for inner_outer_tepal_ratio_avg
p <- ggplot() +
  geom_histogram(aes(x = within_range$tepal_inner_length_avg), alpha = 0.5) +
  geom_histogram(aes(x = outside_range$tepal_inner_length_avg), alpha = 0.5)

#plots the histogram
plot(p)