library(readr)
library(tidyverse)
library(ggplot2)
library(clustMixType)

# Imports Allium data, with categorical variables as 'factor' data type
Allium_data <- read_csv("Allium_data.csv", 
                        col_types = cols(outer_tepal_lanceolate_acuminate = col_factor(levels = c("No", "Yes")), 
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
                                         `bulb-coat_sinuous` = col_factor(levels = c("No", "Some", "Yes")), 
                                         `bulb-coat_variable` = col_factor(levels = c("No", "Some", "Yes")), 
                                         ovary_cresting = col_factor(levels = c("No", "Yes")), 
                                         ovary_long_projections = col_factor(levels = c("No", "Yes"))))

# Creates subsets of data that will be normalized
Allium_data_norm <- Allium_data[-c(1:5,7,10:21,23,25,28:34,36,40:42,45:51,54)]

# Function to min-max normalize data
normalize <- function(x) {
  ((x - min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE)))
}

# Normalizes the continuous data columns in Allium_data_norm
Allium_data_norm[c(4:5,8,14:15)] <- as.data.frame(lapply(Allium_data_norm[c(4:5,8,14:15)], normalize))

# Apply k-prototype, imputing NA values after analysis
res.kpr = kproto(Allium_data_norm, 3, na.rm = "imp.onestep")

# Visualize k-prototype
clprofiles(res.kpr, Allium_data_norm)