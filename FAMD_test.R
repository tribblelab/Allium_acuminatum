library(readr)
library(ggplot2)
library(FactoMineR)
library(factoextra)

# Imports Allium data
Allium_data <- read_csv("Allium_data_2.csv", 
                        col_types = cols(outer_tepal_lanceolate_acuminate = col_factor(levels = c("No", "Some", "Yes")), 
                                         tepal_spread_spreading = col_factor(levels = c("No", "Some", "Yes")), 
                                         tepal_spread_curved = col_factor(levels = c("No", "Some", "Yes")), 
                                         tepal_spread_twisted = col_factor(levels = c("No", "Some", "Yes")), 
                                         inner_tepal_edges_ruffled = col_factor(levels = c("No", "Some", "Yes")), 
                                         inner_tepal_edges_beaded = col_factor(levels = c("No", "Some", "Yes")), 
                                         inner_tepal_edges_denticulate = col_factor(levels = c("No", "Some", "Yes")), 
                                         inner_tepal_edges_undulate = col_factor(levels = c("No", "Some", "Yes")), 
                                         inner_tepal_edges_scalloped = col_factor(levels = c("No", "Some", "Yes")), 
                                         inner_tepal_edges_rolled = col_factor(levels = c("No", "Some", "Yes")), 
                                         `bulb-coat_honeycomb` = col_factor(levels = c("No", "Some", "Yes")), 
                                         `bulb-coat_squarish` = col_factor(levels = c("No", "Some", "Yes")), 
                                         `bulb-coat_irregular` = col_factor(levels = c("No", "Some", "Yes")), 
                                         `bulb-coat_sinuous` = col_factor(levels = c("No", "Some", "Yes")), 
                                         `bulb-coat_variable` = col_factor(levels = c("No", "Some", "Yes")), 
                                         ovary_cresting = col_factor(levels = c("No", "Some", "Yes")), 
                                         ovary_long_projections = col_factor(levels = c("No", "Some", "Yes"))))

# Function to min-max normalize data
normalize <- function(x) {
  ((x - min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE)))
}

# Normalizes the continuous data columns in Allium_data_norm
Allium_data_norm[c(4:5,8,14:15)] <- as.data.frame(lapply(Allium_data_norm[c(4:5,8,14:15)], normalize))

# Creates subsets of data that will be normalized
Allium_data_norm <- Allium_data[-c(1:5,7,10:21,23,25,28:34,36,40:42,45:51,54)]

# Runs FAMD
res.famd <- FAMD(na.exclude(Allium_data_norm), ncp = 5, sup.var = NULL, ind.sup = NULL, graph = TRUE)

#Visualizes FAMD
fviz_pca_ind(res.famd, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

#fviz_famd_var(res.famd, repel = TRUE)