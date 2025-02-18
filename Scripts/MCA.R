library(readr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(tibble)
library(remotes)
library(missMDA)

#Imports Allium data, making all categorical variables in 'factor' data type
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

#Make level names for all categorical variables unique
Allium_data$outer_tepal_lanceolate_acuminate <- factor(Allium_data$outer_tepal_lanceolate_acuminate, labels = c("No1", "Yes1"))
Allium_data$tepal_spread_spreading <- factor(Allium_data$tepal_spread_spreading, labels = c("No2", "Some2", "Yes2"))
Allium_data$tepal_spread_curved <- factor(Allium_data$tepal_spread_curved, labels = c("No3", "Some3", "Yes3"))
Allium_data$tepal_spread_twisted <- factor(Allium_data$tepal_spread_twisted, labels = c("No4", "Yes4"))
Allium_data$inner_tepal_edges_ruffled <- factor(Allium_data$inner_tepal_edges_ruffled, labels = c("No5", "Some5", "Yes5"))
Allium_data$inner_tepal_edges_beaded <- factor(Allium_data$inner_tepal_edges_beaded, labels = c("No6", "Some6", "Yes6"))
Allium_data$inner_tepal_edges_denticulate <- factor(Allium_data$inner_tepal_edges_denticulate, labels = c("No7", "Some7", "Yes7"))
Allium_data$inner_tepal_edges_undulate <- factor(Allium_data$inner_tepal_edges_undulate, labels = c("No8", "Some8", "Yes8"))
Allium_data$inner_tepal_edges_scalloped <- factor(Allium_data$inner_tepal_edges_scalloped, labels = c("No9", "Some9", "Yes9"))
Allium_data$inner_tepal_edges_rolled <- factor(Allium_data$inner_tepal_edges_rolled, labels = c("No10", "Yes10"))
Allium_data$`bulb-coat_squarish` <- factor(Allium_data$`bulb-coat_squarish`, labels = c("No12", "Some12", "Yes12"))
Allium_data$`bulb-coat_irregular` <- factor(Allium_data$`bulb-coat_irregular`, labels = c("No13", "Yes13"))
Allium_data$`bulb-coat_sinuous` <- factor(Allium_data$`bulb-coat_sinuous`, labels = c("No14", "Yes14"))
Allium_data$`bulb-coat_variable` <- factor(Allium_data$`bulb-coat_variable`, labels = c("No15", "Yes15"))
Allium_data$ovary_cresting <- factor(Allium_data$ovary_cresting, labels = c("No16", "Yes16"))
Allium_data$ovary_long_projections <- factor(Allium_data$ovary_long_projections, labels = c("No17", "Yes17"))

# Calculates distances from center
Allium_data$dist_from_center <- sqrt((Allium_data$latitude - 46.67)^2 + (Allium_data$longitude + 120.55)^2)

Allium_data$within_radius <- factor(Allium_data$dist_from_center <= 0.85)
levels(Allium_data$within_radius) <- c("No", "Yes")

#Creates subsets of data that will be analyzed
Allium_data_norm <- Allium_data[-c(2:5,7,10:25,28:34,35:37,40:42,45:51,54:55)] # with lat and long seperate
#Allium_data_norm <- Allium_data[-c(2,4:5,7,10:21,23,25,28:34,36:37,40:42,45:54)] # with distance alone

#Sets row names to the accession nos.
Allium_data_norm <- column_to_rownames(Allium_data_norm, 'accession')

# Imputes missing values
res.impute <- imputeMCA(Allium_data_norm, ncp = 2, quanti.sup=10:11, quali.sup=12)

# performs MCA
res.famd <- MCA(Allium_data_norm, ncp = 2, quanti.sup=10:11, quali.sup=12, ind.sup = NULL, graph = TRUE, tab.disj = res.impute$tab.disj)

# Visualizes MCA
fviz_mca_biplot(res.famd, axes = c(1, 2), habillage = "within_radius", repel = TRUE, geom = "point", alpha.var=0, shape.ind = 19) +
  coord_cartesian(ylim = c(-1.25, 1.5))

