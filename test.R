library(readr)
library(tidyverse)
library(ggplot2)

Allium_data <- read.csv('/Users/haydenwright/Desktop/R_files/Allium_acuminatum/Allium_data.csv')

within_range <- subset(Allium_data,within_lat_long_range==1)
outside_range <- subset(Allium_data,within_lat_long_range==0)

colnames(within_range)

t.test(within_range$tepal_inner_outer_ratio_avg,outside_range$tepal_inner_outer_ratio_avg)

#chi_squared <- function(df,x,y) {
#  chisq.test(table(df$x,df$y), correct=FALSE)
#}

results <- chisq.test(table(Allium_data$within_lat_long_range,Allium_data$outer_tepal_lanceolate_acuminate), correct=FALSE)
chisq.test(table(Allium_data$within_lat_long_range,Allium_data$tepal_spread_spreading), correct=FALSE)
chisq.test(table(Allium_data$tepal_spread_curved,Allium_data$within_lat_long_range))

chi_squared(Allium_data,within_lat_long_range,outer_tepal_lanceolate_acuminate)
chi_squared(Allium_data,tepal_spread_spreading)