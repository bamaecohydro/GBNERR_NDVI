#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Exploratory Analysis Plots
# Coder: Nate Jones (cnjones7@ua.edu)
# Date: 2/24/2025
# Purpose: Plots for Shelbie
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Exploratory Analysis
# Coder: Nate Jones (cnjones7@ua.edu)
# Date: 2/13/2025
# Purpose: Explore variatin in GBNERR NDVI Values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup workspace ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
rm(list=ls())

# Load necessary libraries
library(tidyverse)

#read cluster data in
df <- read_csv("output/k_shape_clusters.csv") %>% rename(cluster = kshape_cluster)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Create plots -------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df %>%
  filter(cluster == 1) %>% 
  group_by(cluster, year) %>%
  summarise(
    NDVI_Q1     = quantile(NDVI, 0.25, na.rm = TRUE), 
    NDVI_median = median(NDVI, na.rm = TRUE), 
    NDVI_Q3     = quantile(NDVI, 0.75, na.rm = TRUE)) %>%
  mutate(year = as.numeric(year)) %>% 
  ggplot(aes(x = year)) +
    geom_ribbon(aes(ymin = NDVI_Q1, ymax = NDVI_Q3), fill = "grey60", alpha = 0.2) +
    geom_line(aes(y = NDVI_median), col = 'grey30', lty = 2, lwd = 2) +
    #Add predefined black/white theme
    theme_bw() +
    #Change font size of axes
    theme(
      axis.title.y = element_text(size = 14), 
      axis.text.y  = element_text(size = 10)
    ) + 
    #Add labels
    xlab('Year') + 
    ylab("NDVI") 
      
