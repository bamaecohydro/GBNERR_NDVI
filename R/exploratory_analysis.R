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

#load packages of interest
library(terra)   # For handling raster data
library(raster)  # For handling raster data the OG way
library(sf)      # For spatial operations
library(tidyverse)   # For reshaping data
library(dtwclust)
library(proxy)
library(dbscan)
library(parallel)
library(doParallel)
library(TSclust)

# Define the folder containing raster files
raster_files <- list.files(path = "data//NDVI Landsat Images", pattern = "\\.tif$", full.names = TRUE)

# Load rasters as a SpatRaster object
raster_stack <- rast(raster_files)  # Creates a multi-layer raster (time series)

#For testing, lower resolation of raster by 10
raster_stack_lowres <- aggregate(raster_stack, fact = 10, fun = mean, na.rm = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Extract time series data -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert raster stack to a data frame
df <- as.data.frame(raster_stack_lowres, xy = TRUE)

# Rename columns
colnames(df) <- c("x", "y", paste0("year_", 1984:2024))

# Convert to long format
df <- df %>%
  pivot_longer(cols = starts_with("year_"), names_to = "year", values_to = "NDVI") %>%
  mutate(year = as.numeric(gsub("year_", "", year)))

# Create UID based on spatial coordinates
df <- df %>%
  mutate(UID = as.integer(as.factor(paste(x, y))))  # Assigns a unique integer to each cell

# Convert to wide format time series matrix
ts_matrix <- df %>%
  select(UID, year, NDVI) %>%
  pivot_wider(names_from = year, values_from = NDVI) %>%
  arrange(UID) %>%
  select(-UID) %>%
  as.matrix()

# Handle missing values (replace with column mean)
ts_matrix <- apply(ts_matrix, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
ts_matrix <- as.matrix(ts_matrix)  # Ensure matrix format

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 DBSCANN Clustering -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute DTW distance matrix (without parallel processing)
dtw_dist <- proxy::dist(ts_matrix, method = "LB_Keogh", window.size = 10)

# Check output
dim(dtw_dist)  # Should be (num_cells x num_cells)

# Check output
summary(as.vector(dtw_dist))  # Should not be all zeros

# Run DBSCAN with eps (radius) and minPts (minimum neighbors) 
dbscan_result <- dbscan(as.dist(dtw_dist), eps = 2, minPts = 5)

# Check cluster assignments
table(dbscan_result$cluster)

# Add cluster labels back to the data
df$cluster <- dbscan_result$cluster[df$UID]

library(RColorBrewer)

# Ensure clusters are treated as categorical
df$cluster <- as.factor(dbscan_result$cluster[df$UID])  

# Create a color palette (Distinct colors for up to 12 clusters)
palette <- brewer.pal(n = min(length(unique(df$cluster)), 12), "Paired")

# Plot with better cluster visibility
ggplot(df, aes(x, y, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal() +
  scale_color_manual(values = palette) +  # Use better colors
  labs(title = "DBSCAN Clustering Results", color = "Cluster") +
  theme(legend.position = "right")

# Plot the first raster layer
plot(raster_stack[[1]], main = "First Raster Layer with Clusters", col = terrain.colors(100))

# Overlay the cluster results
points(df$x, df$y, col = df$cluster, pch = 16, cex = 0.5)


