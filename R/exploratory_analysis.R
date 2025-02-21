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
library(terra)   # For handling raster data
library(raster)  # For handling raster data the OG way
library(sf)      # For spatial operations
library(tidyverse)   # For reshaping data
library(dtwclust)    # For k-Shape clustering
library(proxy)  # For DTW distance function
library(mapview)  # For visualizing results
library(TSclust)  # For time series clustering

# Define the folder containing raster files
raster_files <- list.files(path = "data//NDVI Landsat Images", pattern = "\\.tif$", full.names = TRUE)

# Load SET location data
set <- read_csv('data/pitchford_2022_table1.csv')

# Load rasters as a SpatRaster object
raster_stack <- rast(raster_files)  # Creates a multi-layer raster (time series)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Crop raster to SET data --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert SET data to spatial object
set <- set %>% st_as_sf(coords = c('long','lat'), crs = 4326)

# Create bounding box around points
mask <- st_bbox(set)  # Get bounding box (xmin, ymin, xmax, ymax)
mask <- st_as_sfc(mask)
mask <- st_buffer(mask, 100)
mask <- vect(mask)

# Crop the raster stack with the mask
raster_stack <- crop(raster_stack, mask)

# Visualize the cropped raster
plot(raster_stack[[1]])
set %>% st_geometry() %>% plot(add = TRUE, pch = 19, col = 'red')
mapview(mask) + mapview(set)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 Extract time series data -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert raster stack to a data frame
df <- as.data.frame(raster_stack, xy = TRUE)

# Rename columns to match years
colnames(df) <- c("x", "y", paste0("year_", 1984:2024))

# Convert to long format (one row per coordinate per year)
df <- df %>%
  pivot_longer(cols = starts_with("year_"), names_to = "year", values_to = "NDVI") %>%
  mutate(year = as.numeric(gsub("year_", "", year)))  # Clean the year column

# Create UID based on spatial coordinates (to uniquely identify each cell)
df <- df %>%
  mutate(UID = as.integer(as.factor(paste(x, y))))  # Assigns a unique integer to each cell

# Convert to wide format time series matrix
ts_matrix <- df %>%
  select(UID, year, NDVI) %>%
  pivot_wider(names_from = year, values_from = NDVI) %>%
  arrange(UID) %>%
  select(-UID) %>%
  as.matrix()  # Time series matrix (rows = cells, columns = years)

# Apply Z-score standardization to the time series matrix
ts_matrix_standardized <- apply(ts_matrix, 2, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4.0 Apply k-Shape Clustering -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Apply k-Shape clustering using DTW distance
kshape_result <- tsclust(ts_matrix_standardized, 
                         type = "partitional", 
                         k = 5, 
                         distance = "dtw",  # Dynamic Time Warping (DTW) distance
                         centroid = "shape")  # Shape-based centroid

# View the clustering results
summary(kshape_result)

# Extract cluster assignments
cluster_assignments <- kshape_result@cluster

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5.0 Visualize Results --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add cluster assignments to the original dataframe
df_cluster <- df %>%
  pivot_wider(names_from = year, values_from = NDVI) %>%
  arrange(UID) %>% 
  mutate(kshape_cluster = cluster_assignments) %>% 
  pivot_longer(-c(UID, kshape_cluster, x, y), names_to="year", values_to = "NDVI")

# Visualize the clusters on a map using ggplot2
df_cluster %>%
  group_by(kshape_cluster, year) %>%
  summarise(NDVI_median = median(NDVI, na.rm=T)) %>%
  mutate(year = as.numeric(year)) %>% 
  ggplot(aes(x = year, y = NDVI_median, color = as.factor(kshape_cluster))) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(title = "Median NDVI Time Series by Cluster",
       x = "Year", y = "NDVI",
       color = "Cluster") 

#Plot on map
df_cluster %>% 
  select(x,y,kshape_cluster) %>% 
  unique() %>% 
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  mapview()
  
# Plot the first raster layer
plot(raster_stack[[1]], main = "First Raster Layer with Clusters")

# Overlay the cluster results
points(df_cluster$x, df_cluster$y, col = df_cluster$kshape_cluster, pch = 16, cex = 0.5)
