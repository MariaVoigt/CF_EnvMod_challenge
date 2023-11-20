#---------------------------------------------------------#
# Script No 4 -  Climate Farmers coding challenge         #
# Tasks:                                                  # 
# - calculate sample size                                 # 
# - prepare visualisation of samples.                     #
#---------------------------------------------------------#
rm(list = ls())

# load packages ---------------------------------------------------------
# basic
library(tidyverse)   # Collection of R packages designed for data science
library(here)        # Facilitates easy file path management within R projects
# spatial operations
library(sf)           # Comprehensive toolset for handling spatial data
library(terra)        # Efficient manipulation & analysis of raster data
library(tidyterra)    # Extends 'terra' for tidyverse-friendly raster operations

here::i_am("src/analysis/4_Sampling_size_calculation.R")


# 6. Sample size calculation ---------------------------------------------------
# calculate necessary sample size following
# n = ((z*sigma)/E)^2
# to detect changes in SOC if I want a 95% confidence interval equal or less 
# than 10% of the mean value, assuming a Gaussian distribution.

# create a dataframe of SOC_pt to extract standard deviation 
SOC_pt <- rast(here("data/processed/raster_data/SOC_pt_res.tif"))
SOC_df <- as.data.frame(SOC_pt)
standard_deviation <- sd(SOC_df$SOC) 

# Desired width of the confidence interval (10% of the mean value)
desired_width <- 0.1 * mean(SOC_df$SOC)

# Z-score for 95% confidence level
confidence_level <- 0.95
z_score <- qnorm(1 - (1 - confidence_level) / 2)

# Calculate the number of samples required
nr_samples <- (z_score * standard_deviation / desired_width) ^ 2
# 28 samples needed to sample SOC within 10% of the mean value and a 95% CI

# Loading the non reprojected layer to check what influence this has on the sample size
SOC_pt_orig <- rast(here("data/processed/raster_data/SOC_pt.tif"))
SOC_orig_df <- as.data.frame(SOC_pt_orig)


# * Implement sampling in space ------------------------------------------------


# using the function spatSample and total number of samples necessary to 
# implement random sampling across Portugal
sample_points <- spatSample(SOC_pt, ceiling(nr_samples), method = "random", 
                            na.rm = T, as.points = T)

# * Prepare data for stratified sampling per landcover class in space ----------
# calculate sampling size necessary per LC class
# here I need to start with all classes, because I want to show the 
# ones that can't be sampled

# import landcover_pt 
# create a dataframe of SOC_pt to extract standard deviation 
landcover_pt <- rast(here("data/processed/raster_data/landcover_pt_res.tif"))
# import landcover classes for plotting
landcover_classes <- read.csv(here("data/raw/landcover/lc_classes.csv"),
                              stringsAsFactors = F, strip.white = T)

# make a combined dataframe of the complete landcover and SOC
SOC_list <- list(landcover_pt, SOC_pt)
SOC_stack <- rast(SOC_list) 
SOC_df <- as.data.frame(SOC_stack )

# prepare the dataframe and calculate number of samples per lc class
SOC_sampling_per_lc <- SOC_df %>% 
  group_by(label) %>% 
  summarize(desired_with_SOC = 0.1 * mean(SOC, na.rm = T), # calculating mean
            stdev_SOC = sd(SOC, na.rm = T), # calculating stdeviation
            nr_pixels = n()) %>% # how many pixels in each class
  mutate(nr_samples = ceiling((z_score * stdev_SOC / desired_with_SOC) ^ 2),
         # problem is that for some there is an NA and you can't calculate number of samples
         # or the sampling requires more pixels than exist to be sampled
         # so including this information
         possible = (nr_pixels > nr_samples & !is.na(nr_samples)),
         # change the number of samples for those that can't be sampled to zero
         nr_samples = ifelse(possible== FALSE, NA, nr_samples)) %>% 
  left_join(landcover_classes, by = "label") %>% 
  # we drop NA because that doesn't have a meaning
  filter(label != "NA")

# we can only supply a vector with different sample sizes for landcover classes
# when using a polygon, so we polygonize landcover, but only using the categories
# I actually can sample
sampling_categories <- SOC_sampling_per_lc %>% 
  filter(possible == TRUE) %>% 
  pull(label)

#Define the categorical values for which to create polygons
landcover_polygons <- as.polygons(landcover_pt, aggregate = T, values = T )
# visualise polygon
# ggplot() +
#   geom_sf(data=sf::st_as_sf(landcover_polygons), aes(fill = label))+
#   scale_fill_manual(values = landcover_classes$hex, 
#                     breaks=landcover_classes$label)

# subset for the classes I can sample
landcover_polygons <- landcover_polygons[landcover_polygons$label %in% sampling_categories, ]
# visualise
# ggplot() + 
#   geom_sf(data=sf::st_as_sf(landcover_polygons), aes(fill = label))+
#   scale_fill_manual(values = landcover_classes$hex, 
#                     breaks=landcover_classes$label)

# extract sampling sizes
sample_sizes<- SOC_sampling_per_lc %>% 
  filter(possible == TRUE) %>% 
  pull(nr_samples)

# * Create stratified sampling points and plot ---------------------------------
# creating points for sampling the landcover polygons with sample sizes
# according to each land cover class
sample_points_strat <- spatSample(landcover_polygons, size = sample_sizes) 


# end --------------------------------------------------------------------------
