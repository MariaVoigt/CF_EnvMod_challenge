#---------------------------------------------------------#
# Script No 2 -  Climate Farmers coding challenge         #
# Tasks:                                                  # 
# - resample land cover and soil data                     #
# - write resampled rasters out                           #
#---------------------------------------------------------#

# load packages ---------------------------------------------------------
# basic
library(tidyverse)   # Collection of R packages designed for data science
library(here)        # Facilitates easy file path management within R projects
# spatial operations
library(sf)           # Comprehensive toolset for handling spatial data
library(terra)        # Efficient manipulation & analysis of raster data
library(tidyterra)    # Extends 'terra' for tidyverse-friendly raster operations
options("sp_evolution_status" = 2) 
library(exactextractr) # extraction of raster values based on geometries and resampling

here::i_am("src/analysis/2_integrate_data.R")

# 5. Data integration -----------------------------------------------------
# Reproject the land cover and SOC layer to match climate data, which has
# lower resolution and different origins



# *Import layers ----------------------------------------------------------

# I am using one climate layer to resample to the same dimension
resample_raster <- rast(here("data/processed/raster_data/evapotransp_pt.tif")) %>% 
  subset(1)

# * resample land cover layer----------------------------------------------
landcover_pt <- rast(here("data/processed/raster_data/landcover_pt.tif")) 
  

# check different methods to reproject landcover (categorical data)
# using function resample from terra with method near
landcover_pt_near <-  resample(landcover_pt, resample_raster, method = "near")


# alternative is to extract with exactextractr::exact_resample as we can 
# use a function that allows to use the majority of the fraction of cells covered
# by a certain landcover
landcover_pt_majority <- 
  exactextractr::exact_resample(landcover_pt, 
                                resample_raster, 
                                'majority')
# set landcover categories
landcover_classes <- read.csv(here("data/raw/landcover/lc_classes.csv"),
                              stringsAsFactors = F, strip.white = T)

levels(landcover_pt_majority) <- landcover_classes[c("id", "label")]


writeRaster(landcover_pt_majority, 
            filename = here("data/processed/raster_data/landcover_pt_res.tif"),
            datatype = "INT1U",
            filetype = "GTiff",
            overwrite = T)

# * resample SOC layer----------------------------------------------
# for Soil organic carbon I want the mean t/ha for that area
SOC_pt <- rast(here("data/processed/raster_data/SOC_pt.tif")) 

# test resampling with bilinear because continuous value
SOC_pt_bil <- resample(landcover_pt, resample_raster, method = "bilinear")

# or exactextractr with function mean
SOC_pt_mean <- exactextractr::exact_resample(SOC_pt, 
                                             resample_raster, 
                                             'mean')

# fixing the name of the layer, as the resampling raster name is used
names(SOC_pt_mean) <- "SOC"

# export SOC_pt_mean
writeRaster(SOC_pt_mean, 
            filename = here("data/processed/raster_data/SOC_pt_res.tif"), 
            datatype = "FLT8S",
            filetype = "GTiff",
            overwrite = T)

# end --------------------------------------------------------------------------