# Script to do xxxx

# what do I need to do

# break script into smaller subscripts
# make function for Portugal layer making and maybe some other things
# save the pt layers out to be importet
# fix all plots so they look nice

# still open in general
# -The application of a soil carbon model (e.g. using the R package soilR) and
# plotting of results for a single point using your downloaded and/or artificial
# data as input.
# - Your code translated from R to Python or vice-versa
# A more sophisticated sampling design, such as using stratified


#--------------------------------------------------------------#
# This is a script for the Climate Farmers coding challenge    #
# It loads climate and soil data                               #
# prepares them for visualization, plots data over time and    #
# designs a sampling regime                                    #
#--------------------------------------------------------------#

# load packages ---------------------------------------------------------

# basic
library(tidyverse)   # Collection of R packages designed for data science
library(rlang)       # A toolbox for working with base type (e.g. environments)

library(sf)           # Comprehensive toolset for handling spatial data
library(terra)        # Efficient manipulation & analysis of raster data
library(tidyterra)    # Extends 'terra' for tidyverse-friendly raster operations
library(ncdf4)        # Reading/writing data in netCDF format
options("sp_evolution_status" = 2) 
library(exactextractr) # extraction of raster values based on geometries and resampling
library(here)

here::i_am("src/analysis/1_read_and_prep_data.R")

# * load functions --------------------------------------------------------

# function to clip polygon with a bounding box given by its coordinates
clip_polygon_with_box <- function(shape, lon, lat){
  # create bounding box out of lat and lon coordinates
  bbox_coordinates <- rbind(c(lon[1],lat[1]), c(lon[2],lat[1]), c(lon[2],lat[2]), 
                            c(lon[1],lat[2]), c(lon[1],lat[1]) )
  # create box with crs of input shapefile
  bbox <- terra::vect(bbox_coordinates, "polygons", crs = crs(shape))
  return(crop(bbox, shape))
}

# function to clip raster with a polygon shape
clip_data_shape <- function(raster_data,crop_shape, variable_name){
  if (crs(raster_data, proj = T) == crs(crop_shape, proj = T) ){
    raster_data <- raster_data[variable_name]
    raster_crop <- terra::crop(raster_data,crop_shape, mask = TRUE)
    return(raster_crop)}else {
      # checking for crs match
      stop("Projection doesn't match!")
    }
}

# 1. prepare base map -----------------------------------------------------
# * import shapefile and set bounding box ---------------------------------

# Import shapefile from gadm database at https://gadm.org/download_country.html
# Load shapefile for Portugal (country level information)
pt_shape <- terra::vect(here("data/raw/country_shape/gadm41_PRT_shp/gadm41_PRT_0.shp"))

# I want to work with mainland Portugal, 
# so I have to exclude Madeira and Azores

# Need to define a bounding box for clipping (longitude and latitude of mainland)
pt_lon = c(-9.739,-5.778) 
pt_lat = c(36.820, 42.253)

# clip the shape of Portugal with the bounding box using customized function
pt_mainland <- clip_polygon_with_box(pt_shape, lon = pt_lon, lat = pt_lat)

# can plot the data to check
#ggplot() + 
#  geom_sf(data =  sf::st_as_sf(pt_mainland), fill = "grey" )


# 2. load and prepare weather data ----------------------------------------

# Weather data was downloaded from:https://cds.climate.copernicus.eu/#!/home
# need to subset the relevant variables, and then cropped to Portugal

# * Import raster ---------------------------------------------------------
weather_data <- 
  terra::rast(here("data/raw/weather/ERA5_Land_monthly_averaged_data_2020_2022.nc"))


# * Clip evapotranspiration data ------------------------------------------
evapotransp_pt <- clip_data_shape(weather_data, pt_mainland, "e_")


# * Clip and prepare temperature data -------------------------------------
temperature_pt <-  clip_data_shape(weather_data, pt_mainland, "t2m_")
# native unit is Kelvin, need to convert to degree celsius by subtracting 273.15
temperature_pt <- temperature_pt - 273.15



# Clip and prepare precipitation data -------------------------------------
precipitation_pt <-clip_data_shape(weather_data, pt_mainland, "tp_")


# 3. Load and prepare landcover data --------------------------------------
# Landcover data was downloaded from: https://cds.climate.copernicus.eu/#!/home

# * import global raster ---------------------------------------------------
landcover_data <- 
  terra::rast(here("data/raw/landcover/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"))

# * Clip landcover to Portugal ----------------------------
landcover_pt <- clip_data_shape(landcover_data, pt_mainland, "lccs_class")


# 4. soil organic carbon data ---------------------------------------------
# soil data was downloaded from:  https://soilgrids.org/

# in t/ha in 0-30 cm depth

# * import tif files ------------------------------------------------------
# Portugal is covered by six tiles, which need to be merged first
SOC_tiles <- list.files(here("data/raw/soil"), ".tif$", full.names=TRUE)

# create a SpatRasterCollection to combine SpatRasters for merging
SOC_tiles_collection <- terra::sprc(lapply(SOC_tiles, rast))
SOC_merged <- merge(SOC_tiles_collection)

SOC_pt <- clip_data_shape(SOC_merged , pt_mainland, "SOC_tile1")



# end --------------------------------------------------------------------------