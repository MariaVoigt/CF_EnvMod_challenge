#--------------------------------------------------------#
# First script for the Climate Farmers coding challenge  #
# Tasks:                                                 # 
# - loads baseshape, climate, land cover and  soil data, #
# - clips them with the baseshape                        #
# - writes files out for later processing                #
#--------------------------------------------------------#

# load packages ---------------------------------------------------------

# basic
library(tidyverse)   # Collection of R packages designed for data science
library(here)        # Facilitates easy file path management within R projects
# spatial operations
library(sf)          # Comprehensive toolset for handling spatial data
library(terra)       # Efficient manipulation & analysis of raster data
library(tidyterra)   # Extends 'terra' for tidyverse-friendly raster operations

# set the path
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

# option to write out for later use and checking in qgis
# would switch this off if script read often and running times are important 
writeVector(pt_mainland, here("data/processed/study_area/mainland_portugal.shp"),
             filetype = "ESRI Shapefile", overwrite = T)

# 2. load and prepare weather data ----------------------------------------

# Weather data was downloaded from:https://cds.climate.copernicus.eu/#!/home
# need to subset the relevant variables, and then cropped to Portugal

# * Import raster ---------------------------------------------------------
weather_data <- 
  terra::rast(here("data/raw/weather/ERA5_Land_monthly_averaged_data_2020_2022.nc"))


# * Clip evapotranspiration data ------------------------------------------
evapotransp_pt <- clip_data_shape(weather_data, pt_mainland, "e_")

writeRaster(evapotransp_pt, 
            filename = here("data/processed/raster_data/evapotransp_pt.tif"),
            datatype = "FLT8S",
            filetype = "GTiff",
            overwrite = T)

# * Clip and prepare temperature data -------------------------------------
temperature_pt <-  clip_data_shape(weather_data, pt_mainland, "t2m_")
# native unit is Kelvin, need to convert to degree celsius by subtracting 273.15
temperature_pt <- temperature_pt - 273.15
writeRaster(temperature_pt, 
            filename = here("data/processed/raster_data/temperature_pt.tif"), 
            datatype = "FLT8S",
            filetype = "GTiff",
            overwrite = T)


# Clip and prepare precipitation data -------------------------------------
precipitation_pt <-clip_data_shape(weather_data, pt_mainland, "tp_")
writeRaster(precipitation_pt, 
            filename = here("data/processed/raster_data/precipitation_pt.tif"), 
            datatype = "FLT8S",
            filetype = "GTiff",
            overwrite = T)

# 3. Load and prepare landcover data --------------------------------------
# Landcover data was downloaded from: https://cds.climate.copernicus.eu/#!/home

# * import global raster ---------------------------------------------------
landcover_data <- 
  terra::rast(here("data/raw/landcover/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"))

# * Clip landcover to Portugal ----------------------------
landcover_pt <- clip_data_shape(landcover_data, pt_mainland, "lccs_class")
writeRaster(landcover_pt, 
            filename = here("data/processed/raster_data/landcover_pt.tif"),
            datatype = "INT1U",
            filetype = "GTiff",
            overwrite = T)

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

writeRaster(SOC_pt, 
            filename = here("data/processed/raster_data/SOC_pt.tif"), 
            datatype = "FLT8S",
            filetype = "GTiff",
            overwrite = T)

# end --------------------------------------------------------------------------