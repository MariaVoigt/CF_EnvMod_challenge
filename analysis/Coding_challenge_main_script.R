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


#----------------#
# Install/Load Libraries #
#----------------#
# install.packages(c("here", "sf", "tidyverse", "terra","tidyterra",
# "ncdf4","ncdf4.helpers", "exactextractr" ))
library(here)
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(ncdf4)
options("sp_evolution_status"=2) 
# not optimal, but package seems active, 
# so hoping they'll update dependencies
library(exactextractr)

# clearing working directory when debugging
rm(list = ls())

#-----------#
# Functions #
#-----------#
clip_polygon_with_box <- function(shape, lon, lat){
  # create bounding box out of lat and lon coordinates
  bbox_coordinates <- rbind(c(lon[1],lat[1]), c(lon[2],lat[1]), c(lon[2],lat[2]), 
                            c(lon[1],lat[2]), c(lon[1],lat[1]) )
  # create box with crs of input shapefile
  bbox <- terra::vect(bbox_coordinates, "polygons", crs = crs(shape))
  return(crop(bbox, shape))
}

clip_data_shape <- function(raster_data,crop_shape, variable_name){
  if (crs(raster_data, proj = T) == crs(crop_shape, proj = T) ){
  raster_data <- raster_data[variable_name]
  raster_crop <- terra::crop(raster_data,crop_shape, mask = TRUE)
  return(raster_crop)}else {
    # if crs not the same return error
    stop("Projection doesn't match!")
  }
}

#---------#
# Globals #
#---------#

# set paths
here::i_am("src/analysis/Coding_challenge_main_script.R")

#---------------------------------#
# 1. Prepare base map #
#
# I decided to work with Portugal
# describe a bit more here
#---------------------------------#


#Import shapefile from gadm database at https://gadm.org/download_country.html
# Load shapefile for Portugal (country level information)
pt_shape <- terra::vect(here("data/raw/country_shape/gadm41_PRT_shp/gadm41_PRT_0.shp"))

# check crs
crs(pt_shape,  proj=TRUE )


# I want to work with mainland Portugal, 
# so I have to exclude Madeira and Azores

# creating a bounding box for clipping
# define longitude and latitue of mainland
pt_lon = c(-9.739,-5.778) 
pt_lat = c(36.820, 42.253)

# clip the shape of Portugal with the bounding box using customized function
pt_mainland <- clip_polygon_with_box(pt_shape, lon = pt_lon, lat = pt_lat)

# plot to check
ggplot() + 
  geom_sf(data =  sf::st_as_sf(pt_mainland), fill = "grey" )

# write out for later use and checking in qgis
# writeVector(pt_mainland, here("data/processed/study_area/mainland_portugal.shp"),
#             filetype = "ESRI Shapefile", overwrite = T)


#-----------------------------------------------------#
# 2. Load and prepare weather, landcover and soil data #
#
# description
# Weather data was downloaded from:
# for the area covering portugal
# Landcover data was downloaded from: 
# for the area covering portugal
# soil data was downloaded from: 
# in six tiles covering portugal
#---------------------------------#

# WEATHER DATA
# weather data downloaded in netcdf format
# need to be subsetted, and then cropped to Portugal

# read nc as raster
weather_data <- terra::rast(here("data/raw/weather/ERA5_Land_monthly_averaged_data_2020_2022.nc"))
# check names
names(weather_data)
# check crs match
crs(weather_data, proj = T) == crs(pt_mainland, proj = T)

## Subset weather data
# wrap this into a function, using the mainland, and the variable name

### Evaporation
# describe the kind of data this is


evapotransp_pt <- clip_data_shape(weather_data, pt_mainland, "e_")
#visualize
plot(evapotransp_pt$e_1)


### Temperature
# describe the kind of data this is
temperature_pt  <- clip_data_shape(weather_data, pt_mainland, "t2m_")

# convert to degree celsius by subtracting 273.15
temperature_pt <- temperature_pt -273.15
plot(temperature_pt$t2m_1)

## Precipitation
#!!!!!!!!!!!!!!!!!!!!!
# describe the kind of data this is
precipitation_pt <-clip_data_shape(weather_data, pt_mainland, "tp_")
#visualize
plot(precipitation_pt$tp_1)

## LANDCOVER DATA
#!!!!!!!!!!!!!!!!!!!!!
# describe the kind of data this is
# read nc as raster
# describe the kind of data this is (and where to find classes)

landcover_data <- terra::rast(here("data/raw/landcover/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"))

# check names
names(landcover_data)
# check crs match
crs(landcover_data, proj = T) == crs(pt_mainland, proj = T)

## Subset data and crop landcover to portugal
landcover_pt <-clip_data_shape(landcover_data, pt_mainland, "lccs_class")

#visualize
plot(landcover_pt)
# which and how many classes
unique(landcover_pt$lccs_class)
nrow(unique(landcover_pt$lccs_class))
hist((landcover_pt$lccs_class))

# # need to get legend values and assign as categorical values
## extracting this from the netcdf metadata
landcover_netcdf<- nc_open(here(here("data/raw/landcover/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc")))
print(landcover_netcdf)

# !!!! EDIT HERE THE THREE CROPLAND CLASSES NEED TO BE MORE DISTINGUISHABLE

# manually extracted to excel and stored in table,
# because couldn't find an easy way to extract from metadatafile  
landcover_classes <- read.csv(here("data/raw/landcover/lc_classes.csv"),
                              stringsAsFactors = F, strip.white = T)

# # add the levels so that we can work with the classes as categories
levels(landcover_pt) <- landcover_classes
# setting the third column (value = 2, counting from 0) as active
# which is the label info
activeCat(landcover_pt, layer = 1) <- 2
# # check if worked
categories(landcover_pt, value=landcover_classes, active=2)
is.factor(landcover_pt)


##  SOIL ORGANIC CARBON DATA
# data downloaded in tiles
# need to be merged first
# in t/ha in 0-30 cm depth

SOC_tiles <- list.files(here("data/raw/soil"), ".tif$", full.names=TRUE) 
# create a SpatRasterCollection to combine SpatRasters for merging
SOC_tiles_collection <- terra::sprc(lapply(SOC_tiles, rast))
SOC_merged <- merge(SOC_tiles_collection)

SOC_pt <- clip_data_shape(SOC_merged , pt_mainland, "SOC_tile1")
plot(SOC_pt)


#-----------------------------------------------------#
# 3. Data integration  #
# Reproject the different rasters to match the one with lowest resolution,
# same resolution and origin

data_names <- c("evapotransp_pt", 
                "precipitation_pt", 
                "temperature_pt",
                "landcover_pt",
                "SOC_pt")


# check all resolutions to figure out which ones need 
# to be reprojected how
for (data_name in data_names){
  print( res(get(data_name)))
}
# check extent
for (data_name in data_names){
  print(ext(get(data_name)))
}

# CONTINUE HERE

# we need resample and downscale the landcover and soil data to the weather layer
# we'll use this as the spatial raster to match the others to
resample_raster <- evapotransp_pt["e_1"]

#!!!!fix this explain more here
# how do we do that, for landcover, categorical data, I want the majority 
# of landcover in that pixel area
# method near because categorical

# consider resampling/aggregating by majority class!
landcover_pt_near <- resample(landcover_pt, resample_raster, method = "near")
#plot(landcover_pt_near )

unique(landcover_pt_near ["label"])
nrow(unique(landcover_pt_near ["label"]))

ggplot() + 
  geom_spatraster(data = landcover_pt_near) #+
# scale_fill_manual(data = test, values = rgb(rgb_frac))


landcover_pt <- exactextractr::exact_resample(landcover_pt, 
                                              resample_raster, 
                                              'majority')

# I need to do something here so the name is not e_1 from the resample raster                                                 

# wrap this in a function
#!!!!!!!!!!!!
levels(landcover_pt) <- landcover_classes
# setting the third column (value = 2, counting from 0) as active
activeCat(landcover_pt, layer = 1) <- 2
check_landcover_cats <- as.data.frame(cats(landcover_pt))
# # check if worked
#categories(landcover_pt, value=landcover_classes, active=2)
is.factor(landcover_pt)

lc_categories <- unique(landcover_pt)
landcover_classes_exist <- lc_categories %>% 
  left_join(landcover_classes, by = "label")

nrow(landcover_classes_exist)

# fix classes here
ggplot() + 
  geom_spatraster(data = landcover_pt, mapping = aes(fill = label) )   +
  scale_fill_manual(
    
    values = landcover_classes_exist$hex,
    na.value = "grey50")




ggplot() + 
  geom_spatraster(data = landcover_pt, mapping = aes(fill = label) )   +
  scale_fill_manual(
    values = landcover_classes_exist$hex,
    na.value = "cadetblue3")



# for Soil organic carbon I want the mean t/ha for that area?
# resampling with bilinear because continuous value
SOC_pt_bil <- resample(landcover_pt, resample_raster, method = "bilinear")
ggplot() + 
  geom_spatraster(data = SOC_pt_bil )   #+.  # find a typical soil value scale

# alternatively use average 
SOC_pt_mean <- resample(landcover_pt, resample_raster, method = "average")
ggplot() + 
  geom_spatraster(data = SOC_pt_mean )

# I am quiet baffled at how bad resample works here, it massively extents the 
# range of values and changes patterns --> has high values at the coast, whereas that is where actually
# the low values are and high values are in the North, bu tthere rather low values in the first two 

# testing exactextractr with function mean
SOC_pt <- exactextractr::exact_resample(SOC_pt, 
                                        resample_raster, 
                                        'mean')
ggplot() + 
  geom_spatraster(data = SOC_pt ) 
# FIX find a typical soil value scale

# this looks much better in terms of pattern and value range, so using this

# for some reason the name gets changed to the resample_raster (slightly dubious), 
# so I am fixing that
names(SOC_pt) <- "SOC_2020"

# check whether all layers have the same origin, resolution, extent and crs
compareGeom(evapotransp_pt, precipitation_pt, temperature_pt, 
            landcover_pt, SOC_pt)

# true so I have finished the data prep

#-----------------------------------------------------#
# 4. Analysis and visualization  #
# Visualize the time series of climate and soil variables in the format of your choice,
# aggregated separately for different land cover classes.
# raster calculation, make mean and stdev of each across

# soil would be across landcover in a barplot
# climate across time landcover in line for each month and three years
# we could do over time to see if there are trends
# for each month with bar


# Extract values from raster1 within the classes of raster2
variable_list <- list(evapotransp_pt, precipitation_pt, temperature_pt, 
                      landcover_pt, SOC_pt)
variables_stack <- rast(variable_list) 
variables_df <- as.data.frame(variables_stack )
names(variables_df)

# adding the labels for the landcover classes here, so we have them later

variables_df <- variables_df %>% 
  left_join(landcover_classes, by = "label")

# check how many pixels each class
variables_df_summary <- variables_df %>% 
  group_by(label) %>% 
  summarise(pixel_count = n())

# how many pixels in total
total_pixels <- sum(variables_df_summary$pixel_count)
# how many pixels is 1% of total
total_pixels /100
# that means more than 10 pixels is more than 1% of dataset

# I will filter out landcover classes that have less than 1% coverage 
# and I will also filter out water
variables_considered <- variables_df_summary[variables_df_summary$pixel_count >10 &
                                               variables_df_summary$label != "water" &
                                               variables_df_summary$label != "NA" , ]
# all have 2 or 1 pixel in that landcover class,
# water has 27, so 
27/(total_pixels /100)
# 2.5 % of total number of pixels

# I will also filter out water
# I will filter here the pixels that have landcover NA or water
# as this is an analysis of landcover and soil
# and should not be any soil organic carbon in water
# but this is likely due to a mismatch in landcover and soil layer and/or
# after resampling. On a smaller area it would be useful to clarify what is happening
# here to exclude any potential errors in the layer, for now I will move on as 
# these are 27 (water) and 12 (NA) pixels out of 1077 pixels

variables_df <- filter(variables_df, (label %in% variables_considered$label))

# colors
lc_colors <- variables_considered %>% 
  left_join(landcover_classes)

## visualise distribution of landcover classes
ggplot(variables_df)+
  geom_bar(aes(x = label, fill = label))+ 
  scale_fill_manual(
    # THIS IS WRONG
    values = lc_colors$hex )

## Visualise Soil organic carbon per landcover

# now I am summarising the soil organic carbon per landcover class
SOC_per_lc <- variables_df %>% 
  group_by(label) %>% 
  summarize(mean_SOC_2020 = mean(SOC_2020, na.rm = T), # calculating mean
            stdev_SOC_2020 = sd(SOC_2020, na.rm = T), # calculating stdeviation
            nr_pixels = n()) %>% # how many pixels in each class
  left_join(landcover_classes, by = "label")



# to fix!
# axis names, label names, make sure the yellows are improved
ggplot(SOC_per_lc)+
  geom_bar(aes(x = label, y = mean_SOC_2020, fill = label), 
           #color = "blue", fill = "white", 
           stat = "identity",
           #position = "dodge"
  )+
  scale_fill_manual(
    values = SOC_per_lc$hex )+
  geom_errorbar( aes(ymin=mean_SOC_2020-stdev_SOC_2020, 
                     ymax=mean_SOC_2020+stdev_SOC_2020, 
                     x = label), linewidth = 0.6, width=1.2)+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x=element_text(size = 12, angle = 90, hjust=0.5,vjust=0.2),
    # axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title=element_text(size = 18, face = "bold"),
    #legend.title=element_text(size=18, face = "bold") ,
    legend.position="none",
    strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
    strip.text = element_text(size = 16, face="bold"),
    plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"))


## Visualise climate variables per landcover
# month_id to year and month
month_lookup <- data.frame(month_id = c(1:36), 
                           year = rep(c(2020:2022), each= 12), 
                           month = rep(c(1:12), times= 3))

climate_per_lc <- variables_df %>% 
  dplyr::select(!(SOC_2020)) %>% 
  # change to long format so easier to work with
  pivot_longer(cols = e_1:t2m_36, 
               names_to = c('variable','month_id'),
               names_sep = '_',
               values_to = 'value',
               names_transform = list(month_id= as.numeric)) %>% 
  group_by(label, variable, month_id) %>% 
  summarize(mean_value= mean(value, na.rm = T), # calculating mean
            stdev_value = sd(value, na.rm = T)) %>%  # calculating stdeviation
  # add years and months
  left_join(month_lookup, by = "month_id") %>% 
  # for nice plot
  # CONSIDER ADDING MONTH 
  mutate(month_year = paste0(month, "_", year))


ggplot(climate_per_lc)+
  geom_line(aes(x = as.factor(month_id), y = mean_value, color = label, group = label), )+
  scale_colour_manual(
    values = landcover_classes_exist$hex )+
  facet_wrap(~variable, scales = "free")


# # HERE ADD an error range for sd maybe

# playing around with plotting for individual variables
# fix this to something definitive
test <- filter(climate_per_lc, variable == "e")

ggplot(test)+
  geom_line(aes(x = as.factor(month_id), y = mean_value, group = label), )+
  scale_colour_manual(
    values = landcover_classes_exist$hex )+
  facet_wrap(~label, scales = "fixed")


test <- filter(climate_per_lc, variable == "tp")

ggplot(test)+
  geom_line(aes(x = as.factor(month_id), y = mean_value, group = label), )+
  scale_colour_manual(
    values = landcover_classes_exist$hex )+
  facet_wrap(~label, scales = "fixed")


test <- filter(climate_per_lc, variable == "t2m")
ggplot(test)+
  geom_line(aes(x = as.factor(month_id), y = mean_value, group = label), )+
  scale_colour_manual(
    values = landcover_classes_exist$hex )+
  facet_wrap(~label, scales = "fixed")

# I could look at the 5 most important landcover classes
# with all together
variables_df_summary <- variables_df_summary %>% 
  dplyr::arrange(desc(pixel_count))



# I Could also look at monthly averages over the three variables
# I need to improve the plots to look nice especially for the years


#-----------------------------------------------------#
# 5. Sample size calculation  #

# calculate necessary sample size following
# n = ((z*sigma)/E)^2
# Assuming SOC is your variable and its standard deviation is known or estimated

# dataframe of SOC_pt
# this is looking accross all
SOC_df <- as.data.frame(SOC_pt)
head(SOC_df)
standard_deviation <- sd(SOC_df$SOC_2020) 

# Desired width of the confidence interval (10% of the mean value)
desired_width <- 0.1 * mean(SOC_df$SOC_2020)

# Z-score for 95% confidence level
confidence_level <- 0.95
z_score <- qnorm(1 - (1 - confidence_level) / 2)

# Calculate the number of samples required
nr_samples <- (z_score * standard_deviation / desired_width) ^ 2

ceiling(nr_samples)
# 28 samples needed to sample SOC within 10% of the mean value and a 95% CI

# implement this in space
sample_points <- spatSample(SOC_pt, ceiling(nr_samples), method = "random", 
                            na.rm = T, as.points = T)
sf::st_as_sf(sample_points)

ggplot() + 
  geom_spatraster(data = SOC_pt )  +
  geom_sf(data=sf::st_as_sf(sample_points), color="pink", size=1, alpha=1)



# calculate sampling size necessary per LC class
# here I need to start with all classes, because I want to show the 
# ones that can't be sampled
SOC_list <- list(landcover_pt, SOC_pt)
SOC_stack <- rast(SOC_list) 
SOC_df <- as.data.frame(SOC_stack )
names(SOC_df)

SOC_sampling_per_lc <- SOC_df %>% 
  group_by(label) %>% 
  summarize(desired_with_SOC_2020 = 0.1 * mean(SOC_2020, na.rm = T), # calculating mean
            stdev_SOC_2020 = sd(SOC_2020, na.rm = T), # calculating stdeviation
            nr_pixels = n()) %>% # how many pixels in each class
  mutate(nr_samples = ceiling((z_score * stdev_SOC_2020 / desired_with_SOC_2020) ^ 2),
         # problem is that for some there is an NA and you can't calculate number of samples
         # or the sampling requires more pixels than exist to be sampled
         # we test that
         possible = (nr_pixels > nr_samples & !is.na(nr_samples)),
         # change the number of samples for those that can't be sampled to zero
         nr_samples = ifelse(possible== FALSE, 0, nr_samples)) %>% 
  left_join(landcover_classes, by = "label") %>% 
  # we drop NA
  filter(label != "NA")


# polygonize landcover only using the categories I actually can sample,

sampling_categories <- SOC_sampling_per_lc %>% 
  filter(possible == TRUE) %>% 
  pull(label)

sampling_categories                                            

#Define the categorical values for which you want to create polygons
landcover_polygons <- as.polygons(landcover_pt, aggregate = T, values = T )
#plot
ggplot() +
  geom_sf(data=sf::st_as_sf(landcover_polygons), aes(fill = label))+
  scale_fill_manual(values = landcover_classes_exist$hex, 
                    breaks=landcover_classes_exist$label)

# subset for the classes I can sample
landcover_polygons <- landcover_polygons[landcover_polygons$label %in% sampling_categories, ]
#plot
ggplot() + 
  geom_sf(data=sf::st_as_sf(landcover_polygons), aes(fill = label))+
  scale_fill_manual(values = landcover_classes_exist$hex, 
                    breaks=landcover_classes_exist$label)


# just checking how many polygons
length(landcover_polygons)
# extract sampling sizes
sample_sizes<- SOC_sampling_per_lc %>% 
  filter(possible == TRUE) %>% 
  pull(nr_samples)
length(sample_sizes)


# Here the quality degrades quiet a lot,
# needs commenting and cleaning up
test_points <- spatSample(landcover_polygons, size = sample_sizes) 

# this is just to check if worked
# clean up library if know I need it
library(RColorBrewer)
color_palette <- brewer.pal(10, "Paired")
# looking at points to see if worked
ggplot() + 
  geom_sf(data=sf::st_as_sf(test_points ), aes(color=label), size=2, alpha=1,
          shape = 2)+
  scale_colour_manual(values = color_palette, breaks=landcover_classes_exist$label)

# here we plot it on top of the original raster as this looks nicer
# and also add the number of samples to each category
# note that all classes that have 0, means we can't sample them within the confidence we want
ggplot() + 
  geom_spatraster(data = landcover_pt )   +
  #geom_sf(data=sf::st_as_sf(landcover_polygons), aes(fill = label))+
  scale_fill_manual(values = landcover_classes_exist$hex, 
                    breaks=landcover_classes_exist$label,
                    labels = paste0(landcover_classes_exist$label, 
                                    " (sample size: ", 
                                    SOC_sampling_per_lc$nr_samples, ")"))+
  geom_sf(data=sf::st_as_sf(test_points ), color = "black", size=0.8, alpha=1)+
  labs(fill = "Landcover class with SOC sample size") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x=element_text(size = 12, angle = 90, hjust=0.5,vjust=0.2),
    # axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title=element_text(size = 18, face = "bold"),
    #legend.title=element_text(size=18, face = "bold") ,
    strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
    strip.text = element_text(size = 16, face="bold"),
    plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"))

#---------#
# The end #
#---------#


