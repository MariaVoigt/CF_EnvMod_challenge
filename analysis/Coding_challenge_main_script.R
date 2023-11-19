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



# set up ------------------------------------------------------------------
# * clean up --------------------------------------------------------------
rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
gc(verbose = TRUE)

pkgs <- names(utils::sessionInfo()$otherPkgs)
pkgs <- paste('package:', pkgs, sep = "")

suppressWarnings(if(any(pkgs != "package:")) 
  lapply(pkgs, detach, character.only = TRUE, unload = TRUE))

rm(pkgs)

# * load libraries --------------------------------------------------------

#install if required
# install.packages(c("sf", "tidyverse", "terra","tidyterra", 
#                    "ncdf4","ncdf4.helpers", "exactextractr" ))

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(ncdf4)
options("sp_evolution_status" = 2) 
# not optimal, but package seems active, 
# so hoping they'll update dependencies
library(exactextractr)

# * load functions --------------------------------------------------------

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

# 1. prepare base map -----------------------------------------------------

# I decided to work with Portugal
# describe a bit more here

# * import shapefile and set bounding box ---------------------------------

# Import shapefile from gadm database at https://gadm.org/download_country.html
# Load shapefile for Portugal (country level information)
pt_shape <- terra::vect("./data/raw/country_shape/gadm41_PRT_shp/gadm41_PRT_0.shp")

# check crs
crs(pt_shape,  proj = TRUE )

# I want to work with mainland Portugal, 
# so I have to exclude Madeira and Azores

# creating a bounding box for clipping
# define longitude and latitue of mainland
pt_lon = c(-9.739,-5.778) 
pt_lat = c(36.820, 42.253)

# clip the shape of Portugal with the bounding box using customized function
pt_mainland <- clip_polygon_with_box(pt_shape, lon = pt_lon, lat = pt_lat)

# * quick plot data -------------------------------------------------------
ggplot() + 
  geom_sf(data =  sf::st_as_sf(pt_mainland), fill = "grey" )

# write out for later use and checking in qgis
# writeVector(pt_mainland, ("./data/processed/study_area/mainland_portugal.shp"),
#             filetype = "ESRI Shapefile", overwrite = T)

# 2. load and prepare weather data ----------------------------------------

# Weather data was downloaded from:
# need to be subsetted, and then cropped to Portugal

# * import raster ---------------------------------------------------------
weather_data <- 
  terra::rast("./data/raw/weather/ERA5_Land_monthly_averaged_data_2020_2022.nc")

names(weather_data)

# check whether we need to adjust crs to the basemap
crs(weather_data, proj = T) == crs(pt_mainland, proj = T)
# we don't 

warning('Wird das noch benötigt bzw. fehlt hier etwas?')
warning('Kommentare ggf. anpassen. Besser ist es wenn Kommentare darstellen warum dieser Schritt notwenig ist und nicht was passiert')


# * Clip evaporation data ------------------------------------------------------
warning('Describe data')
evapotransp_pt <- clip_data_shape(weather_data, pt_mainland, "e_")

warning('Falls du das Sktipt am Ende in das R Markdown Skript einbindest, solltest du die Plots auskommentieren um Resource zu schonen.')
#visualize
plot(evapotransp_pt$e_1)

# * temperature data ------------------------------------------------------
warning('Describe data')
temperature_pt <-  clip_data_shape(weather_data, pt_mainland, "t2m_")
# convert to degree celsius by subtracting 273.15
temperature_pt <- temperature_pt - 273.15

plot(temperature_pt$t2m_1)

# * precipitation data ----------------------------------------------------
warning('Describe data')

precipitation_pt <-clip_data_shape(weather_data, pt_mainland, "tp_")
#visualize
plot(precipitation_pt$tp_8)

# 3. load and prepare landcover data --------------------------------------
# for the area covering portugal
# Landcover data was downloaded from: 

# * import raster ---------------------------------------------------------

landcover_data <- 
  terra::rast("./data/raw/landcover/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc")

warning('check and how to describe this')
# check names
names(landcover_data)

# * subset data and crop landcover to portugal ----------------------------
landcover_pt <- clip_data_shape(landcover_data, pt_mainland, "lccs_class")

#visualize
plot(landcover_pt)
# which and how many classes
unique(landcover_pt$lccs_class)
nrow(unique(landcover_pt$lccs_class))
hist((landcover_pt$lccs_class))

landcover_netcdf<- nc_open(("./data/raw/landcover/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"))
print(landcover_netcdf)

warning('Edit here the three yellow classes')
# extracted from metadata
landcover_classes <- read.csv("./data/raw/landcover/lc_classes.csv",
                              stringsAsFactors = F, strip.white = T)

#edit the landcover class information, so we can plot colours according
# to hex values in metadata

levels(landcover_pt) <- landcover_classes
# setting the third column (value = 2, counting from 0) as active
activeCat(landcover_pt, layer = 1) <- 2
# # check if worked
categories(landcover_pt, value=landcover_classes, active=2)
is.factor(landcover_pt)
warning('fix this layer thing because I might not need it')

# 4. soil organic carbon data ---------------------------------------------
warning('fix this')
# soil data was downloaded from: 
# in six tiles covering portugal
# data downloaded in tiles
# need to be merged first
# in t/ha in 0-30 cm depth

# * import tif files ------------------------------------------------------

SOC_tiles <- list.files("./data/raw/soil", ".tif$", full.names=TRUE) 

# create a SpatRasterCollection to combine SpatRasters for merging
SOC_tiles_collection <- terra::sprc(lapply(SOC_tiles, rast))
SOC_merged <- merge(SOC_tiles_collection)

SOC_pt <- SOC_pt <- clip_data_shape(SOC_merged , pt_mainland, "SOC_tile1")
#Visualize
plot(SOC_pt)

# 5. Data integration -----------------------------------------------------

# * check resolution of the different layers ------------------------------
# Reproject the different rasters to match the one with lowest resolution,
# same resolution and origin

warning('Bis hier habe ich mir den Code angeschaut. Ohne die Landcover komme ich hier allerindgs nicht mehr sinnvoll weiter')

data_names <- 
  c("evapotransp_pt", 
    "precipitation_pt", 
    "temperature_pt",
    "landcover_pt",
    "SOC_pt")
# check all resolutions
warning('is there a nicer way to do this?')
for (data_name in data_names){
  print( res(get(data_name)))
}
for (data_name in data_names){
  print(ext(get(data_name)))
}
# we need to lower the resolution of the landcover and soil data to 
# the weather layer, and also change the origin


# * resample landcover layer----------------------------------------------

# we'll use this as the first layer as a raster to match the others to
resample_raster <- evapotransp_pt["e_1"]

# check different methods to reproject landcover (categorical data)
# using function resample from terra with method near
landcover_pt_near <-  resample(landcover_pt, resample_raster, method = "near")
#visualize
ggplot() + 
  geom_spatraster(data = landcover_pt_near) 

# alternative is to extract with exactextractr::exact_resample as we can 
# use a function that allows to use the majority of the fraction of cells covered
# by a certain landcover
landcover_pt_majority <- 
  exactextractr::exact_resample(landcover_pt, 
                                resample_raster, 
                                'majority')
warning('forgot the categories again, fix this')
ggplot() + 
  geom_spatraster(data = landcover_pt_majority) 
# this looks much better, the classes are more continuous and less fragmented
# will work with this layer
landcover_pt <- landcover_pt_majority
# need to redefine categories
levels(landcover_pt) <- landcover_classes
# setting the third column (value = 2, counting from 0) as active
activeCat(landcover_pt, layer = 1) <- 2
check_landcover_cats <- as.data.frame(cats(landcover_pt))
# # check if worked
#categories(landcover_pt, value=landcover_classes, active=2)
is.factor(landcover_pt)

# I make a dataframe with the lc categories that exist 
# in the lower resolution version to be used in plotting later
lc_categories <- unique(landcover_pt)
landcover_classes_exist <- lc_categories %>% 
  left_join(landcover_classes, by = "label")

nrow(landcover_classes_exist)
# we have 16 classes to work with

ggplot() + 
  geom_spatraster(data = landcover_pt, mapping = aes(fill = label) )   +
  scale_fill_manual(
    values = landcover_classes_exist$hex,
    na.value = "cadetblue3")

# * resample SOC layer----------------------------------------------
# for Soil organic carbon I want the mean t/ha for that area

# test resampling with bilinear because continuous value
SOC_pt_bil <- resample(landcover_pt, resample_raster, method = "bilinear")
ggplot() + 
  geom_spatraster(data = SOC_pt_bil )  

# alternatively use average with 
SOC_pt_average <- resample(landcover_pt, resample_raster, method = "average")
ggplot() + 
  geom_spatraster(data = SOC_pt_average )

# both resamples seem identical and to have a larger range of values (+ 50 t/ha)
# and different pattern (high values at the coast, instad of low like in original)
 

# testing exactextractr with function mean
SOC_pt_mean <- exactextractr::exact_resample(SOC_pt, 
                                        resample_raster, 
                                        'mean')
ggplot() + 
  geom_spatraster(data = SOC_pt_mean ) 
# this looks much better in terms of pattern, although the value range is lower
# but I'll use this method, also to be consistent with the landcover layer
SOC_pt <- SOC_pt_mean

# fixing the name of the layer, as the resample raster name is used
names(SOC_pt) <- "SOC_2020"

# * check resample of landcover and SOC ----------------------------------------

# check whether all layers have the same origin, resolution, extent and crs
compareGeom(evapotransp_pt, precipitation_pt, temperature_pt, 
            landcover_pt, SOC_pt)

# TRUE, dataprep finished

# 5. Analysis and visualization ------------------------------------------------
# Visualize SOC across landcover classes
# Visualise weather/climate across time landcover over time

# * Prepare dataframe for analysis----------------------------------------------

# dataframe with all variables for aggregating per landcover class
variable_list <- list(evapotransp_pt, precipitation_pt, temperature_pt, 
                      landcover_pt, SOC_pt)
variables_stack <- rast(variable_list) 
variables_df <- as.data.frame(variables_stack )
names(variables_df)

# * Prepare landcover classes for analysis--------------------------------------

# adding the labels for the landcover classes here, so we have them later
variables_df <- variables_df %>% 
  left_join(landcover_classes, by = "label")

# We might not want to consider all the classes if they 
# only cover few pixels, so I will check this 
variables_df_summary <- variables_df %>% 
  group_by(label) %>% 
  summarise(pixel_count = n())

# nr of pixels in total
total_pixels <- sum(variables_df_summary$pixel_count)
total_pixels 
#1066 -> 1% of dataset is >10 pixels

# I will filter out landcover classes that have less than 1% coverage 
# and I will also filter out water (27 pixels - 2.5%) as that was not the point
# of the  analysis and pixels without landcover class
# other landcover classes excluded have 1 or 2 pixels each
variables_considered <- 
  variables_df_summary[variables_df_summary$pixel_count >10 &
                         variables_df_summary$label != "water" &
                         variables_df_summary$label != "NA" , ]

# having water pixels or NA but this is likely due to a mismatch with the landcover 
# and soil layer and/or after resampling. On a smaller area it would be useful 
# to clarify what is happening
# here to exclude any potential errors in the layer, for now I will move on as 
# these are 27 (water) and 12 (NA) pixels out of 1066 pixels

variables_df <- filter(variables_df, (label %in% variables_considered$label))

# * Visualise distribution of landcover classes --------------------------------

# lookup for colors for remaining classes
landcover_colors <- variables_considered %>% 
  left_join(landcover_classes)

## visualise distribution of landcover classes
ggplot(variables_df)+
  geom_bar(aes(x = label, fill = label))+ 
  scale_fill_manual(
    values = landcover_colors$hex )+
  theme_bw()
warning('make my own theme here')

# * Visualise Soil organic carbon per landcover --------------------------------

SOC_per_lc <- variables_df %>% 
  group_by(label) %>% 
  summarize(mean_SOC_2020 = mean(SOC_2020, na.rm = T), # calculating mean
            stdev_SOC_2020 = sd(SOC_2020, na.rm = T), # calculating stdeviation
            nr_pixels = n()) %>% # how many pixels in each class
  left_join(landcover_classes, by = "label")

warning('fix axis names, label names, make sure the yellows are improved
        make my own theme')

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

# * Visualise climate variables per landcover ----------------------------------

# month_id to year and month
month_lookup <- data.frame(month_id = c(1:36), 
                           year = rep(c(2020:2022), each= 12), 
                           month = rep(c(1:12), times= 3))

climate_per_lc <- variables_df %>% 
  dplyr::select(!(SOC_2020)) %>% 
  # change to long format so easier to plot
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
  mutate(month_year = paste0(month, "_", year))
warning('consider adding month word')

ggplot(climate_per_lc)+
  geom_line(aes(x = as.factor(month_id), y = mean_value, color = label, group = label), )+
  scale_colour_manual(
    values = landcover_classes_exist$hex )+
  facet_wrap(~variable, scales = "free")

warning('ADD an error range for sd maybe')

warning('Fix this whole section')
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

# 6. Sample size calculation ---------------------------------------------------
# calculate necessary sample size following
# n = ((z*sigma)/E)^2
# to detect changes in SOC if I want a 95% confidence interval equal or less 
# than 10% of the mean value, assuming a gaussian distribution.

# create a dataframe of SOC_pt to extract standard deviation of the 
# note that I am continuing to work with the low resolution file in 
# line with other layers analysed, but could also work with the high res 
SOC_df <- as.data.frame(SOC_pt)
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
warning('fix here that high resolution yields different resoult, in best case do both and use 
        different name throughout for low resolution, but then again
        difficult for stratified so maybe leave as is and just include a note')

# * Implement sampling in space ------------------------------------------------

# using the function spatSample and total number of samples necessary to 
# implement random sampling across Portugal
sample_points <- spatSample(SOC_pt, ceiling(nr_samples), method = "random", 
                            na.rm = T, as.points = T)
sf::st_as_sf(sample_points)
#visualise
ggplot() + 
  geom_spatraster(data = SOC_pt )  +
  geom_sf(data=sf::st_as_sf(sample_points), color="pink", size=1, alpha=1)

# * Prepare data for stratified sampling per landcover class in space ----------
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
         # so including this information
         possible = (nr_pixels > nr_samples & !is.na(nr_samples)),
         # change the number of samples for those that can't be sampled to zero
         nr_samples = ifelse(possible== FALSE, NA, nr_samples)) %>% 
  left_join(landcover_classes, by = "label") %>% 
  # we drop NA
  filter(label != "NA")

# we can only supply a vector with different sample sizes for landcover classes
# when using a polygon, so we polygonize landcover, but only using the categories
# I actually can sample
sampling_categories <- SOC_sampling_per_lc %>% 
  filter(possible == TRUE) %>% 
  pull(label)

#Define the categorical values for which you want to create polygons
landcover_polygons <- as.polygons(landcover_pt, aggregate = T, values = T )
# visualise polygon
ggplot() +
  geom_sf(data=sf::st_as_sf(landcover_polygons), aes(fill = label))+
  scale_fill_manual(values = landcover_classes_exist$hex, 
                    breaks=landcover_classes_exist$label)

# subset for the classes I can sample
landcover_polygons <- landcover_polygons[landcover_polygons$label %in% sampling_categories, ]
# visualise
ggplot() + 
  geom_sf(data=sf::st_as_sf(landcover_polygons), aes(fill = label))+
  scale_fill_manual(values = landcover_classes_exist$hex, 
                    breaks=landcover_classes_exist$label)

# extract sampling sizes
sample_sizes<- SOC_sampling_per_lc %>% 
  filter(possible == TRUE) %>% 
  pull(nr_samples)

# check if same number of polygons and sample sizes
length(sample_sizes) ==   length(landcover_polygons)

warning('work on quality starting here')

# * Create stratified sampling points and plot ---------------------------------

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


# end --------------------------------------------------------------------------