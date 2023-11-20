#---------------------------------------------------------#
# Script No 3 -  Climate Farmers coding challenge         #
# Tasks:                                                  # 
# - prepare data for visualisation.                       #
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

here::i_am("src/analysis/3_Data_analysis_and_visualisation.R")

# 6. Analysis and visualization ------------------------------------------------

# * Prepare dataframe for analysis----------------------------------------------
#list with all variables for importing 
raster_files <- list.files(here("data/processed/raster_data/"), 
                           full.names = TRUE, pattern = ".tif$")

variable_names <- c("evapotransp_pt", "precipitation_pt", "temperature_pt", 
                       "landcover_pt_res", "SOC_pt_res")


for(variable_name in variable_names){
  assign(variable_name, rast(here("data/processed/raster_data/",
                           paste0(variable_name, ".tif"))))
}

# dataframe with all variables for aggregating per landcover class
variable_list <- list(evapotransp_pt, precipitation_pt, temperature_pt, 
                      landcover_pt_res, SOC_pt_res)
variables_stack <- rast(variable_list) 
variables_df <- as.data.frame(variables_stack )

# * Prepare landcover classes for analysis--------------------------------------
# load landcover classes
landcover_classes <- read.csv(here("data/raw/landcover/lc_classes.csv"),
                              stringsAsFactors = F, strip.white = T)
# adding the labels for the landcover classes here, so we have them later
variables_df <- variables_df %>% 
  left_join(landcover_classes, by = "label")

# We might not want to consider all the classes if they 
# only cover few pixels, so I will check this 
variables_df_summary <- variables_df %>% 
  group_by(label) %>% 
  summarise(pixel_count = n())


# lookup for colors for remaining classes
landcover_colors_all <- variables_df_summary %>% 
  left_join(landcover_classes)

# filtering classes with less than 1 % data, water or NA labels
variables_considered <- 
  variables_df_summary[variables_df_summary$pixel_count >10 &
                         variables_df_summary$label != "water" &
                         variables_df_summary$label != "NA" , ]

variables_df_filter <- filter(variables_df, (label %in% variables_considered$label))

# * Prepare visualisation of distribution of landcover classes ----------------

# lookup for colors for remaining classes
landcover_colors <- variables_considered %>% 
  left_join(landcover_classes)



# * Prepare visualise climate variables per landcover ----------------------------------

# month_id to year and month
month_lookup <- data.frame(month_id = c(1:36), 
                           year = rep(c(2020:2022), each= 12), 
                           month = rep(c(1:12), times= 3))

climate_per_lc <- variables_df_filter  %>% 
  dplyr::select(!(SOC)) %>% 
  # change to long format so easier to plot
  pivot_longer(cols = e_1:t2m_36, 
               names_to = c('variable','month_id'),
               names_sep = '_',
               values_to = 'value',
               names_transform = list(month_id= as.numeric)) %>% 
  group_by(label, variable, month_id) %>% 
  summarize(mean_value= mean(value, na.rm = T), # calculating mean
            stdev_value = sd(value, na.rm = T)) %>%  # calculating stdeviation
  # add years and months for better representation
  left_join(month_lookup, by = "month_id") %>% 
  mutate(date_obs = as.Date(paste(year, month, "15", sep = "-")))


# * Prepare visualisation of soil organic carbon per landcover -----------------

SOC_per_lc <- variables_df_filter  %>% 
  group_by(label) %>% 
  summarize(mean_SOC = mean(SOC, na.rm = T), 
            stdev_SOC = sd(SOC, na.rm = T), 
            nr_pixels = n()) %>% 
  left_join(landcover_classes, by = "label")

# end ---------------------------------------------------------------------
