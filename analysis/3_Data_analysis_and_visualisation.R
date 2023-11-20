#---------------------------------------------------------#
# Second script for the Climate Farmers coding challenge  #
# Tasks:                                                  # 
# - resample land cover and soil data                     #
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

# filtering classes with less than 1 % data, water or NA labels
variables_considered <- 
  variables_df_summary[variables_df_summary$pixel_count >10 &
                         variables_df_summary$label != "water" &
                         variables_df_summary$label != "NA" , ]

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
