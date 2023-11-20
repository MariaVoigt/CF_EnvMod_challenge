
# Function to represent landcover classes  --------------------------------
here::i_am("src/functions/0_visualise_landcover_classes.R")

# landcover classes and hex codes extracted from metadata
landcover_categories <- function(landcover_raster, aktive_class){
  landcover_classes <- read.csv(here("data/raw/landcover/lc_classes.csv"),
                                stringsAsFactors = F, strip.white = T)
  
  levels(landcover_raster) <- landcover_classes
  # setting the respective column counting from 0) as active
  activeCat(landcover_raster, layer = 1) <- aktive_class - 1
  return(landcover_raster)
}

existing_landcover_classes <- function(landcover_raster, aktive_class){
  landcover_classes <- read.csv(here("data/raw/landcover/lc_classes.csv"),
                                stringsAsFactors = F, strip.white = T)
  levels(landcover_raster) <- landcover_classes
  activeCat(landcover_raster, layer = 1) <- aktive_class - 1
  # we need to adjust a dataframe to use for plotting
  lc_categories <- unique(landcover_raster)
  landcover_classes_exist <- lc_categories %>% 
    left_join(landcover_classes, by = "label")
  return(landcover_classes_exist)
}