#---------------------------------------------------------#
# Script No 5 -  Climate Farmers coding challenge         #
# Tasks:                                                  # 
# - format data for soil model                            #                           
# - implement soil model                                  #
# - plot soil model                                       #
#---------------------------------------------------------#

# load packages ---------------------------------------------------------
# basic
library(tidyverse)   # Collection of R packages designed for data science
library(here)        # Facilitates easy file path management within R projects
# spatial operations
library(terra)        # Efficient manipulation & analysis of raster data
library(tidyterra)    # Extends 'terra' for tidyverse-friendly raster operations
# soil model
library(SoilR)        # Specialized for soil data analysis and modeling

# set path
here::i_am("src/analysis/5_soil_model.R")

# Import data -------------------------------------------------------------
temperature_pt <- rast(here("data/processed/raster_data/temperature_pt.tif")) 
evapotransp_pt <- rast(here("data/processed/raster_data/evapotransp_pt.tif")) 
precipitation_pt <- rast(here("data/processed/raster_data/precipitation_pt.tif")) 
SOC_pt<- rast(here("data/processed/raster_data/SOC_pt_res.tif")) 

# Step 3: Parameterize the Model
# Define model parameters

# Define point to extract data from at a single point coordinate
model_point <- vect(matrix(c(lon = -7.74053, lat = 41.60447), ncol =2), 
                    crs="+proj=longlat +datum=WGS84")


# Set up data -------------------------------------------------------------

# format temperature, precipitation, and evaporation
point_temperature <- t(terra::extract(temperature_pt, model_point))[-1]
point_precipitation <-  t(terra::extract(precipitation_pt, model_point))[-1]
point_evapotransp <-  t(terra::extract(evapotransp_pt, model_point))[-1]

# Other soil-related values
soil_thick <- 20  # Soil thickness (organic layer topsoil), in cm
SOC <- terra::extract(SOC_pt, model_point)$SOC  # Soil organic carbon in Mg/ha 
clay <- 16.3  # Percent clay
Cinputs <- 2.7  # Annual C inputs to soil in Mg/ha/yr

# Create a vector of time steps for the simulation
years <- seq(1/12, 500, by = 1/12) 



# Calculate temperature and moisture effects on decomposition -------------
fT <- fT.RothC(point_temperature)  # Temperature effects per month
fW <- fW.RothC(P = point_precipitation, 
               E = point_evapotransp,
               S.Thick = soil_thick, 
               pClay = clay, 
               pE = 1.0, 
               bare = FALSE)$b  # Moisture effects per month
xi.frame <- data.frame(years, rep(fT * fW, length.out = length(years)))


# Calculate the size of the inert organic matter pool (IOM) ---------------

FallIOM <- 0.049 * SOC^(1.139)  # IOM using Falloon method


# Initialize the model and calculate C stocks for each pool ---------------
Model1 <- RothCModel(t = years, 
                     C0 = c(DPM = 0, RPM = 0, BIO = 0, HUM = 0, IOM = FallIOM), 
                     In = Cinputs, clay = clay, xi = xi.frame)
Ct1 <- getC(Model1)

# Extract data for plotting -----------------------------------------------
df_plot <- data.frame(years = years, Ct1)
df_plot_long <- df_plot %>%
  tidyr::gather(key = "Pool", value = "C_stocks", -years)

# Plot using ggplot2
ggplot(df_plot_long, aes(x = years, y = C_stocks, color = Pool)) +
  geom_line() +
  labs(x = "Time (years)", y = "C stocks (Mg/ha)", 
       title = "Carbon Stocks Over Time") +
  scale_color_manual(values = c("blue", "red", "green", "orange", "purple"),
                     labels = c("DPM", "RPM", "BIO", "HUM", "IOM")) +
  theme_minimal()
