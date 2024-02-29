################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(terra)
library(tidyverse)
library(gbm)
library(caret)
library(elevatr)
library(datawizard)

terraOptions(memfrac = 0.8, verbose = TRUE)

# ============================================================================#
# load data
# ============================================================================#
max_swe_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_prism/max_swe_layers_prism.tif")
temp_diff_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_prism/temp_diff_layers_prism.tif")
mean_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_prism/mean_temp_layers_prism.tif")
total_ppt_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_prism/total_ppt_layers_prism.tif")
snow_window_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_prism/snow_window_layers_prism.tif")

storm_one_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_prism/storm_one_temp_layers_prism.tif")
storm_two_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_prism/storm_two_temp_layers_prism.tif")
storm_three_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_prism/storm_three_temp_layers_prism.tif")
storm_one_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_prism/storm_one_layers_prism.tif")
storm_two_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_prism/storm_two_layers_prism.tif")
storm_three_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_prism/storm_three_layers_prism.tif")

ecoregions_L1_prism <- rep(terra::rast("data-raw/eco_regions/ecoregions_L1_prism.tif"), 40)
ecoregions_L2_prism <- rep(terra::rast("data-raw/eco_regions/ecoregions_L2_prism.tif"), 40)
ecoregions_L3_prism <- rep(terra::rast("data-raw/eco_regions/ecoregions_L3_prism.tif"), 40)

elev_raster_layers_prism <- rep(terra::rast("E:/data-raw/swe_model_vars/elev_raster_layer_prism.tif"), 40)

lon <- terra::rast("E:/data-raw/swe_model_vars/lon_prism.tif")
lat <- terra::rast("E:/data-raw/swe_model_vars/lat_prism.tif")

# # ============================================================================#
# # get lon and lat
# # ============================================================================#
# lon_prism <- terra::init(max_swe_layers[[1]], 'x')
# lat_prism <- terra::init(max_swe_layers[[1]], 'y')
#
# terra::writeRaster(lon_prism,"E:/data-raw/swe_model_vars/lon_prism.tif",overwrite = TRUE)
# terra::writeRaster(lat_prism,"E:/data-raw/swe_model_vars/lat_prism.tif",overwrite = TRUE)


# # ============================================================================#
# # get elevation raster
# # ============================================================================#
# # download elevation raster
# elev_raster <- elevatr::get_elev_raster(locations = max_swe_layers[[1]], z = 5)
#
# # match elevation raster with max_swe_layers resolution
# elev_raster <- terra::rast(elev_raster)
# elev_raster <- terra::project(x = elev_raster, y = terra::crs(max_swe_layers))
#
#
# elev_raster_layer_prism <- terra::project(elev_raster, max_swe_layers[[1]],
#   method = "bilinear"
# )
#
#
# # save data
# terra::writeRaster(elev_raster_layer_prism,
#   "E:/data-raw/swe_model_vars/elev_raster_layer_prism.tif",
#   overwrite = TRUE
# )






################################################################################
## STEP 1: PREPARE DATA FOR MODELING
################################################################################

# get train data
train_max_swe_layers <- max_swe_layers[[1:32]]
train_total_ppt_layers <- total_ppt_layers[[1:32]]
train_temp_diff_layers <- temp_diff_layers[[1:32]]
train_mean_temp_layers <- mean_temp_layers[[1:32]]
train_snow_window_layers <- snow_window_layers[[1:32]]

train_storm_one_temp_layers <- storm_one_temp_layers[[1:32]]
train_storm_two_temp_layers <- storm_two_temp_layers[[1:32]]
train_storm_three_temp_layers <- storm_three_temp_layers[[1:32]]
train_storm_one_layers <- storm_one_layers[[1:32]]
train_storm_two_layers <- storm_two_layers[[1:32]]
train_storm_three_layers <- storm_three_layers[[1:32]]

train_ecoregions_L1_layers_prism <- ecoregions_L1_prism[[1:32]]
train_ecoregions_L2_layers_prism <- ecoregions_L2_prism[[1:32]]
train_ecoregions_L3_layers_prism <- ecoregions_L3_prism[[1:32]]

train_elev_raster_layers_prism <- elev_raster_layers_prism[[1:32]]


# cells that have a NA value in at least one layer are removed
max_swe_df <- terra::as.data.frame(train_max_swe_layers, xy = TRUE, na.rm = FALSE) # GET LON AND LAT
total_ppt_df <- terra::as.data.frame(train_total_ppt_layers, na.rm = FALSE)
mean_temp_df <- terra::as.data.frame(train_mean_temp_layers, na.rm = FALSE)
temp_diff_df <- terra::as.data.frame(train_temp_diff_layers, na.rm = FALSE)
snow_window_df <- terra::as.data.frame(train_snow_window_layers, na.rm = FALSE)

storm_one_temp_df <- terra::as.data.frame(train_storm_one_temp_layers, na.rm = FALSE)
storm_two_temp_df <- terra::as.data.frame(train_storm_two_temp_layers, na.rm = FALSE)
storm_three_temp_df <- terra::as.data.frame(train_storm_three_temp_layers, na.rm = FALSE)

storm_one_df <- terra::as.data.frame(train_storm_one_layers, na.rm = FALSE)
storm_two_df <- terra::as.data.frame(train_storm_two_layers, na.rm = FALSE)
storm_three_df <- terra::as.data.frame(train_storm_three_layers, na.rm = FALSE)


elev_raster_df <- terra::as.data.frame(train_elev_raster_layers_prism, na.rm = FALSE)

ecoregions_L1_df <- terra::as.data.frame(train_ecoregions_L1_layers_prism, na.rm = FALSE)
ecoregions_L2_df <- terra::as.data.frame(train_ecoregions_L2_layers_prism, na.rm = FALSE)
ecoregions_L3_df <- terra::as.data.frame(train_ecoregions_L3_layers_prism, na.rm = FALSE)

# Creating a vector of new column names
swe_colnames <- c("lon", "lat", 1982:2013)
others_colnames <- c(1982:2013)

# ============================================================================#
# annual swe max
# ============================================================================#
# Renaming the columns
colnames(max_swe_df) <- swe_colnames

# Pivoting longer
model_vars_df <- max_swe_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "swe"
  )

# ============================================================================#
# total ppt in winter monmths
# ============================================================================#
# Renaming the columns
colnames(total_ppt_df) <- others_colnames

# Pivoting longer
total_ppt_df <- total_ppt_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "total_ppt"
  ) %>%
  dplyr::select(total_ppt)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, total_ppt_df)


# ============================================================================#
# elevation
# ============================================================================#
# Renaming the columns
colnames(elev_raster_df) <- others_colnames

# Pivoting longer
elev_raster_df <- elev_raster_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "elev"
  ) %>%
  dplyr::select(elev)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, elev_raster_df)


# ============================================================================#
# mean temp in winter months
# ============================================================================#
# Renaming the columns
colnames(mean_temp_df) <- others_colnames

# Pivoting longer
mean_temp_df <- mean_temp_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "mean_temp"
  ) %>%
  dplyr::select(mean_temp)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, mean_temp_df)

# ============================================================================#
# 20 day moving window
# ============================================================================#
# Renaming the columns
colnames(snow_window_df) <- others_colnames

# Pivoting longer
snow_window_df <- snow_window_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "snow_window"
  ) %>%
  dplyr::select(snow_window)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, snow_window_df)



# ============================================================================#
# diff in temp in winter months
# ============================================================================#
# Renaming the columns
colnames(temp_diff_df) <- others_colnames

# Pivoting longer
temp_diff_df <- temp_diff_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "temp_diff"
  ) %>%
  dplyr::select(temp_diff)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, temp_diff_df)



# ============================================================================#
# storm one temp
# ============================================================================#
# Renaming the columns
colnames(storm_one_temp_df) <- others_colnames

# Pivoting longer
storm_one_temp_df <- storm_one_temp_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "storm_one_temp"
  ) %>%
  dplyr::select(storm_one_temp)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, storm_one_temp_df)



# ============================================================================#
# storm two temp
# ============================================================================#
# Renaming the columns
colnames(storm_two_temp_df) <- others_colnames

# Pivoting longer
storm_two_temp_df <- storm_two_temp_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "storm_two_temp"
  ) %>%
  dplyr::select(storm_two_temp)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, storm_two_temp_df)



# ============================================================================#
# storm three temp
# ============================================================================#
# Renaming the columns
colnames(storm_three_temp_df) <- others_colnames

# Pivoting longer
storm_three_temp_df <- storm_three_temp_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "storm_three_temp"
  ) %>%
  dplyr::select(storm_three_temp)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, storm_three_temp_df)



# ============================================================================#
# storm one
# ============================================================================#
# Renaming the columns
colnames(storm_one_df) <- others_colnames

# Pivoting longer
storm_one_df <- storm_one_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "storm_one"
  ) %>%
  dplyr::select(storm_one)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, storm_one_df)



# ============================================================================#
# storm two
# ============================================================================#
# Renaming the columns
colnames(storm_two_df) <- others_colnames

# Pivoting longer
storm_two_df <- storm_two_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "storm_two"
  ) %>%
  dplyr::select(storm_two)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, storm_two_df)



# ============================================================================#
# storm three
# ============================================================================#
# Renaming the columns
colnames(storm_three_df) <- others_colnames

# Pivoting longer
storm_three_df <- storm_three_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "storm_three"
  ) %>%
  dplyr::select(storm_three)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, storm_three_df)


# ============================================================================#
# ECOREGION L1
# ============================================================================#
# Renaming the columns
colnames(ecoregions_L1_df) <- others_colnames

# Pivoting longer
ecoregions_L1_df <- ecoregions_L1_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "L1"
  ) %>%
  dplyr::select(L1)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, ecoregions_L1_df)

# ========================================================================#
# ECOREGION L2
# ============================================================================#
# Renaming the columns
colnames(ecoregions_L2_df) <- others_colnames

# Pivoting longer
ecoregions_L2_df <- ecoregions_L2_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "L2"
  ) %>%
  dplyr::select(L2)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, ecoregions_L2_df)

# ========================================================================#
# ECOREGION L3
# ============================================================================#
# Renaming the columns
colnames(ecoregions_L3_df) <- others_colnames

# Pivoting longer
ecoregions_L3_df <- ecoregions_L3_df %>%
  pivot_longer(
    cols = `1982`:`2013`,
    names_to = "year",
    values_to = "L3"
  ) %>%
  dplyr::select(L3)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, ecoregions_L3_df)

# ============================================================================#
# ============================================================================#
# ============================================================================#

# Remove rows where any column has an NA value
model_vars_df_III_storms_prism <- model_vars_df %>%
  filter(complete.cases(.))

# save data for use later
save(model_vars_df_III_storms_prism,
  file = "E:/data-raw/swe_model_vars/storm_prism/model/train_data/model_vars_df_III_storms_prism.RData"
)


# ============================================================================#
# test data
# ============================================================================#
test_max_swe_layers <- max_swe_layers[[33:40]]
test_total_ppt_layers <- total_ppt_layers[[33:40]]
test_temp_diff_layers <- temp_diff_layers[[33:40]]
test_mean_temp_layers <- mean_temp_layers[[33:40]]
test_snow_window_layers <- snow_window_layers[[33:40]]

test_storm_one_temp_layers <- storm_one_temp_layers[[33:40]]
test_storm_two_temp_layers <- storm_two_temp_layers[[33:40]]
test_storm_three_temp_layers <- storm_three_temp_layers[[33:40]]
test_storm_one_layers <- storm_one_layers[[33:40]]
test_storm_two_layers <- storm_two_layers[[33:40]]
test_storm_three_layers <- storm_three_layers[[33:40]]

test_ecoregions_L1_layers <- ecoregions_L1_prism[[33:40]]
test_ecoregions_L2_layers <- ecoregions_L2_prism[[33:40]]
test_ecoregions_L3_layers <- ecoregions_L3_prism[[33:40]]

test_elev_raster_layers <- elev_raster_layers_prism[[33:40]]

# Create a sequence of years from 2014 to 2021
years <- time(test_max_swe_layers)

for (i in 1:nlyr(test_max_swe_layers)) {
  test_data_per_year <- c(
    lon, lat,
    test_total_ppt_layers[[i]], test_temp_diff_layers[[i]],
    test_mean_temp_layers[[i]], test_snow_window_layers[[i]],
    test_storm_one_temp_layers[[i]], test_storm_two_temp_layers[[i]],
    test_storm_three_temp_layers[[i]], test_storm_one_layers[[i]],
    test_storm_two_layers[[i]], test_storm_three_layers[[i]],
    test_ecoregions_L1_layers[[i]], test_ecoregions_L2_layers[[i]],
    test_ecoregions_L3_layers[[i]], test_elev_raster_layers[[i]]
  )
  
  names(test_data_per_year) <- c(
    "lon", "lat", "total_ppt", "temp_diff", "mean_temp", "snow_window",
    "storm_one_temp", "storm_two_temp",
    "storm_three_temp", "storm_one",
    "storm_two", "storm_three", "L1", "L2", "L3", "elev"
  )
  
  
  # Extract year from 2014 to 2021 based on loop iteration
  year <- years[i]
  
  # Construct file name for the output raster file
  file_name <- paste0("E:/data-raw/swe_model_vars/storm_prism/model/test_data/test_data_storm_prism_", year, ".tif")
  
  # Write the raster file
  writeRaster(test_data_per_year, file_name, overwrite = TRUE)
}


remove(
  test_max_swe_layers, test_total_ppt_layers,
  test_temp_diff_layers,
  test_mean_temp_layers,
  test_snow_window_layers,
  test_storm_one_temp_layers,
  test_storm_two_temp_layers,
  test_storm_three_temp_layers,
  test_storm_one_layers,
  test_storm_two_layers,
  test_storm_three_layers,
  test_ecoregions_L1_layers,
  test_ecoregions_L2_layers,
  test_ecoregions_L3_layers,
  test_elev_raster_layers
)


remove(
  train_max_swe_layers, train_total_ppt_layers,
  train_temp_diff_layers,
  train_mean_temp_layers,
  train_snow_window_layers,
  train_storm_one_temp_layers,
  train_storm_two_temp_layers,
  train_storm_three_temp_layers,
  train_storm_one_layers,
  train_storm_two_layers,
  train_storm_three_layers,
  train_ecoregions_L1_layers_prism,
  train_ecoregions_L2_layers_prism,
  train_ecoregions_L3_layers_prism,
  train_elev_raster_layers_prism
)

remove(
  max_swe_layers,
  temp_diff_layers,
  mean_temp_layers,
  total_ppt_layers,
  snow_window_layers,
  storm_one_temp_layers,
  storm_two_temp_layers,
  storm_three_temp_layers,
  storm_one_layers,
  storm_two_layers,
  storm_three_layers,
  ecoregions_L1_prism,
  ecoregions_L2_prism,
  ecoregions_L3_prism,
  elev_raster_layers_prism
)

remove(
  max_swe_df ,
  total_ppt_df,
  mean_temp_df ,
  temp_diff_df,
  snow_window_df,
  storm_one_temp_df ,
  storm_two_temp_df ,
  storm_three_temp_df ,
  storm_one_df ,
  storm_two_df ,
  storm_three_df,
  elev_raster_df ,
  ecoregions_L1_df ,
  ecoregions_L2_df ,
  ecoregions_L3_df , lat, lon, model_vars_df)

################################################################################
## STEP 2: CREATE SWE MODEL -GLOBAL MODEL
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(terra)
library(tidyverse)
library(gbm)
library(caret)
library(elevatr)
library(datawizard)

terraOptions(memfrac = 0.8, verbose = TRUE)

base::load("E:/data-raw/swe_model_vars/storm_prism/model/train_data/model_vars_df_III_storms_prism.RData")

# ============================================================================#
# pre-process data
# ============================================================================#
# Filter rows where swe is zero and storm_one is not zero, or storm_two is zero, 
# or storm_three is zero
model_vars_df_III_storms_prism <- model_vars_df_III_storms_prism[
  # Negate the condition to keep rows where swe is not zero or storm_one is zero
  !(model_vars_df_III_storms_prism$swe == 0 &
      (model_vars_df_III_storms_prism$storm_one != 0 |
         model_vars_df_III_storms_prism$storm_two != 0 |
         model_vars_df_III_storms_prism$storm_three != 0)
  ),
]

# # Remove rows where 'swe' is GREATER THAN 10
# model_vars_df_III_storms_prism <- model_vars_df_III_storms_prism
#  filter(swe > 10)


# ============================================================================#
# sample from the data
# ============================================================================#
#use less data for training
train_test = datawizard::data_partition(data = model_vars_df_III_storms_prism,
                           proportion = 0.5, seed = 234,
                           group = c("L1","L2","L3"))

train_data = train_test[["p_0.5"]]

# save data for use later
save(train_data,
     file = "E:/data-raw/swe_model_vars/storm_prism/model/train_data/train_data.RData"
)


load("E:/data-raw/swe_model_vars/storm_prism/model/train_data/train_data.RData")
# ============================================================================#
# Basic regression Model
# ============================================================================#
regression_model_storm_prism <- lm(
  swe ~ lon + lat + total_ppt + elev + mean_temp + temp_diff + storm_one_temp +
    storm_two_temp + storm_three_temp + storm_one + storm_two +
    storm_three + snow_window + L1 + L2 + L3,
  data = train_data
)

summary(regression_model_storm_prism)

# Save the regression model
save(regression_model_storm_prism,
     file = "E:/data-raw/swe_model_vars/storm_prism/model/regression_model_storm_prism.RData")


# ============================================================================#
# predict using regression Model
# ============================================================================#
# Load the saved regression model
load("E:/data-raw/swe_model_vars/storm_prism/model/regression_model.RData")


# Get names of TIFF files in the folder
tif_files <- list.files("E:/data-raw/swe_model_vars/storm_prism/model/test_data/",
                        pattern = "\\.tif$", full.names = TRUE)

years = 2014:2021

# predict to a raster
for (i in 1:length(tif_files)) {
  test = rast(tif_files[i])
  
  reg_pred_raster <- predict(test, regression_model, na.rm = TRUE)
  
  # Extract year from 2014 to 2021 based on loop iteration
  year <- years[i]
  
  # Construct file name for the output raster file
  file_name <- paste0("E:/data-raw/swe_model_vars/storm_prism/model/prediction/regression/reg_pred_raster_",
                      year, ".tif")
  
  # Write the raster file
  writeRaster(reg_pred_raster, file_name, overwrite = TRUE)
  gc()
  gc()
}




# ============================================================================#
# RF Model
# ============================================================================#

rf_storms_prism <- ranger::ranger(
  swe ~ lon + lat + total_ppt + elev + mean_temp + temp_diff + storm_one_temp +
    storm_two_temp + storm_three_temp + storm_one + storm_two +
    storm_three + snow_window + L1 + L2 + L3,
  data = train_data, importance = "permutation",
  num.trees = 121, min.node.size = 20, num.threads = 10
)



save(rf_storms_prism, file = "E:/data-raw/swe_model_vars/storm_prism/model/rf_storms_prism.RData")

load("E:/data-raw/swe_model_vars/storm_prism/model/rf_storms_prism.RData")

vip::vi_model(rf_storms_prism)

# Get names of TIFF files in the folder
tif_files <- list.files("E:/data-raw/swe_model_vars/storm_prism/model/test_data/",
                        pattern = "\\.tif$", full.names = TRUE)

years = 2014:2021

# predict to a raster
for (i in 1:length(tif_files)) {
  test = rast(tif_files[i])
  
  gc()
  rf_pred_raster <- predict(test, rf_storms_prism, na.rm = TRUE)
  
  # Extract year from 2014 to 2021 based on loop iteration
  year <- years[i]
  
  # Construct file name for the output raster file
  file_name <- paste0("E:/data-raw/swe_model_vars/storm_prism/model/prediction/rf/rf_pred_raster_",
                      year, ".tif")
  
  # Write the raster file
  writeRaster(rf_pred_raster, file_name, overwrite = TRUE)
  gc()
  gc()
}





# ============================================================================#
# load data
# ============================================================================#
rf_storms_prediction_raster_III <- rast("E:/data-raw/swe_model_vars/rf_storms_prediction_raster_II.tif")
ua_swe <- terra::rast("E:/data-raw/ua/raster/annual/max_swe_ua.tif")
ua_swe <- subset(ua_swe, time(ua_swe) %in% c(2008, 2009, 2010, 2011, 2012, 2013, 2014))


rf_prediction_raster_III_shift <- terra::shift(rf_storms_prediction_raster_III, dx = -360)
rf_prediction_raster_III_shift <- terra::project(
  rf_prediction_raster_III_shift,
  ua_swe[[1]]
)

prism <- terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]
prism_mask <- project(prism, ua_swe[[1]])

# ============================================================================#
# Take the difference
# ============================================================================#
error_layers <- ua_swe - rf_prediction_raster_III_shift



# ============================================================================#
# Mask the error layers for US
# ============================================================================#
cropped_error_layers <- mask(x = error_layers, mask = prism_mask)

plot(cropped_error_layers[[1]], breaks = c(
  -500, -400, -300, -200, -100, -80, -60,
  -40, -20, 0, 20, 40, 60, 80, 100, 200, 300, 400, 500
))

# Specify the file name for the PDF
pdf("plots_rf_storms.pdf", width = 8, height = 6) # Adjust the overall PDF dimensions as needed

# Loop through your data and create and save the smaller plots
for (i in 1:7) {
  # Create a smaller plot for cropped_error_layers[[i]]
  par(mar = c(5, 4, 4, 2)) # Adjust margin as needed (top, right, bottom, left)
  plot(cropped_error_layers[[i]],
    breaks = c(
      -500, -400, -300, -200, -100, -80, -60,
      -40, -20, 0, 20, 40, 60, 80, 100, 200, 300, 400, 500
    ),
    width = 2, height = 2
  ) # Adjust width and height as needed
}

# Close the PDF file
dev.off()

# ============================================================================#
# RMSE MAE
# ============================================================================#
rmse <- vector("numeric", length = 0)
mae <- vector("numeric", length = 0)

for (i in 1:nlyr(cropped_error_layers)) {
  rmse[i] <- sqrt(mean((values(cropped_error_layers[[i]]))^2, na.rm = TRUE))
  mae[i] <- mean(abs(values(cropped_error_layers[[i]])), na.rm = TRUE)
}

mae <- mean(mae)
rmse <- mean(rmse)
mae
rmse

################################################################################
## STEP 2: linear interpolation
################################################################################
