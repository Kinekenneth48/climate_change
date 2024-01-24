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

terraOptions(memfrac = 0.8, verbose = TRUE)

# ============================================================================#
# load data
# ============================================================================#
max_swe_layers <- terra::rast("E:/data-raw/swe_model_vars/max_swe_layers.tif")
temp_diff_layers <- terra::rast("E:/data-raw/swe_model_vars/temp_diff_layers.tif")
mean_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/mean_temp_layers.tif")
total_ppt_layers <- terra::rast("E:/data-raw/swe_model_vars/total_ppt_layers.tif")
snow_window_layers <- terra::rast("E:/data-raw/swe_model_vars/snow_window_layers.tif")

storm_one_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_one_temp_layers.tif")
storm_two_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_two_temp_layers.tif")
storm_three_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_three_temp_layers.tif")
storm_one_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_one_layers.tif")
storm_two_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_two_layers.tif")
storm_three_layers <- terra::rast("E:/data-raw/swe_model_vars/storm_three_layers.tif")

ecoregions_L1 <- rep(terra::rast("data-raw/eco_regions/ecoregions_L1.tif"), 33)
ecoregions_L2 <- rep(terra::rast("data-raw/eco_regions/ecoregions_L2.tif"), 33)
ecoregions_L3 <- rep(terra::rast("data-raw/eco_regions/ecoregions_L3.tif"), 33)

elev_raster_layers <- terra::rast("E:/data-raw/swe_model_vars/elev_raster_layers.tif")
lon <- terra::rast("E:/data-raw/swe_model_vars/lon.tif")
lat <- terra::rast("E:/data-raw/swe_model_vars/lat.tif")

# ============================================================================#
# If ppt is NA ,set value as 0
# ============================================================================#
# total_ppt_layers <- terra::ifel(is.na(total_ppt_layers), 0, total_ppt_layers)
# snow_window_layers <- terra::ifel(is.na(snow_window_layers), 0, snow_window_layers)
# storm_one_layers <- terra::ifel(is.na(storm_one_layers), 0, storm_one_layers)
# storm_two_layers <- terra::ifel(is.na(storm_two_layers), 0, storm_two_layers)
# storm_three_layers <- terra::ifel(is.na(storm_three_layers), 0, storm_three_layers)



################################################################################
## STEP 1: PREPARE DATA FOR MODELING
################################################################################

# get train data
train_max_swe_layers <- max_swe_layers[[1:26]]
train_total_ppt_layers <- total_ppt_layers[[1:26]]
train_temp_diff_layers <- temp_diff_layers[[1:26]]
train_mean_temp_layers <- mean_temp_layers[[1:26]]
train_snow_window_layers <- snow_window_layers[[1:26]]

train_storm_one_temp_layers <- storm_one_temp_layers[[1:26]]
train_storm_two_temp_layers <- storm_two_temp_layers[[1:26]]
train_storm_three_temp_layers <- storm_three_temp_layers[[1:26]]
train_storm_one_layers <- storm_one_layers[[1:26]]
train_storm_two_layers <- storm_two_layers[[1:26]]
train_storm_three_layers <- storm_three_layers[[1:26]]

train_ecoregions_L1_layers <- ecoregions_L1[[1:26]]
train_ecoregions_L2_layers <- ecoregions_L2[[1:26]]
train_ecoregions_L3_layers <- ecoregions_L3[[1:26]]

train_elev_raster_layers <- elev_raster_layers[[1:26]]


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


elev_raster_df <- terra::as.data.frame(train_elev_raster_layers, na.rm = FALSE)

ecoregions_L1_df <- terra::as.data.frame(train_ecoregions_L1_layers, na.rm = FALSE)
ecoregions_L2_df <- terra::as.data.frame(train_ecoregions_L2_layers, na.rm = FALSE)
ecoregions_L3_df <- terra::as.data.frame(train_ecoregions_L3_layers, na.rm = FALSE)

# Creating a vector of new column names
swe_colnames <- c("lon", "lat", 1982:2007)
others_colnames <- c(1982:2007)

# ============================================================================#
# annual swe max
# ============================================================================#
# Renaming the columns
colnames(max_swe_df) <- swe_colnames

# Pivoting longer
model_vars_df <- max_swe_df %>%
  pivot_longer(
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
    cols = `1982`:`2007`,
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
model_vars_df_III_storms <- model_vars_df %>%
  filter(complete.cases(.))

# save data for use later
save(model_vars_df_III_storms,
  file = "E:/data-raw/swe_model_vars/model_vars_df_III_storms.RData"
)


# ============================================================================#
# test data
# ============================================================================#
test_max_swe_layers <- max_swe_layers[[27:33]]
test_total_ppt_layers <- total_ppt_layers[[27:33]]
test_temp_diff_layers <- temp_diff_layers[[27:33]]
test_mean_temp_layers <- mean_temp_layers[[27:33]]
test_snow_window_layers <- snow_window_layers[[27:33]]

test_storm_one_temp_layers <- storm_one_temp_layers[[27:33]]
test_storm_two_temp_layers <- storm_two_temp_layers[[27:33]]
test_storm_three_temp_layers <- storm_three_temp_layers[[27:33]]
test_storm_one_layers <- storm_one_layers[[27:33]]
test_storm_two_layers <- storm_two_layers[[27:33]]
test_storm_three_layers <- storm_three_layers[[27:33]]

test_ecoregions_L1_layers <- ecoregions_L1[[27:33]]
test_ecoregions_L2_layers <- ecoregions_L2[[27:33]]
test_ecoregions_L3_layers <- ecoregions_L3[[27:33]]

test_elev_raster_layers <- elev_raster_layers[[27:33]]


test_data_list <- list()

for (i in 1:nlyr(test_max_swe_layers)) {
  test_data_per_year <- c(
    lon, lat,
    test_total_ppt_layers[[i]], test_temp_diff_layers[[i]],
    test_mean_temp_layers[[i]], test_snow_window_layers[[i]],
    test_storm_one_temp_layers[[i]], test_storm_two_temp_layers[[i]],
    test_storm_three_temp_layers[[i]], test_storm_one_layers[[i]],
    test_storm_two_layers[[i]], test_storm_three_layers[[i]],
    test_ecoregions_L1_layers[[i]], test_ecoregions_L1_layers[[i]],
    test_ecoregions_L1_layers[[i]], test_elev_raster_layers[[i]]
  )

  names(test_data_per_year) <- c(
    "lon", "lat", "total_ppt", "temp_diff", "mean_temp", "snow_window",
    "storm_one_temp", "storm_two_temp",
    "storm_three_temp", "storm_one",
    "storm_two", "storm_three", "L1", "L2", "L3", "elev"
  )

  test_data_list[[i]] <- test_data_per_year
}

#save data for use later
save(test_data_list, file = "E:/data-raw/swe_model_vars/test_data_list.RData")

load("E:/data-raw/swe_model_vars/test_data_list.RData")
################################################################################
## STEP 2: CREATE SWE MODEL -GLOBAL MODEL
################################################################################

base::load("E:/data-raw/swe_model_vars/model_vars_df_III_storms.RData")

# Remove rows where 'swe' is GREATER THAN 10
model_vars_df_III_storms <- model_vars_df_III_storms
# filter(swe > 10)



# ============================================================================#
# Basic regression Model
# ============================================================================#
regression_model <- lm(
  swe ~ lon + lat + total_ppt + elev + mean_temp + temp_diff + storm_one_temp +
    storm_two_temp + storm_three_temp + storm_one + storm_two +
    storm_three + snow_window ,
  data = model_vars_df_III_storms
)

summary(regression_model)


regression_prediction_raster <- list()

# predict to a raster
for (i in 1:length(test_data_list)) {
  regression_prediction_raster[[i]] <- predict(test_data_list[[i]], regression_model)
}

regression_prediction_raster_II_storms <- rast(regression_prediction_raster)


writeRaster(regression_prediction_raster_II_storms,
  "E:/data-raw/swe_model_vars/regression_prediction_raster_II_storms.tif",
  overwrite = TRUE
)

plot(regression_prediction_raster_II_storms[[4]])



# ============================================================================#
# RF Model
# ============================================================================#

rf_storms <- ranger::ranger(
  swe ~ lon + lat + total_ppt + elev + mean_temp + temp_diff + storm_one_temp +
    storm_two_temp + storm_three_temp + storm_one + storm_two +
    storm_three + snow_window+L1+L2+L3,
  data = model_vars_df_III_storms, importance = "permutation",
  num.trees = 121, min.node.size = 20, num.threads = 12
)

rf_storms

save(rf_storms, file = "E:/data-raw/swe_model_vars/rf_storms.RData")

#load("E:/data-raw/swe_model_vars/test_data_list.RData")
#load("E:/data-raw/swe_model_vars/rf_storms.RData")

rf_storms_prediction_raster <- list()
# predict to a raster
for (i in 1:length(test_data_list)) {
  rf_storms_prediction_raster[[i]] <- predict(test_data_list[[i]], rf_storms, na.rm = TRUE)
}


rf_storms_prediction_raster_III <- rast(rf_storms_prediction_raster)

writeRaster(rf_storms_prediction_raster_III,
  "E:/data-raw/swe_model_vars/rf_storms_prediction_raster_III.tif",
  overwrite = TRUE
)



# ============================================================================#
# load data
# ============================================================================#
rf_storms_prediction_raster_III=rast("E:/data-raw/swe_model_vars/rf_storms_prediction_raster_II.tif")
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
