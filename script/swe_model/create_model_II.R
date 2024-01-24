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

terraOptions(memfrac = 0.7, verbose = TRUE)

# ============================================================================#
# load data
# ============================================================================#
total_ppt_layers <- terra::rast("E:/data-raw/swe_model_vars/total_ppt_layers.tif")
mean_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/mean_temp_layers.tif")
max_swe_layers <- terra::rast("E:/data-raw/swe_model_vars/max_swe_layers.tif")
freezing_days_layers <- terra::rast("E:/data-raw/swe_model_vars/freezing_days_layers.tif")
snowfall_days_layers <- terra::rast("E:/data-raw/swe_model_vars/snowfall_days_layers.tif")
max_con_freeze_days_layers <- terra::rast("E:/data-raw/swe_model_vars/max_con_freeze_days_layers.tif")
melt_degree_days_layers <- terra::rast("E:/data-raw/swe_model_vars/melt_degree_days_layers.tif")
temp_diff_layers <- terra::rast("E:/data-raw/swe_model_vars/temp_diff_layers.tif")
elev_raster_layers <- terra::rast("E:/data-raw/swe_model_vars/elev_raster_layers.tif")
single_max_pr_layers <- terra::rast("E:/data-raw/swe_model_vars/max_pr_layers.tif")
lon <- terra::rast("E:/data-raw/swe_model_vars/lon.tif")
lat <- terra::rast("E:/data-raw/swe_model_vars/lat.tif")



################################################################################
## STEP 1: PREPARE DATA FOR MODELING
################################################################################

# CONVERT FROM KG M2 S TO MMper day
total_ppt_layers <- total_ppt_layers * 86400
single_max_pr_layers <- single_max_pr_layers * 86400


# get train data
train_max_swe_layers <- max_swe_layers[[1:26]]
train_total_ppt_layers <- total_ppt_layers[[1:26]]
train_mean_temp_layers <- mean_temp_layers[[1:26]]
train_freezing_days_layers <- freezing_days_layers[[1:26]]
train_snowfall_days_layers <- snowfall_days_layers[[1:26]]
train_max_con_freeze_days_layers <- max_con_freeze_days_layers[[1:26]]
train_melt_degree_days_layers <- melt_degree_days_layers[[1:26]]
train_temp_diff_layers <- temp_diff_layers[[1:26]]
train_elev_raster_layers <- elev_raster_layers[[1:26]]
train_single_max_pr_layers <- single_max_pr_layers[[1:26]]


# cells that have a NA value in at least one layer are removed
max_swe_df <- terra::as.data.frame(train_max_swe_layers, xy = TRUE, na.rm = FALSE) # GET LON AND LAT
total_ppt_df <- terra::as.data.frame(train_total_ppt_layers, na.rm = FALSE)
mean_temp_df <- terra::as.data.frame(train_mean_temp_layers, na.rm = FALSE)
freezing_days_df <- terra::as.data.frame(train_freezing_days_layers, na.rm = FALSE)
snowfall_days_df <- terra::as.data.frame(train_snowfall_days_layers, na.rm = FALSE)
max_con_freeze_df <- terra::as.data.frame(train_max_con_freeze_days_layers, na.rm = FALSE)
melt_degree_df <- terra::as.data.frame(train_melt_degree_days_layers, na.rm = FALSE)
temp_diff_df <- terra::as.data.frame(train_temp_diff_layers, na.rm = FALSE)
elev_raster_df <- terra::as.data.frame(train_elev_raster_layers, na.rm = FALSE)
single_max_pr_df <- terra::as.data.frame(train_single_max_pr_layers, na.rm = FALSE)

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
# number of freezing days in winter months
# ============================================================================#
# Renaming the columns
colnames(freezing_days_df) <- others_colnames

# Pivoting longer
freezing_days_df <- freezing_days_df %>%
  pivot_longer(
    cols = `1982`:`2007`,
    names_to = "year",
    values_to = "freezing_days"
  ) %>%
  dplyr::select(freezing_days)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, freezing_days_df)


# ============================================================================#
# number of snowfall days in winter months
# ============================================================================#
# Renaming the columns
colnames(snowfall_days_df) <- others_colnames

# Pivoting longer
snowfall_days_df <- snowfall_days_df %>%
  pivot_longer(
    cols = `1982`:`2007`,
    names_to = "year",
    values_to = "snowfall_days"
  ) %>%
  dplyr::select(snowfall_days)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, snowfall_days_df)


# ============================================================================#
# max number of consecutive freeze days
# ============================================================================#
# Renaming the columns
colnames(max_con_freeze_df) <- others_colnames

# Pivoting longer
max_con_freeze_df <- max_con_freeze_df %>%
  pivot_longer(
    cols = `1982`:`2007`,
    names_to = "year",
    values_to = "max_con_freeze"
  ) %>%
  dplyr::select(max_con_freeze)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, max_con_freeze_df)

# ============================================================================#
# melt degree days
# ============================================================================#
# Renaming the columns
colnames(melt_degree_df) <- others_colnames

# Pivoting longer
melt_degree_df <- melt_degree_df %>%
  pivot_longer(
    cols = `1982`:`2007`,
    names_to = "year",
    values_to = "melt_degree"
  ) %>%
  dplyr::select(melt_degree)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, melt_degree_df)


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
# diff in temp in winter months
# ============================================================================#
# Renaming the columns
colnames(single_max_pr_df) <- others_colnames

# Pivoting longer
single_max_pr_df <- single_max_pr_df %>%
  pivot_longer(
    cols = `1982`:`2007`,
    names_to = "year",
    values_to = "single_max_pr"
  ) %>%
  dplyr::select(single_max_pr)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, single_max_pr_df)


# ============================================================================#
# ============================================================================#
# ============================================================================#

# Remove rows where any column has an NA value
model_vars_df_II <- model_vars_df %>%
  filter(complete.cases(.))

# save data for use later
save(model_vars_df_II, file = "E:/data-raw/swe_model_vars/model_vars_df_II.RData")


#
# get test and train data
test_max_swe_layers <- max_swe_layers[[27:33]]
test_total_ppt_layers <- total_ppt_layers[[27:33]]
test_mean_temp_layers <- mean_temp_layers[[27:33]]
test_freezing_days_layers <- freezing_days_layers[[27:33]]
test_snowfall_days_layers <- snowfall_days_layers[[27:33]]
test_max_con_freeze_days_layers <- max_con_freeze_days_layers[[27:33]]
test_melt_degree_days_layers <- melt_degree_days_layers[[27:33]]
test_temp_diff_layers <- temp_diff_layers[[27:33]]
test_elev_raster_layers <- elev_raster_layers[[27:33]]
test_single_max_pr_layers <- single_max_pr_layers[[27:33]]

test_data_list <- list()

for (i in 1:nlyr(test_max_swe_layers)) {
  test_data_per_year <- c(
    lon, lat,
    test_elev_raster_layers[[i]], test_total_ppt_layers[[i]],
    test_mean_temp_layers[[i]], test_freezing_days_layers[[i]],
    test_snowfall_days_layers[[i]], test_max_con_freeze_days_layers[[i]],
    test_melt_degree_days_layers[[i]], test_temp_diff_layers[[i]],
    test_single_max_pr_layers[[i]]
  )

  names(test_data_per_year) <- c(
    "lon", "lat", "elev", "total_ppt", "mean_temp",
    "freezing_days", "snowfall_days",
    "max_con_freeze", "melt_degree",
    "temp_diff", "single_max_pr"
  )

  test_data_list[[i]] <- test_data_per_year
}

################################################################################
## STEP 2: CREATE SWE MODEL -GLOBAL MODEL
################################################################################

base::load("E:/data-raw/swe_model_vars/model_vars_df_II.RData")

# Remove rows where 'swe' is GREATER THAN 10
model_vars_df_II <- model_vars_df_II %>%
  # filter(swe > 10)



  # ============================================================================#
  # Basic regression Model
  # ============================================================================#
  regression_model() <- lm(
  swe ~ lon + lat + elev + total_ppt + mean_temp + freezing_days +
    snowfall_days + max_con_freeze + melt_degree + temp_diff + single_max_pr,
  data = model_vars_df_II
)

summary(regression_model)


regression_prediction_raster <- list()

# predict to a raster
for (i in 1:length(test_data_list)) {
  regression_prediction_raster[[i]] <- predict(test_data_list[[i]], regression_model)
}

regression_prediction_raster_II <- rast(regression_prediction_raster)


writeRaster(regression_prediction_raster_II,
  "E:/data-raw/swe_model_vars/regression_prediction_raster_II.tif",
  overwrite = TRUE
)

plot(regression_prediction_raster_II[[4]])


#
# predictions_regress <- predict(regression_model, newdata = testData)
# actuals <- testData$swe
#
# rmse_regress <- sqrt(mean((predictions_regress - actuals)^2))
# rmse_regress

# ============================================================================#
# Basic GBM Model
# ============================================================================#

basic_gbm <- gbm(
  swe ~ lon + lat + elev + total_ppt + mean_temp + freezing_days +
    snowfall_days + max_con_freeze + melt_degree + temp_diff + single_max_pr,
  data = trainData,
  distribution = "gaussian",
  n.trees = 100,
  interaction.depth = 3,
  shrinkage = 0.01,
  bag.fraction = 0.5
)

summary(basic_gbm)

predictions_gbm <- predict(basic_gbm, newdata = testData, n.trees = 100)
actuals <- testData$swe

rmse_gbm <- sqrt(mean((predictions_gbm - actuals)^2))
rmse_gbm



# ============================================================================#
# RF Model
# ============================================================================#

rf <- ranger::ranger(
  swe ~ lon + lat + elev + total_ppt + mean_temp + freezing_days +
    snowfall_days + max_con_freeze + melt_degree + temp_diff + single_max_pr,
  data = model_vars_df_II, importance = "permutation",
  num.trees = 200, min.node.size = 10, num.threads = 7
)

rf

save(rf,file="E:/data-raw/swe_model_vars/rf.RData")



rf_prediction_raster <- list()
# predict to a raster
for (i in 1:length(test_data_list)) {
  rf_prediction_raster[[i]] <- predict(test_data_list[[i]], rf, na.rm = TRUE)
}

rf_prediction_raster_II <- rast(rf_prediction_raster)

writeRaster(rf_prediction_raster_II,
  "E:/data-raw/swe_model_vars/rf_prediction_raster_II.tif",
  overwrite = TRUE
)

plot(rf_prediction_raster[[4]],
  breaks = c(
    -500, -400, -300, -200, -100, -80, -60,
    -40, -20, 0, 20, 40, 60, 80, 100, 200, 300, 400, 500
  )
)

# ============================================================================#
# load data
# ============================================================================#
ua_swe <- terra::rast("E:/data-raw/ua/raster/annual/max_swe_ua.tif")
ua_swe= subset(ua_swe, time(ua_swe) %in% c(2008, 2009, 2010,2011, 2012, 2013, 2014))


rf_prediction_raster_II_shift = terra::shift(rf_prediction_raster_II, dx=-360)
rf_prediction_raster_II_shift = terra::project(rf_prediction_raster_II_shift,
                                        ua_swe[[1]] )

prism = terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]
prism_mask = project(prism, ua_swe[[1]])

# ============================================================================#
# Take the difference
# ============================================================================#
error_layers <- ua_swe - rf_prediction_raster_II_shift



# ============================================================================#
# Mask the error layers for US
# ============================================================================#
cropped_error_layers = mask(x=error_layers, mask=prism_mask)



# Specify the file name for the PDF
pdf("plots_rf.pdf", width = 8, height = 6)  # Adjust the overall PDF dimensions as needed

# Loop through your data and create and save the smaller plots
for (i in 1:7) {
  # Create a smaller plot for cropped_error_layers[[i]]
  par(mar = c(5, 4, 4, 2))  # Adjust margin as needed (top, right, bottom, left)
  plot(cropped_error_layers[[i]], breaks = c(-500,-400,-300,-200,-100, -80, -60, 
                                             -40, -20, 0, 20, 40, 60, 80,100,200,300,400, 500), 
       width = 2, height = 2)  # Adjust width and height as needed
  
}

# Close the PDF file
dev.off()

# ============================================================================#
# RMSE MAE
# ============================================================================#
rmse = vector("numeric", length = 0)
mae = vector("numeric", length = 0)

for (i in 1:nlyr(cropped_error_layers)) {
  rmse[i] = sqrt(mean((values(cropped_error_layers[[i]]))^2, na.rm = TRUE))
  mae[i] = mean(abs(values(cropped_error_layers[[i]])), na.rm = TRUE)
}

mae = mean(mae)
rmse= mean(rmse)

# ============================================================================#
# Tuning GBM Model
# ============================================================================#
set.seed(123)
tuneGrid <- expand.grid(
  interaction.depth = seq(1, 10, by = 2), # depth of trees
  n.trees = (1:5) * 100, # number of trees
  shrinkage = c(0.01, 0.05, 0.1, 0.2), # learning rate
  n.minobsinnode = c(5, 10, 15, 20)
)


fitControl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

set.seed(123)
tuned_gbm <- train(
  swe ~ lon + lat + elev + max_pr + mean_pr + sum_pr + corresp_temp +
    mean_temp + snow_year_day + ground,
  data = trainData,
  method = "gbm",
  trControl = fitControl,
  tuneGrid = tuneGrid,
  verbose = FALSE,
  metric = "RMSE"
)

# Display tuning results
print(tuned_gbm)




################################################################################
## STEP 2: CREATE SWE MODEL - LOCALIZED MODEL
################################################################################

# ============================================================================#
# Average Model
# ============================================================================#
# load data
max_swe_layers <- terra::rast("E:data-raw/swe_model_vars/max_swe_layers.tif")

# get test and train data
train_layers <- max_swe_layers[[1:26]]
test_layers <- max_swe_layers[[27:33]]

# Compute the average across all layers
average_raster <- mean(train_layers, na.rm = TRUE)

# make average predictions
prediction_layers <- c(average_raster, rep(average_raster, 6))

# compute error
error_layers <- prediction_layers - test_layers

# error_layers <- ((prediction_layers - test_layers)/clamp(test_layers, lower=0.001))*100

# Calculate single RMSE value
mean_squared_errors <- mean((values(error_layers))^2, na.rm = TRUE)
rmse_avg_model <- sqrt(mean_squared_errors)
rmse_avg_model

# ============================================================================#
# RMSE MAE
# ============================================================================#
rmse = vector("numeric", length = 0)
mae = vector("numeric", length = 0)

for (i in 1:nlyr(error_layers)) {
  rmse[i] = sqrt(mean((values(error_layers[[i]]))^2, na.rm = TRUE))
  mae[i] = mean(abs(values(error_layers[[i]])), na.rm = TRUE)
}

mae = mean(mae)
rmse= mean(rmse)

plot(error_layers[[3]], breaks=c(-500,-80,-60,-40,-20,0,20, 40,60,80,500))


# ============================================================================#
# cellwise regression Model
# ============================================================================#
max_swe_layers <- terra::rast("E:data-raw/swe_model_vars/max_swe_layers.tif")
total_ppt_layers <- terra::rast("E:data-raw/swe_model_vars/total_ppt_layers.tif")
mean_temp_layers <- terra::rast("E:data-raw/swe_model_vars/mean_temp_layers.tif")


# max_swe_layers <- terra::rast("data-raw/swe_model_vars/max_swe_layers.tif")
# freezing_days_layers <- terra::rast("data-raw/swe_model_vars/freezing_days_layers.tif")
# snowfall_days_layers <- terra::rast("data-raw/swe_model_vars/snowfall_days_layers.tif")
# max_con_freeze_days_layers <- terra::rast("data-raw/swe_model_vars/max_con_freeze_days_layers.tif")
# melt_degree_days_layers <- terra::rast("data-raw/swe_model_vars/melt_degree_days_layers.tif")
# temp_diff_layers <- terra::rast("data-raw/swe_model_vars/temp_diff_layers.tif")
# elev_raster_layers <- terra::rast("data-raw/swe_model_vars/elev_raster_layers.tif")
# single_max_pr_layers <- terra::rast("data-raw/swe_model_vars/max_pr_layers.tif")


# get 26 years of train data
train_max_swe_layers <- max_swe_layers[[1:26]]
train_total_ppt_layers <- total_ppt_layers[[1:26]]
train_mean_temp_layers <- mean_temp_layers[[1:26]]

# combine train data into a multi-layer single raster
train_raster <- c(
  train_max_swe_layers, train_total_ppt_layers,
  train_mean_temp_layers
)



# get 6 years of test data
test_max_swe_layers <- max_swe_layers[[27:33]]
test_total_ppt_layers <- total_ppt_layers[[27:33]]
test_mean_temp_layers <- mean_temp_layers[[27:33]]



# combine train data into a multi-layer single raster
test_raster <- c(
  test_total_ppt_layers, test_mean_temp_layers
)



source("R/cellwise_lm_sol.R")
source("R/cellwise_matrix_sol.R")


raster <- c(train_max_swe_layers, train_total_ppt_layers, train_mean_temp_layers)


single_regress <- terra::regress(y = train_max_swe_layers, train_total_ppt_layers)

tictoc::tic() # 7mins
multi_regress <- terra::app(raster, cellwise_lm_sol, variables = 2)
tictoc::toc()



prediction_1 <- sum(single_regress[[1]], single_regress[[2]] * test_total_ppt_layers, na.rm = TRUE)


prediction_2 <- sum(multi_regress[[1]], multi_regress[[2]] * test_total_ppt_layers,
  multi_regress[[3]] * test_mean_temp_layers,
  na.rm = TRUE
)



single_regress_error <- prediction_1 - test_max_swe_layers
multi_regress_error <- prediction_2 - test_max_swe_layers

# error_layers <- ((prediction_1 - test_max_swe_layers)/clamp(test_max_swe_layers, lower=0.001))*100

rmse_1 <- sqrt(mean((values(single_regress_error))^2, na.rm = TRUE))
rmse_2 <- sqrt(mean((values(multi_regress_error))^2, na.rm = TRUE))
rmse_1
rmse_2
