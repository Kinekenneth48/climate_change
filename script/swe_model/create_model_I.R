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


# ============================================================================#
# load data
# ============================================================================#
max_swe_layers <- terra::rast("E:/data-raw/swe_model_vars/max_swe_layers.tif")
max_pr_layers <- terra::rast("E:/data-raw/swe_model_vars/max_pr_layers.tif")
mean_pr_layers <- terra::rast("E:/data-raw/swe_model_vars/mean_pr_layers.tif")
sum_pr_layers <- terra::rast("E:/data-raw/swe_model_vars/sum_pr_layers.tif")
corresp_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/corresp_temp_layers.tif")
mean_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/mean_temp_layers.tif")
max_pr_position_layers <- terra::rast("E:/data-raw/swe_model_vars/max_pr_position_layers.tif")
days_on_ground_layers <- terra::rast("E:/data-raw/swe_model_vars/days_on_ground_layers.tif")
lon <- terra::rast("E:/data-raw/swe_model_vars/lon.tif")
lat <- terra::rast("E:/data-raw/swe_model_vars/lat.tif")
elev_raster_layers <- terra::rast("E:/data-raw/swe_model_vars/elev_raster_layers.tif")

# ============================================================================#
# get elevation raster
# ============================================================================#
# download elevation raster
ua_raster <- terra::shift(max_swe_layers[[1]], dx = -360)
elev_raster <- elevatr::get_elev_raster(locations = ua_raster, z = 4)

# match elevation raster with max_swe_layers resolution
elev_raster <- terra::rast(elev_raster)
elev_raster <- terra::project(x = elev_raster, y = terra::crs(max_swe_layers))
elev_raster <- terra::shift(elev_raster, dx = 360)

elev_raster_layer <- terra::project(elev_raster, max_swe_layers[[1]],
  method = "bilinear"
)

# Replicate the raster 33 times and combine them into a multi-layer raster
elev_raster_layers <- c(elev_raster_layer, rep(elev_raster_layer, 32))


# save data
terra::writeRaster(elev_raster_layers,
  "E:/data-raw/swe_model_vars/elev_raster_layers.tif",
  overwrite = TRUE
)

elev_raster_layers <- terra::rast("E:/data-raw/swe_model_vars/elev_raster_layers.tif")

################################################################################
## STEP 1: PREPARE DATA FOR MODELING
################################################################################
#CONVERT FROM KG M2 S TO MMper day
max_pr_layers = max_pr_layers*86400
mean_pr_layers = mean_pr_layers*86400
sum_pr_layers = sum_pr_layers*86400

# get test and train data
train_max_swe_layers <- max_swe_layers[[1:26]]
train_max_pr_layers <- max_pr_layers[[1:26]]
train_mean_pr_layers <- mean_pr_layers[[1:26]]
train_sum_pr_layers <- sum_pr_layers[[1:26]]
train_corresp_temp_layers <- corresp_temp_layers[[1:26]]
train_mean_temp_layers <- mean_temp_layers[[1:26]]
train_max_pr_position_layers <- max_pr_position_layers[[1:26]]
train_days_on_ground_layers <- days_on_ground_layers[[1:26]]
train_elev_raster_layers <- elev_raster_layers[[1:26]]

max_swe_df <- terra::as.data.frame(train_max_swe_layers, xy = TRUE, na.rm = FALSE)
max_pr_df <- terra::as.data.frame(train_max_pr_layers, na.rm = FALSE)
mean_pr_df <- terra::as.data.frame(train_mean_pr_layers, na.rm = FALSE)
sum_pr_df <- terra::as.data.frame(train_sum_pr_layers, na.rm = FALSE)
corresp_temp_df <- terra::as.data.frame(train_corresp_temp_layers, na.rm = FALSE)
mean_temp_df <- terra::as.data.frame(train_mean_temp_layers, na.rm = FALSE)
max_pr_position_df <- terra::as.data.frame(train_max_pr_position_layers, na.rm = FALSE)
days_on_ground_df <- terra::as.data.frame(train_days_on_ground_layers, na.rm = FALSE)
elev_raster_df <- terra::as.data.frame(train_elev_raster_layers, na.rm = FALSE)

# Creating a vector of new column names
swe_colnames <- c("lon", "lat", 1982:2007)
others_colnames <- c(1982:2007)

#test_layers <- max_swe_layers[[27:33]]
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
# annual pr max
# ============================================================================#
# Renaming the columns
colnames(max_pr_df) <- others_colnames

# Pivoting longer
max_pr_df <- max_pr_df %>%
  pivot_longer(
    cols = `1982`:`2007`,
    names_to = "year",
    values_to = "max_pr"
  ) %>%
  dplyr::select(max_pr)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, max_pr_df)


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
# mean pr from accumulation of pr to max pr
# ============================================================================#
# Renaming the columns
colnames(mean_pr_df) <- others_colnames

# Pivoting longer
mean_pr_df <- mean_pr_df %>%
  pivot_longer(
    cols = `1982`:`2007`,
    names_to = "year",
    values_to = "mean_pr"
  ) %>%
  dplyr::select(mean_pr)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, mean_pr_df)


# ============================================================================#
# sum pr from accumulation of pr to max pr
# ============================================================================#
# Renaming the columns
colnames(sum_pr_df) <- others_colnames

# Pivoting longer
sum_pr_df <- sum_pr_df %>%
  pivot_longer(
    cols = `1982`:`2007`,
    names_to = "year",
    values_to = "sum_pr"
  ) %>%
  dplyr::select(sum_pr)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, sum_pr_df)


# ============================================================================#
# corresponding temp at max pr
# ============================================================================#
# Renaming the columns
colnames(corresp_temp_df) <- others_colnames

# Pivoting longer
corresp_temp_df <- corresp_temp_df %>%
  pivot_longer(
    cols = `1982`:`2007`,
    names_to = "year",
    values_to = "corresp_temp"
  ) %>%
  dplyr::select(corresp_temp)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, corresp_temp_df)


# ============================================================================#
# mean temp from accumulation of pr to max pr
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
# day in the snow year that the max pr(assumed as snow) occurred
# ============================================================================#
# Renaming the columns
colnames(max_pr_position_df) <- others_colnames

# Pivoting longer
max_pr_position_df <- max_pr_position_df %>%
  pivot_longer(
    cols = `1982`:`2007`,
    names_to = "year",
    values_to = "snow_year_day"
  ) %>%
  dplyr::select(snow_year_day)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, max_pr_position_df)


# ============================================================================#
# number of days the max pr(assumed as snow) stayed on ground
# ============================================================================#
# Renaming the columns
colnames(days_on_ground_df) <- others_colnames

# Pivoting longer
days_on_ground_df <- days_on_ground_df %>%
  pivot_longer(
    cols = `1982`:`2007`,
    names_to = "year",
    values_to = "ground"
  ) %>%
  dplyr::select(ground)

# add max pr to combined data frame
model_vars_df <- cbind(model_vars_df, days_on_ground_df)


# ============================================================================#
# ============================================================================#
# ============================================================================#

# Remove rows where any column has an NA value
model_vars_df <- model_vars_df %>%
  filter(complete.cases(.))

# save data for use later
save(model_vars_df, file = "E:/data-raw/swe_model_vars/model_vars_df.RData")


#
# get test and train data
test_max_swe_layers <- max_swe_layers[[27:33]]
test_max_pr_layers <- max_pr_layers[[27:33]]
test_mean_pr_layers <- mean_pr_layers[[27:33]]
test_sum_pr_layers <- sum_pr_layers[[27:33]]
test_corresp_temp_layers <- corresp_temp_layers[[27:33]]
test_mean_temp_layers <- mean_temp_layers[[27:33]]
test_max_pr_position_layers <- max_pr_position_layers[[27:33]]
test_days_on_ground_layers <- days_on_ground_layers[[27:33]]
test_elev_raster_layers <- elev_raster_layers[[27:33]]

test_data_list =list()

for (i in 1:nlyr(test_max_swe_layers)){
  test_data_per_year = c(lon,lat,
       test_elev_raster_layers[[i]], test_max_pr_layers[[i]],
       test_mean_pr_layers[[i]],test_sum_pr_layers[[i]],
       test_corresp_temp_layers[[i]],test_mean_temp_layers[[i]],
       test_max_pr_position_layers[[i]],test_days_on_ground_layers[[i]])
  
  names(test_data_per_year) <- c("lon", "lat", "elev","max_pr","mean_pr" ,"sum_pr" , 
                  "corresp_temp" , "mean_temp" , "snow_year_day" , "ground")
  test_data_list[[i]]=test_data_per_year
  
}


################################################################################
## STEP 2: CREATE SWE MODEL -GLOBAL MODEL
################################################################################

base::load("E:/data-raw/swe_model_vars/model_vars_df.RData")

# Remove rows where 'swe' is equal to zero
model_vars_df <- model_vars_df %>%
  filter(swe > 10)


# ============================================================================#
# Basic regression Model
# ============================================================================#
regression_model <- lm(
  swe ~ lon + lat + elev + max_pr + mean_pr + sum_pr +
    corresp_temp +
    mean_temp + snow_year_day + ground,
  data = model_vars_df
)

summary(regression_model)

regression_prediction_raster = list()
#predict to a raster
for (i in 1:length(test_data_list)){
  regression_prediction_raster[[i]] <- predict(test_data_list[[i]], regression_model)
}

regression_prediction_raster_II = rast(regression_prediction_raster)

save(regression_prediction_raster_II, file = "E:/data-raw/swe_model_vars/regression_prediction_raster_II.tif")


plot(regression_prediction_raster[[4]], 
     breaks=c(-500,-400,-300,-200,-100, -80, -60, 
           -40, -20, 0, 20, 40, 60, 80,100,200,300,400, 500))



predictions_regress <- predict(regression_model, newdata = testData)
actuals <- testData$swe

rmse_regress <- sqrt(mean((predictions_regress - actuals)^2))
rmse_regress

# ============================================================================#
# Basic GBM Model
# ============================================================================#

basic_gbm <- gbm(
  swe ~ lon + lat + elev + max_pr + mean_pr + sum_pr + corresp_temp +
    mean_temp + snow_year_day + ground,
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
  swe ~ lon + lat + elev + max_pr + mean_pr + sum_pr + corresp_temp +
    mean_temp + snow_year_day + ground,
  data = model_vars_df, importance = "permutation",
  num.trees = 200, min.node.size = 10, num.threads = 7
)

rf


rf_prediction_raster = list()
#predict to a raster
for (i in 1:length(test_data_list)){
  rf_prediction_raster[[i]] <- predict(test_data_list[[i]], rf, na.rm=TRUE)
}

rf_prediction_raster_I = rast(rf_prediction_raster)

save(rf_prediction_raster_I, file = "E:/data-raw/swe_model_vars/rf_prediction_raster_I.RData")


plot(rf_prediction_raster[[4]], 
     breaks=c(-500,-400,-300,-200,-100, -80, -60, 
              -40, -20, 0, 20, 40, 60, 80,100,200,300,400, 500))



vip::vip(rf)


predictions_rf <- predict(rf, data = testData)$predictions
actuals <- testData$swe

rmse_rf <- sqrt(mean((predictions_rf - actuals)^2))
rmse_rf
















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
max_swe_layers <- terra::rast("data-raw/swe_model_vars/max_swe_layers.tif")

# get test and train data
train_layers <- max_swe_layers[[1:26]]
test_layers <- max_swe_layers[[27:33]]

# Compute the average across all layers
average_raster <- mean(train_layers, na.rm = TRUE)

# make average predictions
prediction_layers <- c(average_raster, rep(average_raster, 6))

# compute error
error_layers <- prediction_layers - test_layers


# Calculate single RMSE value
mean_squared_errors <- mean((values(error_layers))^2, na.rm = TRUE)
rmse_avg_model <- sqrt(mean_squared_errors)
rmse_avg_model



# ============================================================================#
# cellwise regression Model
# ============================================================================#
max_swe_layers <- terra::rast("data-raw/swe_model_vars/max_swe_layers.tif")
mean_pr_layers <- terra::rast("data-raw/swe_model_vars/mean_pr_layers.tif")
sum_pr_layers <- terra::rast("data-raw/swe_model_vars/sum_pr_layers.tif")
mean_temp_layers <- terra::rast("data-raw/swe_model_vars/mean_temp_layers.tif")
elev_raster_layers <- terra::rast("data-raw/swe_model_vars/elev_raster_layers.tif")


# get 26 years of train data
train_max_swe_layers <- max_swe_layers[[1:26]]
train_mean_pr_layers <- mean_pr_layers[[1:26]]
train_sum_pr_layers <- sum_pr_layers[[1:26]]
train_mean_temp_layers <- mean_temp_layers[[1:26]]
train_elev_raster_layers <- elev_raster_layers[[1:26]]

# combine train data into a multi-layer single raster
train_raster <- c(
  train_max_swe_layers, train_mean_pr_layers,
  train_sum_pr_layers, train_mean_temp_layers,
  train_elev_raster_layers
)



# get 6 years of test data
test_max_swe_layers <- max_swe_layers[[27:33]]
test_mean_pr_layers <- mean_pr_layers[[27:33]]
test_sum_pr_layers <- sum_pr_layers[[27:33]]
test_mean_temp_layers <- mean_temp_layers[[27:33]]
test_elev_raster_layers <- elev_raster_layers[[27:33]]


# combine train data into a multi-layer single raster
test_raster <- c(
  test_mean_pr_layers,
  test_sum_pr_layers, test_mean_temp_layers,
  test_elev_raster_layers
)



source("R/cellwise_lm_sol.R")
source("R/cellwise_matrix_sol.R")

raster1 <- c(train_max_swe_layers, train_sum_pr_layers, train_elev_raster_layers)
raster2 <- c(train_max_swe_layers, train_sum_pr_layers, train_elev_raster_layers, train_mean_temp_layers)
raster3 <- c(train_max_swe_layers, train_sum_pr_layers, train_mean_temp_layers)

l <- regress(y = train_max_swe_layers, train_sum_pr_layers)

tictoc::tic() # 7mins
lm_model_layer1 <- terra::app(raster1, cellwise_lm_sol, variables = 2)
tictoc::toc()

tictoc::tic() # 7mins
lm_model_layer2 <- terra::app(raster2, cellwise_lm_sol, variables = 3)
tictoc::toc()

tictoc::tic() # 7mins
lm_model_layer3 <- terra::app(raster3, cellwise_lm_sol, variables = 3)
tictoc::toc()


prediction_1 <- sum(l[[1]], l[[2]] * train_sum_pr_layers, na.rm = TRUE)


prediction_2 <- sum(lm_model_layer1[[1]], lm_model_layer1[[2]] * test_sum_pr_layers,
  lm_model_layer1[[3]] * test_max_swe_layers,
  na.rm = TRUE
)

prediction_3 <- sum(lm_model_layer2[[1]],
  lm_model_layer2[[2]] * test_sum_pr_layers,
  lm_model_layer2[[3]] * test_max_swe_layers,
  lm_model_layer2[[4]] * test_mean_temp_layers,
  na.rm = TRUE
)

prediction_4 <- sum(lm_model_layer3[[1]],
  lm_model_layer2[[2]] * test_sum_pr_layers,
  lm_model_layer2[[4]] * test_mean_temp_layers,
  na.rm = TRUE
)

error1 <- prediction_1 - test_max_swe_layers
error2 <- prediction_2 - test_max_swe_layers
error3 <- prediction_3 - test_max_swe_layers
error4 <- prediction_4 - test_max_swe_layers

rmse_1 <- sqrt(mean((values(error1))^2, na.rm = TRUE))
rmse_2 <- sqrt(mean((values(error2))^2, na.rm = TRUE))
rmse_3 <- sqrt(mean((values(error3))^2, na.rm = TRUE))
rmse_4 <- sqrt(mean((values(error4))^2, na.rm = TRUE))
rmse_1
rmse_2
rmse_3
rmse_4
