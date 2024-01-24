################################################################################
## STEP 0: INITIAL SETUP
################################################################################
# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(terra)
library(lubridate)

terraOptions(memfrac = 0.85, verbose = TRUE)

hist_gcm_coarser_layers <- terra::rast("data-raw/cmip6/GFDL_ESM4_day_snw_hist.tif")
hist_ua_finer_layers <- terra::rast("data-raw/ua/raster/combined/ua_swe_combined_daily_1982_2014_for_loca_res.tif")

# define the extent of q1
q3_extent <- terra::ext(260, 300, 24, 40)

source("R/dates_for_analog_filter.R")
source("R/analog_day_index.R")
source("R/get_21_days_across_years_vector.R")
source("R/get_21_days_snow_year.R")






################################################################################
## MACA Step 1: Common Grid
################################################################################
# ============================================================================#
# # Replace negative zero with zero in GCM
# ============================================================================#
hist_gcm_coarser_layers <- terra::ifel(
  hist_gcm_coarser_layers < 0, 0,
  hist_gcm_coarser_layers
)


# ============================================================================#
# common grid dates for GCM and observed data
# ============================================================================#
coarser_dates <- terra::time(hist_gcm_coarser_layers)
finer_dates <- terra::time(hist_ua_finer_layers)

# Find the common dates between set1 and set2
common_dates <- as.Date(intersect(coarser_dates, finer_dates), origin = "1970-01-01")

# Subset the raster stack based on the desired dates
hist_ua_finer_layers <- hist_ua_finer_layers[[which(finer_dates %in% common_dates)]]
hist_gcm_coarser_layers <- hist_gcm_coarser_layers[[which(coarser_dates %in% common_dates)]]

# ============================================================================#
# Crop GCM and UA data for third quadrant
# ============================================================================#

hist_gcm_coarser_layers_q3 <- terra::crop(
  x = hist_gcm_coarser_layers,
  y = q3_extent
)

hist_ua_finer_layers_q3 <- terra::crop(
  x = hist_ua_finer_layers,
  y = q3_extent
)

# ============================================================================#
# split raster into train and test
# ============================================================================#
# 2013 & 2014 snow year (future layers)
future_gcm_coarser_layers_q3 <- hist_gcm_coarser_layers_q3[[11316:12045]]
future_ua_finer_layers_q3 <- hist_ua_finer_layers_q3[[11316:12045]]

# 1982 to 2012 snow year (hist layers)
hist_ua_finer_layers_q3 <- hist_ua_finer_layers_q3[[1:11315]]
hist_gcm_coarser_layers_q3 <- hist_gcm_coarser_layers_q3[[1:11315]]


# ============================================================================#
# Interpolate UA grid data to GCM grid resolution
# ============================================================================#
# 9086.72 sec elapsed
tictoc::tic()
fine_to_coarse_hist_ua_layers_q3 <- terra::project(hist_ua_finer_layers_q3,
  hist_gcm_coarser_layers_q3[[1]],
  method = "bilinear"
)
tictoc::toc()

# ============================================================================#
# Mask GCM
# ============================================================================#
future_gcm_coarser_layers_q3 <- terra::mask(
  x = future_gcm_coarser_layers_q3,
  mask = fine_to_coarse_hist_ua_layers_q3[[1]]
)

hist_gcm_coarser_layers_q3 <- terra::mask(
  x = hist_gcm_coarser_layers_q3,
  mask = fine_to_coarse_hist_ua_layers_q3[[1]]
)


# ============================================================================#
# Save data
# ============================================================================#
terra::writeRaster(hist_gcm_coarser_layers_q3,
  filename = "data-raw/macca/q3/hist_gcm_coarser_layers_q3.tif",
  overwrite = TRUE
)


terra::writeRaster(hist_ua_finer_layers_q3,
  filename = "data-raw/macca/q3/hist_ua_finer_layers_q3.tif",
  overwrite = TRUE
)

terra::writeRaster(future_gcm_coarser_layers_q3,
  filename = "data-raw/macca/q3/future_gcm_coarser_layers_q3.tif",
  overwrite = TRUE
)


terra::writeRaster(future_ua_finer_layers_q3,
  filename = "data-raw/macca/q3/future_ua_finer_layers_q3.tif",
  overwrite = TRUE
)


terra::writeRaster(fine_to_coarse_hist_ua_layers_q3,
  filename = "data-raw/macca/q3/fine_to_coarse_hist_ua_layers_q3.tif",
  overwrite = TRUE
)







################################################################################
## MACA Step 2: Epoch Adjustment
################################################################################


# ============================================================================#
# 21-day moving window
# ============================================================================#
# Extract time information from future and historical SpatRaster objects
future_dates <- time(future_gcm_coarser_layers_q3)
hist_dates <- time(hist_gcm_coarser_layers_q3)

# Determine the maximum and minimum years in the historical data
max_year_hist <- max(as.numeric(format(hist_dates, "%Y")))
min_year_hist <- min(as.numeric(format(hist_dates, "%Y")))

# Determine the maximum and minimum years in the future data
max_year_future <- max(as.numeric(format(future_dates, "%Y")))
min_year_future <- min(as.numeric(format(future_dates, "%Y")))

# Initialize a list to store each diff_adjusted layer
diff_adjusted_list <- list()

# 84.47 sec elapsed
tictoc::tic()
# Loop through each layer of the future data
for (i in 1:terra::nlyr(future_gcm_coarser_layers_q3)) {
  # Calculate the 21-day period for the current future date considering the snow year
  # future_moving_dates <- get_21_days_snow_year(terra::time(future_gcm_coarser_layers[[i]]))
  future_moving_dates <- get_21_days_across_years_vector(
    terra::time(future_gcm_coarser_layers_q3[[i]]), min_year_future,
    max_year_future
  )

  # Compute the mean of the future data within the 21-day period
  future_mean <- mean(future_gcm_coarser_layers_q3[[which(future_dates %in%
                                                         future_moving_dates)]],
    na.rm = TRUE
  )

  # Calculate the corresponding 21-day period across historical years
  hist_moving_dates <- get_21_days_across_years_vector(
    terra::time(future_gcm_coarser_layers_q3[[i]]),
    min_year_hist, max_year_hist
  )

  # Compute the mean of the historical data within the calculated 21-day periods
  hist_mean <- mean(hist_gcm_coarser_layers_q3[[which(hist_dates %in%
                                                        hist_moving_dates)]],
    na.rm = TRUE
  )

  # Calculate the difference between future and historical means
  diff <- future_mean - hist_mean

  # Adjust the difference: set negative values to zero
  diff_adjusted <- ifel(diff < 0, 0, diff)

  # Append the adjusted difference to the list
  diff_adjusted_list[[i]] <- diff_adjusted
}
tictoc::toc()

# Combine the list of adjusted differences into a single multi-layer SpatRaster
diff_adjusted_layers_q3 <- rast(diff_adjusted_list)


epoch_future_gcm_coarser_layers_q3 <- future_gcm_coarser_layers_q3 - diff_adjusted_layers_q3


# ============================================================================#
# save data for use later
# ============================================================================#

terra::writeRaster(epoch_future_gcm_coarser_layers_q3,
  filename = "data-raw/other/epoch_future_gcm_coarser_layers_q3.tif",
  overwrite = TRUE
)

terra::writeRaster(diff_adjusted_layers_q3,
  filename = "data-raw/other/diff_adjusted_layers_q3.tif",
  overwrite = TRUE
)







################################################################################
## MACA Step 3: Coarse Bias Correction
################################################################################

# ============================================================================#
# Bias correct hist GCM
# ============================================================================#

# Determine the maximum and minimum years in the historical data
max_year_hist <- max(as.numeric(format(time(fine_to_coarse_hist_ua_layers_q3), "%Y")))
min_year_hist <- min(as.numeric(format(time(fine_to_coarse_hist_ua_layers_q3), "%Y")))

list_hist <- list()

# 17996.2 sec elapsed
tictoc::tic()
for (i in 1:terra::nlyr(fine_to_coarse_hist_ua_layers_q3)) {
  gcm_hist <- terra::mask(
    x = hist_gcm_coarser_layers_q3[[i]],
    mask = fine_to_coarse_hist_ua_layers_q3[[1]]
  )

  # Calculate the corresponding 21-day period across historical years
  hist_moving_dates <- get_21_days_across_years_vector(
    terra::time(gcm_hist),
    min_year_hist, max_year_hist
  )

  target_array <- as.array(gcm_hist)

  obs_array <- as.array(fine_to_coarse_hist_ua_layers_q3[[which(hist_dates %in%
                                                                  hist_moving_dates)]])

  gcm_array <- as.array(hist_gcm_coarser_layers_q3[[which(hist_dates %in%
                                                         hist_moving_dates)]])

  corrected_array <- distfixer::macca_bias_correction_cpp(
    gcm_array = gcm_array, obs_array = obs_array,
    gcm_target_array = target_array
  )

  corrected_array <- rast(corrected_array)
  ext(corrected_array) <- c(xmin = 260, xmax = 300, ymin = 24, ymax = 40)
  crs(corrected_array) <- crs(hist_gcm_coarser_layers)
  time(corrected_array) <- time(hist_gcm_coarser_layers[[i]])

  list_hist[[i]] <- corrected_array
}
tictoc::toc()

bias_corrected_hist_gcm_q3 <- rast(list_hist)

terra::writeRaster(bias_corrected_hist_gcm_q3,
  filename = "data-raw/other/bias_corrected_hist_gcm_q3.tif",
  overwrite = TRUE
)

# ============================================================================#
# Bias correct future GCM
# ============================================================================#
list_future <- list()

# 1072.18 sec elapsed
tictoc::tic()
for (i in 1:terra::nlyr(epoch_future_gcm_coarser_layers_q3)) {
  gcm_future <- epoch_future_gcm_coarser_layers_q3[[i]]
    
    
  # Calculate the corresponding 21-day period across historical years
  future_moving_dates <- get_21_days_across_years_vector(
    terra::time(gcm_future),
    min_year_future, max_year_future
  )

  hist_moving_dates <- get_21_days_across_years_vector(
    terra::time(gcm_future),
    min_year_hist, max_year_hist
  )

  target_array <- as.array(gcm_future)

  obs_array <- as.array(fine_to_coarse_hist_ua_layers_q3[[which(hist_dates %in%
    hist_moving_dates)]])

  gcm_array <- as.array(epoch_future_gcm_coarser_layers_q3[[which(time(
    epoch_future_gcm_coarser_layers_q3
  ) %in% future_moving_dates)]])

  corrected_array <- distfixer::macca_bias_correction_cpp(
    gcm_array = gcm_array, obs_array = obs_array,
    gcm_target_array = target_array
  )

  corrected_array <- rast(corrected_array)
  ext(corrected_array) <- c(xmin = 260, xmax = 300, ymin = 24, ymax = 40)
  crs(corrected_array) <- crs(epoch_future_gcm_coarser_layers_q3)
  time(corrected_array) <- time(epoch_future_gcm_coarser_layers_q3[[i]])

  list_future[[i]] <- corrected_array
}
tictoc::toc()

bias_corrected_future_gcm_q3 <- rast(list_future)


terra::writeRaster(bias_corrected_future_gcm_q3,
  filename = "data-raw/other/bias_corrected_future_gcm_q3.tif",
  overwrite = TRUE
)



################################################################################
# MACA Step 4: Constructed Analogs
################################################################################


fine_list <- list()

# 47229.53 (13 hr) sec elapsed
# 6.5 hrs per snow year
tictoc::tic()
for (i in 1:nlyr(bias_corrected_future_gcm_q3)) {
  # ============================================================================#
  # if global = TRUE, return vector of index else layers of index
  # ============================================================================#

  best_analog_index <- analog_day_index(
    future_layer = bias_corrected_future_gcm_q3[[i]],
    hist_layers = fine_to_coarse_hist_ua_layers_q3,
    n = 20, common_dates = time(fine_to_coarse_hist_ua_layers_q3),
    global = TRUE
  )

  # ============================================================================#
  #  filter the analog layer based on best_analog_index
  # ============================================================================#

  ananlog_finer_layers <- hist_ua_finer_layers_q3[[best_analog_index]]
 # ananlog_coarser_layers <- fine_to_coarse_hist_ua_layers_q3[[best_analog_index]]

 

  fine_list[[i]] <- mean(ananlog_finer_layers)
}
tictoc::toc()

fine_downscaled_future = rast(fine_list)

terra::writeRaster(fine_downscaled_future,
                   filename = "data-raw/other/fine_downscaled_future.tif",
                   overwrite = TRUE
)

downscale_2013 <- max(rast(fine_list[1:365]))
downscale_2014 <- max(rast(fine_list[366:730]))

true_2013 <- max(future_ua_finer_layers[[1:365]])
true_2014 <- max(future_ua_finer_layers[[366:730]])

d_2013 <- true_2013 - downscale_2013
d_2014 <- true_2014 - downscale_2014

terra::writeRaster(d_2013,
  filename = "E:data-raw/other/d_2013.tif",
  overwrite = TRUE
)

terra::writeRaster(d_2014,
  filename = "E:data-raw/other/d_2014.tif",
  overwrite = TRUE
)

plot(d_2013, breaks = c(-50, 50))
plot(d_2014, breaks = c(-50, 50))

RMSE_2013 <- sqrt(mean(((as.vector(d_2013))^2), na.rm = TRUE))
RMSE_2014 <- sqrt(mean(((as.vector(d_2014))^2), na.rm = TRUE))




























# ============================================================================#
# create a finer version of ananlog_coarser_layers
# ============================================================================#
ananlog_coarser_to_finer_layers <- terra::project(ananlog_coarser_layers,
  ananlog_finer_layers,
  method = "bilinear"
)



l <- regress(y = ananlog_finer_layers, ananlog_coarser_to_finer_layers)

# interpoltae future

future_coarser_to_finer_layer_predictor <- terra::project(future_coarser_layer,
  ananlog_finer_layers,
  method = "bilinear"
)

y <- l[[1]] + l[[2]] * future_coarser_to_finer_layer_predictor

plot((y - future_finer_layer))


################################################################################
## STEP 3: fit regression at cell level
################################################################################

source("R/cellwise_lm_sol.R")

# ============================================================================#
#  select values at best analog positions
# ============================================================================#

# Create an empty list to store the selected layers
best_analog_values <- list()

for (i in 1:terra::nlyr(best_analog_pos_select_f_coarser_layers)) {
  best_analog_values[[i]] <-
    terra::selectRange(x = select_f_coarser_layers, y = best_analog_pos_select_f_coarser_layers[[i]])
}

# Create a multilayer raster from the list of selected layers
predictor_layers <- rast(best_analog_values)

# ============================================================================#


# Create an empty list to store the selected layers
best_analog_values_finer <- list()

for (i in 1:terra::nlyr(best_analog_pos_select_f_coarser_layers)) {
  best_analog_values_finer[[i]] <-
    terra::selectRange(x = select_f_coarser_layers, y = best_analog_pos_select_f_coarser_layers[[i]])
}

# Create a multilayer raster from the list of selected layers
predictor_layers <- rast(best_analog_values_finer)


# ============================================================================#




# ============================================================================#
#  fit regression to predictor at cell level
# ============================================================================#

raster_yx <- c(future_layer, predictor_layers)

tictoc::tic() # 7mins
lm_model_layer_no_inter <- terra::app(raster_yx, cellwise_lm_sol,
  variables = 5,
  include_intercept = FALSE
)
tictoc::toc()

tictoc::tic() # 7mins
lm_model_layer_inter <- terra::app(raster_yx, cellwise_lm_sol,
  variables = 5,
  include_intercept = TRUE
)
tictoc::toc()

lm_model_layer_no_inter
lm_model_layer_inter
# ============================================================================#


y <- as.vector(future_layer)

data <- data.frame(
  y = as.vector(future_layer),
  x1 = as.vector(predictor_layers[[1]]),
  x2 = as.vector(predictor_layers[[2]]),
  x3 = as.vector(predictor_layers[[3]]),
  x4 = as.vector(predictor_layers[[4]]),
  x5 = as.vector(predictor_layers[[5]])
)

l_model <- lm(formula = y ~ ., data = data)

new_names <- c("x1", "x2", "x3", "x4", "x5") # Replace with your desired names

# Set new names
names(predictor_layers) <- new_names

predicted_raster <- predict(predictor_layers, l_model)


# ============================================================================#
l <- regress(y = future_layer, rast(best_analog_values[1]))

raster_yx <- c(future_layer, rast(best_analog_values[1]))
ll <- terra::app(raster_yx, cellwise_lm_sol,
  variables = 1,
  include_intercept = TRUE
)
l
ll



# Original resolution
original_res_x <- 0.625
original_res_y <- 0.5

# Target resolution
target_res_x <- 0.04166667
target_res_y <- 0.04166667

# Calculate the aggregation factors
agg_factor_x <- original_res_x / target_res_x
agg_factor_y <- original_res_y / target_res_y

# Assuming 'predictor_layers' is your SpatRaster
aggregated_raster <- terra::project(best_analog_pos_non_select_f_coarser_layers,
  finer_layers_common_crs,
  method = "average"
)

# Check if the values are integers
all(apply(aggregated_raster, 2, function(x) all(x == floor(x))))

# Create an empty list to store the selected layers
y_values <- list()

for (i in 1:terra::nlyr(aggregated_raster)) {
  y_values[[i]] <-
    terra::selectRange(x = finer_layers_common_crs, y = aggregated_raster[[i]])
}

# Create a multilayer raster from the list of selected layers
predictorFINER_layers <- rast(y_values)
names(predictorFINER_layers) <- new_names
predictedFINER_raster <- predict(predictorFINER_layers, l_model)
