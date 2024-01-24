################################################################################
## STEP 0: INITIAL SETUP
################################################################################
# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(terra)
library(lubridate)

hist_finer_layers <- terra::rast("D:/data-raw/prism/raster/prism_day_tmean_raster.tif") # (deg C) 4km
hist_coarser_layers <- terra::rast("D:/data-raw/spear/raster/spear_hist_day_tas_20010101_20101231.tif")
future_coarser_layer <- terra::rast("D:/data-raw/spear/raster/spear_hist_day_tas_20110101_20141231.tif") # K 50km

# date : 2011-01-01
future_coarser_layer <- future_coarser_layer[[1]]
future_finer_layer <- hist_finer_layers[[10958]]

source("R/dates_for_analog_filter.R")

################################################################################
## STEP 1: bias correct the GCM (cdf method or adjust using a mean)
################################################################################

# skip for now



################################################################################
## STEP 2: Analog selection
################################################################################


# ============================================================================#
# check for missing dates in hist_coarser_layers
# ============================================================================#
# Retrieve the date/time information from the raster
coarser_dates <- terra::time(hist_coarser_layers)

# Find the minimum and maximum dates in the set
min_date <- min(coarser_dates)
max_date <- max(coarser_dates)

# Create a sequence of dates from min_date to max_date
all_dates <- seq(min_date, max_date, by = "days")

# Find the missing dates by comparing the original set with the sequence
missing_dates <- setdiff(all_dates, coarser_dates)

# Print the missing dates if any
if (length(missing_dates) > 0) {
  print("Missing dates:")
  print(missing_dates)
} else {
  print("No missing dates.")
}


# ============================================================================#
# check for missing dates in hist_finer_layers
# ============================================================================#
finer_dates <- terra::time(hist_finer_layers)

# Find the minimum and maximum dates in the set
min_date <- min(finer_dates)
max_date <- max(finer_dates)

# Create a sequence of dates from min_date to max_date
all_dates <- seq(min_date, max_date, by = "days")

# Find the missing dates by comparing the original set with the sequence
missing_dates <- setdiff(all_dates, finer_dates)

# Print the missing dates if any
if (length(missing_dates) > 0) {
  print("Missing dates:")
  print(missing_dates)
} else {
  print("No missing dates.")
}


# ============================================================================#
# find common dates between finer_layers and coarser_layers
# filter layers based on common dates
# ============================================================================#

# Find the common dates between set1 and set2
common_dates <- as.Date(intersect(coarser_dates, finer_dates))

# Subset the raster stack based on the desired dates
finer_layers_common <- finer_layers[[which(finer_dates %in% common_dates)]]
coarser_layers_common <- coarser_layers[[which(coarser_dates %in% common_dates)]]


# ============================================================================#
# #Convert deg C to K
# ============================================================================#
finer_layers_common <- finer_layers_common + 273.15


remove(finer_layers, coarser_layers)

# ============================================================================#
# make my life easy for now
# ============================================================================#
# finer_layers_common <- finer_layers_common[[1:2000]]
# coarser_layers_common <- coarser_layers_common[[1:2000]]

# ============================================================================#
# find common dates between finer_layers and coarser_layers
# ============================================================================#

crs_coarser <- crs(coarser_layers_common)
crs_finer <- crs(finer_layers_common)

# If CRSs are different, reproject finer_layers to match the CRS of coarser_layers
# takes long to complete (2 hr)
if (crs_finer != crs_coarser) {
  finer_layers_common <- terra::project(
    x = finer_layers_common,
    y = crs_coarser, threads = TRUE
  )
}

# reproject the future_finer_layer to match the CRS of coarser_layers
future_finer_layer <- terra::project(
  x = future_finer_layer, y = crs_coarser,
  threads = TRUE
)

# Shift raster data (prism)
finer_layers_common <- terra::shift(finer_layers_common, dx = 360)
future_finer_layer <- terra::shift(future_finer_layer, dx = 360)


# save for later use
terra::writeRaster(finer_layers_common,
  filename = "D:data-raw/other/finer_layers_common.tif",
  overwrite = TRUE
)

terra::writeRaster(future_finer_layer,
  filename = "D:data-raw/other/future_finer_layer.tif",
  overwrite = TRUE
)

terra::writeRaster(coarser_layers_common,
  filename = "D:data-raw/other/coarser_layers_common.tif",
  overwrite = TRUE
)

terra::writeRaster(future_coarser_layer,
  filename = "D:data-raw/other/future_coarser_layer.tif",
  overwrite = TRUE
)




# ============================================================================#
# create a coarsened version of finer_layers
# ============================================================================#
# Resample 'finer_layers' to have the same resolution as 'coarser_layers'
coarser_to_finer_layers <- terra::project(coarser_layers_common,
  finer_layers_common,
  method = "bilinear"
)

terra::writeRaster(coarser_to_finer_layers,
  filename = "D:data-raw/other/coarser_to_finer_layers.tif",
  overwrite = TRUE
)

coarser_to_finer_layers <- rast("D:data-raw/other/coarser_to_finer_layers.tif")

################################################################################
################################################################################
################################################################################
library(terra)
library(lubridate)
source("R/dates_for_analog_filter.R")

# read in raster
finer_layers_common <- rast("D:data-raw/other/finer_layers_common.tif")
future_finer_layer <- rast("D:data-raw/other/future_finer_layer.tif")
coarser_layers_common <- rast("D:data-raw/other/coarser_layers_common.tif")
future_coarser_layer <- rast("D:data-raw/other/future_coarser_layer.tif")
# coarser_to_finer_layers <- rast("D:data-raw/other/coarser_to_finer_layers.tif")

# test function
future_layer <- future_coarser_layer
hist_layers <- coarser_layers_common

finer_dates <- terra::time(finer_layers_common)
coarser_dates <- terra::time(coarser_layers_common)
common_dates <- as.Date(intersect(coarser_dates, finer_dates))

################################################################################
## STEP 3: find analogues index ---- 45 days
################################################################################
# ============================================================================#
# if global = TRUE, return vector of index else layers of index
# ============================================================================#

best_analog_index <- analog_day_index(
  future_layer = future_layer, hist_layers = hist_layers,
  n = 600, common_dates = common_dates, global = TRUE
)

# ============================================================================#
#  filter the analog layer based on best_analog_index
# ============================================================================#

ananlog_finer_layers = finer_layers_common[[best_analog_index]]
ananlog_coarser_layers = coarser_layers_common[[best_analog_index]]


# ============================================================================#
# create a finer version of ananlog_coarser_layers
# ============================================================================#
ananlog_coarser_to_finer_layers <- terra::project(ananlog_coarser_layers,
                                          ananlog_finer_layers,
                                          method = "bilinear"
)



l <- regress(y = ananlog_finer_layers, ananlog_coarser_to_finer_layers)

#interpoltae future 

future_coarser_to_finer_layer_predictor  <- terra::project(future_coarser_layer,
                                                   ananlog_finer_layers,
                                                   method = "bilinear"
)

y = l[[1]] +l[[2]]*future_coarser_to_finer_layer_predictor

plot((y-future_finer_layer))


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
