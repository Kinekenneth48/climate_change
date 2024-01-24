################################################################################
## STEP 0: INITIAL SETUP
################################################################################
# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(terra)
library(tictoc)

# ============================================================================#
# load downscaled mosaic layers
# ============================================================================#

fine_downscaled_future_q1 <- rast("E:data-raw/macca/q1/fine_downscaled_future_q1.tif")
fine_downscaled_future_q2 <- rast("E:data-raw/macca/q2/fine_downscaled_future_q2.tif")
fine_downscaled_future_q3 <- rast("E:data-raw/macca/q3/fine_downscaled_future.tif")
fine_downscaled_future_q4 <- rast("E:data-raw/macca/q4/fine_downscaled_future_q4.tif")


# ============================================================================#
# combine splits into one mosaic
# ============================================================================#
# 343.08 sec elapsed
tictoc::tic()
fine_downscaled_future_mosaic <- terra::mosaic(fine_downscaled_future_q1,
  fine_downscaled_future_q2,
  fine_downscaled_future_q3,
  fine_downscaled_future_q4,
  fun = "mean"
)
tictoc::toc()
#=================================================================#
# load true future layers and match extent to that of fine_downscaled_future_mosaic
# ============================================================================#
true_layers =rast("E:data-raw/other/future_ua_finer_layers.tif")


true_layers <- terra::crop(
  x = true_layers,
  y =  terra::ext(fine_downscaled_future_mosaic[[1]])
)

# ============================================================================#
# save data
# ============================================================================#

terra::writeRaster(fine_downscaled_future_mosaic,
  filename = "E:data-raw/macca/mosaic/fine_downscaled_future_mosaic.tif",
  overwrite = TRUE
)


fine_downscaled_future_mosaic =rast("E:data-raw/macca/mosaic/fine_downscaled_future_mosaic.tif")



################################################################################
## STEP 1: create annual max and compare
################################################################################


downscale_2013 <- max(fine_downscaled_future_mosaic[[1:365]])
downscale_2014 <- max(fine_downscaled_future_mosaic[[366:730]])

true_2013 <- max(true_layers[[1:365]])
true_2014 <- max(true_layers[[366:730]])

d_2013_mosaic <- (downscale_2013-true_2013)/true_2013
d_2014_mosaic <- (downscale_2014-true_2014)/true_2014

# Assuming downscale_2013 and true_2013 are already loaded
# Calculate the difference
difference_2013 = true_2013 - downscale_2013

# Calculate percentage change
# Handle division by zero by replacing 0 in true_2013 with a small number (e.g., 0.001)
true_2013_corrected = clamp(true_2013, lower=0.001)
percentage_change_2013 = (difference_2013 / true_2013_corrected) * 100

###########################################################################

# Calculate the difference
difference_2014 = true_2014 - downscale_2014

# Calculate percentage change
# Handle division by zero by replacing 0 in true_2013 with a small number (e.g., 0.001)
true_2014_corrected = clamp(true_2014, lower=0.001)
percentage_change_2014 = (difference_2014 / true_2014_corrected) * 100

plot(percentage_change_2013,breaks =
       c(-100, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 100))

plot(percentage_change_2014,breaks =
       c(-100, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 100))


d_2013 <- rast("E:data-raw/other/d_2013.tif")
d_2014 <- rast("E:data-raw/other/d_2014.tif")

d_2013= terra::crop(
  x = d_2013,
  y =  terra::ext(d_2013_mosaic)
)

plot(d_2013, breaks = c(-100, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 100))
plot(d_2013_mosaic, breaks = c(-100, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 100))

plot(d_2014, breaks = c(-50, 50))

RMSE_2013 <- sqrt(mean(((as.vector(d_2013))^2), na.rm = TRUE))
RMSE_2014 <- sqrt(mean(((as.vector(d_2014))^2), na.rm = TRUE))
