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
extent <- terra::ext(235, 293, 24, 50)




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
# Crop GCM and UA data for first quadrant
# ============================================================================#


hist_ua_finer_layers <- terra::crop(
  x = hist_ua_finer_layers,
  y = extent
)


hist_gcm_coarser_layers <- terra::crop(
  x = hist_gcm_coarser_layers,
  y = extent
)

# ============================================================================#
# split raster into train and test
# ============================================================================#
# 2013 & 2014 snow year (future layers)
future_gcm_coarser_layers_q1 <- hist_gcm_coarser_layers_q1[[11316:12045]]
future_ua_finer_layers_q1 <- hist_ua_finer_layers_q1[[11316:12045]]

# 1982 to 2012 snow year (hist layers)
hist_ua_finer_layers_q1 <- hist_ua_finer_layers_q1[[1:11315]]
hist_gcm_coarser_layers_q1 <- hist_gcm_coarser_layers_q1[[1:11315]]


# ============================================================================#
# Interpolate UA grid data to GCM grid resolution
# ============================================================================#
# 12330.62  sec elapsed
tictoc::tic()
fine_to_coarse_hist_ua_layers_q1 <- terra::project(hist_ua_finer_layers_q1,
                                                   hist_gcm_coarser_layers_q1[[1]],
                                                   method = "bilinear"
)
tictoc::toc()

# ============================================================================#
# Mask GCM
# ============================================================================#
future_gcm_coarser_layers_q1 <- terra::mask(
  x = future_gcm_coarser_layers_q1,
  mask = fine_to_coarse_hist_ua_layers_q1[[1]]
)

hist_gcm_coarser_layers_q1 <- terra::mask(
  x = hist_gcm_coarser_layers_q1,
  mask = fine_to_coarse_hist_ua_layers_q1[[1]]
)


# ============================================================================#
# Save data
# ============================================================================#
terra::writeRaster(hist_gcm_coarser_layers_q1,
                   filename = "data-raw/macca/q1/hist_gcm_coarser_layers_q1.tif",
                   overwrite = TRUE
)


terra::writeRaster(hist_ua_finer_layers_q1,
                   filename = "E:data-raw/macca/q1/hist_ua_finer_layers_q1.tif",
                   overwrite = TRUE
)

terra::writeRaster(future_gcm_coarser_layers_q1,
                   filename = "E:data-raw/macca/q1/future_gcm_coarser_layers_q1.tif",
                   overwrite = TRUE
)


terra::writeRaster(future_ua_finer_layers_q1,
                   filename = "E:data-raw/macca/q1/future_ua_finer_layers_q1.tif",
                   overwrite = TRUE
)


terra::writeRaster(fine_to_coarse_hist_ua_layers_q1,
                   filename = "E:data-raw/macca/q1/fine_to_coarse_hist_ua_layers_q1.tif",
                   overwrite = TRUE
)
