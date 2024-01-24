################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(terra)

source("R/zero_position_before_max.R")
source("R/cal_max_cons_days_below_thrd.R")
source("R/cal_max_cons_days_above_thrd.R")

#manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE) 

# ============================================================================#
# load data
# ============================================================================#
loca_tas <- terra::rast("E:data-raw/loca/raster/loca_hist_day_comb_tas.tif")
loca_pr <- terra::rast("E:/data-raw/loca/raster/loca_hist_day_comb_pr.tif")
ua_swe <- terra::rast("E:data-raw/ua/raster/combined/ua_swe_combined_daily_1982_2014_for_loca_res.tif")


# ============================================================================#
# match the dates for both rasters
# ============================================================================#
# Extract time attributes from both rasters
time_tas <- terra::time(loca_tas)
time_pr <- terra::time(loca_pr)
time_swe <- terra::time(ua_swe)

# Identify indices in loca_tas that match the dates in ua_swe
matching_indices <- which(time_tas %in% time_swe)

# Subset loca_tas & loca_pr using the matching indices
loca_tas <- loca_tas[[matching_indices]]
loca_pr <- loca_pr[[matching_indices]]

# set temp threshold to classify pr as snow (3 cel is 276.15K)
temp_threshold <- 276.15

# ============================================================================#
# Convert from kg/m2/s to mm per day (ppt)
# ============================================================================#
# tictoc::tic()
# loca_pr <- loca_pr * 86400
# tictoc::toc()


