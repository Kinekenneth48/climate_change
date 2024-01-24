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

# Read in the rasters
loca_tas <- terra::rast("data-raw/loca/raster/loca_hist_day_comb_tas.tif")
loca_pr <- terra::rast("E:/data-raw/loca/raster/loca_hist_day_comb_pr.tif")
ua_swe <- terra::rast("data-raw/ua/raster/combined/ua_swe_combined_daily_1982_2014_for_loca_res.tif")

# max_swe_positions <- terra::rast("data-raw/swe_model_vars/max_swe_pos_layers.tif")
# zero_positions <- terra::rast("data-raw/swe_model_vars/zero_pos_before_max_layers.tif")

terraOptions(memfrac = 0.85, verbose = TRUE)

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

################################################################################
## STEP 1: get max annual snow and corresponding temp
################################################################################

# Extracting years and months from the time attributes
years_loca_tas <- as.numeric(format(time(loca_tas), "%Y"))
months_loca_tas <- as.numeric(format(time(loca_tas), "%m"))

# Identify the unique years in your dataset
unique_years <- unique(years_loca_tas)

# Initialize vectors to store the maximum SWE and corresponding temperature
total_ppt_list <- vector("list", length = length(unique_years) - 1)
mean_temp_list <- vector("list", length = length(unique_years) - 1)
max_swe_list <- vector("list", length = length(unique_years) - 1)
freezing_days_list <- vector("list", length = length(unique_years) - 1)
snowfall_days_list <- vector("list", length = length(unique_years) - 1)
max_con_freeze_days_list <- vector("list", length = length(unique_years) - 1)
melt_degree_days_list <- vector("list", length = length(unique_years) - 1)
temp_diff_list <- vector("list", length = length(unique_years) - 1)


# Initialize a progress bar
pb <- txtProgressBar(min = 0, max = length(unique_years) - 1, style = 3)


tictoc::tic() # 51 hrs
# Loop through each water year (October to September next year)
for (j in 1:(length(unique_years) - 1)) {
  # Update the progress bar
  setTxtProgressBar(pb, j)

  start_year <- unique_years[j]
  end_year <- unique_years[j + 1]

  # Create a mask for the water year (winter months)
  water_year_mask <- (years_loca_tas == start_year & months_loca_tas >= 12) |
    (years_loca_tas == end_year & months_loca_tas <= 2)

  # Create a mask for the water year (October of start_year to September of end_year)
  water_year_mask_swe <- (years_loca_tas == start_year & months_loca_tas >= 10) |
    (years_loca_tas == end_year & months_loca_tas <= 9)

  # Subset the data
  swe_subset <- ua_swe[[which(water_year_mask_swe)]]
  temp_subset <- loca_tas[[which(water_year_mask)]]
  pr_subset <- loca_pr[[which(water_year_mask)]]

  # # Check: Ensure that all subsetted rasters have the same number of layers
  # if (nlyr(swe_subset) != nlyr(temp_subset) || nlyr(swe_subset) != nlyr(pr_subset)) {
  #   stop("Mismatch in number of layers for year: ", start_year)
  # }

  # 1. get total ppt in winter months
  total_ppt <- sum(pr_subset, na.rm = TRUE)

  # 2. get mean temp in winter months
  mean_temp <- mean(temp_subset, na.rm = TRUE)

  # 3. get annual max swe
  max_swe <- max(swe_subset, na.rm = TRUE)

  # 4. days below freezing
  freezing_days <- sum(temp_subset < temp_threshold, na.rm = TRUE)

  # 5. snowfall days
  snowfall_days <- terra::app((temp_subset < temp_threshold) & (pr_subset > 0),
    fun = sum, na.rm = TRUE
  )

  # 6. max consecutive freezing days
  max_con_freeze_days <- terra::app(temp_subset, cal_max_cons_days_below_thrd,
    threshold_temp = temp_threshold
  )

  # 7. freezing degree days
  melt_degree_days <- sum(terra::ifel(temp_subset < temp_threshold, temp_subset, 0),
    na.rm = TRUE
  ) / nlyr(temp_subset)

  # 8. difference in temp IN WINTER MONTH
  temp_diff <- max(temp_subset, na.rm = TRUE) - min(temp_subset, na.rm = TRUE)



  # Store layer in the lists
  total_ppt_list[[j]] <- total_ppt
  mean_temp_list[[j]] <- mean_temp
  max_swe_list[[j]] <- max_swe
  freezing_days_list[[j]] <- freezing_days
  snowfall_days_list[[j]] <- snowfall_days
  max_con_freeze_days_list[[j]] <- max_con_freeze_days
  melt_degree_days_list[[j]] <- melt_degree_days
  temp_diff_list[[j]] <- temp_diff
}
# Close the progress bar
close(pb)

tictoc::toc()

# Convert the lists to SpatRaster (multi-layer raster)
total_ppt_layers <- terra::rast(total_ppt_list)
mean_temp_layers <- terra::rast(mean_temp_list)
max_swe_layers <- terra::rast(max_swe_list)
freezing_days_layers <- terra::rast(freezing_days_list)
snowfall_days_layers <- terra::rast(snowfall_days_list)
max_con_freeze_days_layers <- terra::rast(max_con_freeze_days_list)
melt_degree_days_layers <- terra::rast(melt_degree_days_list)
temp_diff_layers <- terra::rast(temp_diff_list)


# Save the resulting multi-layer raster layers
writeRaster(total_ppt_layers,
            "data-raw/swe_model_vars/total_ppt_layers.tif",
  overwrite = TRUE
)

writeRaster(mean_temp_layers,
  "data-raw/swe_model_vars/mean_temp_layers.tif",
  overwrite = TRUE
)
writeRaster(max_swe_layers,
  "data-raw/swe_model_vars/max_swe_layers.tif",
  overwrite = TRUE
)
writeRaster(freezing_days_layers,
  "data-raw/swe_model_vars/freezing_days_layers.tif",
  overwrite = TRUE
)

writeRaster(snowfall_days_layers,
  "data-raw/swe_model_vars/snowfall_days_layers.tif",
  overwrite = TRUE
)

writeRaster(max_con_freeze_days_layers,
  "data-raw/swe_model_vars/max_con_freeze_days_layers.tif",
  overwrite = TRUE
)

writeRaster(melt_degree_days_layers,
  "data-raw/swe_model_vars/melt_degree_days_layers.tif",
  overwrite = TRUE
)

writeRaster(temp_diff_layers,
  "data-raw/swe_model_vars/temp_diff_layers.tif",
  overwrite = TRUE
)





# ============================================================================#
# get log and lat per grid cell
# ============================================================================#

lon <- terra::init(loca_pr[[1]], "x")
lat <- terra::init(loca_pr[[1]], "y")

writeRaster(lon,
  "data-raw/swe_model_vars/lon.tif",
  overwrite = TRUE
)

writeRaster(lat,
  "data-raw/swe_model_vars/lat.tif",
  overwrite = TRUE
)

# ============================================================================#
# get elevation and ecoregion
# ============================================================================#
