################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(terra)

source("R/zero_position_before_max.R")

# Read in the rasters
loca_tas <- terra::rast("data-raw/loca/raster/loca_hist_day_comb_tas.tif")
loca_pr <- terra::rast("data-raw/loca/raster/loca_hist_day_comb_pr.tif")
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
max_swe_list <- vector("list", length = length(unique_years) - 1)
corresp_temp_list <- vector("list", length = length(unique_years) - 1)
mean_temp_list <- vector("list", length = length(unique_years) - 1)
mean_pr_list <- vector("list", length = length(unique_years) - 1)
sum_pr_list <- vector("list", length = length(unique_years) - 1)
max_pr_list <- vector("list", length = length(unique_years) - 1)
zero_pos_before_max_list <- vector("list", length = length(unique_years) - 1)
max_pr_position_list <- vector("list", length = length(unique_years) - 1)
days_on_ground_list <- vector("list", length = length(unique_years) - 1)


# Initialize a progress bar
pb <- txtProgressBar(min = 0, max = length(unique_years) - 1, style = 3)


tictoc::tic() # 7 hrs
# Loop through each water year (October to September next year)
for (j in 1:(length(unique_years) - 1)) {
  # Update the progress bar
  setTxtProgressBar(pb, j)

  start_year <- unique_years[j]
  end_year <- unique_years[j + 1]

  # Create a mask for the water year (October of start_year to September of end_year)
  water_year_mask <- (years_loca_tas == start_year & months_loca_tas >= 10) |
    (years_loca_tas == end_year & months_loca_tas <= 9)

  # Subset the data
  swe_subset <- ua_swe[[which(water_year_mask)]]
  temp_subset <- loca_tas[[which(water_year_mask)]]
  pr_subset <- loca_pr[[which(water_year_mask)]]

  # Check: Ensure that all subsetted rasters have the same number of layers
  if (nlyr(swe_subset) != nlyr(temp_subset) || nlyr(swe_subset) != nlyr(pr_subset)) {
    stop("Mismatch in number of layers for year: ", start_year)
  }


  # if temperature is below the threshold, keep the precipitation value (as snow),
  # else set to zero (15 mins)
  modified_pr_subset <- terra::ifel(temp_subset < temp_threshold, pr_subset, 0)

  # find the zero position before max precipitation as snow
  zero_pos_before_max <- terra::app(modified_pr_subset, zero_position_before_max)


  # find the index of the max precipitation as snow for each cell
  prp_index <- which.max(modified_pr_subset)

  # number of days assumed snow(precipitation) stayed on ground
  ground <- prp_index - zero_pos_before_max

  # get the max swe,max pr, mean pr, corresponding temp,amd mean temp
  max_snow <- max(swe_subset)
  max_pr <- max(modified_pr_subset)
  mean_pr <- terra::rapp(
    x = modified_pr_subset, first = zero_pos_before_max,
    last = prp_index, fun = "mean"
  )
  sum_pr <- terra::rapp(
    x = modified_pr_subset, first = zero_pos_before_max,
    last = prp_index, fun = "sum"
  )
  corresp_temp <- terra::selectRange(x = temp_subset, y = prp_index)
  mean_temp <- terra::rapp( x = temp_subset, first = zero_pos_before_max,
                            last = prp_index, fun = "mean")

  # Store the max SWE and corresponding temp layer in the lists
  max_swe_list[[j]] <- max_snow
  max_pr_list[[j]] <- max_pr
  mean_pr_list[[j]] <- mean_pr
  sum_pr_list[[j]] <- sum_pr
  corresp_temp_list[[j]] <- corresp_temp
  mean_temp_list[[j]] <- mean_temp
  zero_pos_before_max_list[[j]] <- zero_pos_before_max
  max_pr_position_list[[j]] <- prp_index
  days_on_ground_list[[j]] <- ground
}
# Close the progress bar
close(pb)

tictoc::toc()

# Convert the lists to SpatRaster (multi-layer raster)
max_swe_layers <- terra::rast(max_swe_list)
max_pr_layers <- terra::rast(max_pr_list)
mean_pr_layers <- terra::rast(mean_pr_list)
sum_pr_layers <- terra::rast(sum_pr_list)
corresp_temp_layers <- terra::rast(corresp_temp_list)
mean_temp_layers <- terra::rast(mean_temp_list)
zero_pos_before_max_layers <- terra::rast(zero_pos_before_max_list)
max_pr_position_layers <- terra::rast(max_pr_position_list)
days_on_ground_layers <- terra::rast(days_on_ground_list)

# Save the resulting multi-layer raster layers
writeRaster(max_swe_layers, "data-raw/swe_model_vars/max_swe_layers.tif",
  overwrite = TRUE
)

writeRaster(max_pr_layers,
            "data-raw/swe_model_vars/max_pr_layers.tif",
            overwrite = TRUE
)
writeRaster(mean_pr_layers,
            "data-raw/swe_model_vars/mean_pr_layers.tif",
            overwrite = TRUE
)
writeRaster(sum_pr_layers,
            "data-raw/swe_model_vars/sum_pr_layers.tif",
            overwrite = TRUE
)

writeRaster(corresp_temp_layers,
            "data-raw/swe_model_vars/corresp_temp_layers.tif",
            overwrite = TRUE
)

writeRaster(mean_temp_layers,
            "data-raw/swe_model_vars/mean_temp_layers.tif",
            overwrite = TRUE
)

writeRaster(zero_pos_before_max_layers,
            "data-raw/swe_model_vars/zero_pos_before_max_layers.tif",
            overwrite = TRUE
)

writeRaster(max_pr_position_layers,
            "data-raw/swe_model_vars/max_pr_position_layers.tif",
            overwrite = TRUE
)

writeRaster(days_on_ground_layers,
            "data-raw/swe_model_vars/days_on_ground_layers.tif",
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



