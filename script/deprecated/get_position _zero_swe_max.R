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
ua_swe <- terra::rast(
  "data-raw/ua/raster/combined/ua_swe_combined_daily_1982_2014_for_loca_res.tif"
)

# set memory usage by terra
terraOptions(memfrac = 0.85, verbose = TRUE)

# ============================================================================#
# match the dates for both rasters
# ============================================================================#
# Extract time attributes from both rasters
time_tas <- terra::time(loca_tas)
time_swe <- terra::time(ua_swe)

# Identify indices in loca_tas that match the dates in ua_swe
matching_indices <- which(time_tas %in% time_swe)

# Subset loca_tas using the matching indices
loca_tas <- loca_tas[[matching_indices]]


################################################################################
## STEP 1: get max annual SWE position and position of zero closest to max SWE
# per snow year
################################################################################

# Extracting years and months from the time attributes
years_loca_tas <- as.numeric(format(time(loca_tas), "%Y"))
months_loca_tas <- as.numeric(format(time(loca_tas), "%m"))


# Identify the unique years in your dataset
unique_years <- unique(years_loca_tas)

# Initialize vectors to store the maximum SWE position per snow year
max_swe_pos_list <- vector("list", length = length(unique_years) - 1)
zero_pos_before_max_list <- vector("list", length = length(unique_years) - 1)

# Initialize a progress bar
pb <- txtProgressBar(min = 0, max = length(unique_years) - 1, style = 3)


tictoc::tic() #0.8 hr
# Loop through each water year (October to September next year)
for (j in 1:(length(unique_years) - 1)) {
  # Update the progress bar
  setTxtProgressBar(pb, j)

  start_year <- unique_years[j]
  end_year <- unique_years[j + 1]

  # Create a mask for the water year (October of start_year to September of end_year)
  water_year_mask <- (years_loca_tas == start_year & months_loca_tas >= 10) |
    (years_loca_tas == end_year & months_loca_tas <= 9)

  # Subset the data for the snow year
  swe_subset <- ua_swe[[which(water_year_mask)]]

  # find the max swe position
  max_swe_pos_list[[j]] <- terra::which.max(swe_subset)

  # find the zero position before max swe 
  zero_pos_before_max_list[[j]] <- terra::app(swe_subset, zero_position_before_max)
  

}
# Close the progress bar
close(pb)

tictoc::toc()



# Convert the lists to SpatRaster (multi-layer raster)
max_swe_pos_layers <- terra::rast(max_swe_pos_list)
zero_pos_before_max_layers <- terra::rast(zero_pos_before_max_list)

# Save the resulting multi-layer raster layers
writeRaster(max_swe_pos_layers, "data-raw/swe_model_vars/max_swe_pos_layers.tif",
  overwrite = TRUE
)

writeRaster(zero_pos_before_max_layers,
  "data-raw/swe_model_vars/zero_pos_before_max_layers.tif",
  overwrite = TRUE
)


