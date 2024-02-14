################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(terra)

source("R/find_top_storm_sums_w_index.R")


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

# ============================================================================#
# load data
# ============================================================================#

ua_swe <- terra::rast("E:data-raw/ua/raster/combined/ua_swe_combined_daily.tif")

prism_ppt = rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")
prism_tmean = rast("E:/data-raw/prism/raster/prism_day_tmean_raster.tif")

# ============================================================================#
# match the dates for both rasters
# ============================================================================#
# Subset prism_tmean & prism_ppt using the matching indices
prism_tmean <-  subset(prism_tmean, time(prism_tmean) >= "1981-10-01" & time(prism_tmean) <= "2021-09-30")
prism_ppt <- subset(prism_ppt, time(prism_ppt) >= "1981-10-01" & time(prism_ppt) <= "2021-09-30")
ua_swe <- subset(ua_swe, time(ua_swe) >= "1981-10-01" & time(ua_swe) <= "2021-09-30")

# set temp threshold to classify pr as snow (3 cel is 276.15K)
temp_threshold <- 3


################################################################################
## STEP 1: get max annual snow and corresponding variables
################################################################################

# Extracting years and months from the time attributes
years_prism_tas <- as.numeric(format(time(prism_tmean), "%Y"))
months_prism_tas <- as.numeric(format(time(prism_tmean), "%m"))

# Identify the unique years in your dataset
unique_years <- unique(years_prism_tas)

# Initialize vectors to store the maximum SWE and corresponding temperature
max_swe_list <- vector("list", length = length(unique_years) - 1)
temp_diff_list <- vector("list", length = length(unique_years) - 1)
mean_temp_list <- vector("list", length = length(unique_years) - 1)
total_ppt_list <- vector("list", length = length(unique_years) - 1)
snow_window_list <- vector("list", length = length(unique_years) - 1)

storm_one_temp_list <- vector("list", length = length(unique_years) - 1)
storm_two_temp_list <- vector("list", length = length(unique_years) - 1)
storm_three_temp_list <- vector("list", length = length(unique_years) - 1)

storm_one_list <- vector("list", length = length(unique_years) - 1)
storm_two_list <- vector("list", length = length(unique_years) - 1)
storm_three_list <- vector("list", length = length(unique_years) - 1)


# Initialize a progress bar
pb <- txtProgressBar(min = 0, max = length(unique_years) - 1, style = 3)


tictoc::tic() # 7 hrs
# Loop through each water year (October to September next year)
for (j in 11:(length(unique_years) - 1)) {
  # Update the progress bar
  setTxtProgressBar(pb, j)
  
  start_year <- unique_years[j]
  end_year <- unique_years[j + 1]
  
  # Create a mask for the water year (October of start_year to September of end_year)
  water_year_mask <- (years_prism_tas == start_year & months_prism_tas >= 10) |
    (years_prism_tas == end_year & months_prism_tas <= 9)
  
  # Create a mask for the water year (winter months)
  water_year_mask_winter <- (years_prism_tas == start_year & months_prism_tas >= 12) |
    (years_prism_tas == end_year & months_prism_tas <= 3)
  
  # Subset the data for water year and  tmean in C
  # PPT in mm
  swe_subset <- ua_swe[[which(water_year_mask)]]
  temp_subset <- prism_tmean[[which(water_year_mask)]] 
  pr_subset <- prism_ppt[[which(water_year_mask)]]
  
  # Subset the data for water year(winter) 
  temp_subset_winter <- prism_tmean[[which(water_year_mask_winter)]]
  pr_subset_winter <- prism_ppt[[which(water_year_mask_winter)]]
  
  # Check: Ensure that all subsetted rasters have the same number of layers
  if (nlyr(swe_subset) != nlyr(temp_subset) || nlyr(swe_subset) != nlyr(pr_subset)) {
    stop("Mismatch in number of layers for year: ", start_year)
  }
  
  gc()
  gc()
  # if temperature is below the threshold, keep the precipitation value (as snow),
  # else set to zero (15 mins)
  modified_pr_subset <- terra::ifel(temp_subset < temp_threshold, pr_subset, 0)
  modified_pr_subset_winter <- terra::ifel(
    temp_subset_winter < temp_threshold,
    pr_subset_winter, 0
  )
  
  gc()
  # ============================================================================#
  # 1. annual max SWE
  # ============================================================================#
  max_swe <- max(swe_subset, na.rm = TRUE)
  time(max_swe) = end_year
  
  # ============================================================================#
  # 2. winter temp difference
  # ============================================================================#
  temp_diff <- max(temp_subset_winter, na.rm = TRUE) - min(temp_subset_winter,
                                                           na.rm = TRUE
  )
  time(temp_diff) = end_year
  
  # ============================================================================#
  # 3. winter temp mean
  # ============================================================================#
  mean_temp <- mean(temp_subset_winter, na.rm = TRUE)
  time(mean_temp) = end_year
  
  # ============================================================================#
  # 4. get total ppt in winter months
  # ============================================================================#
  total_ppt <- sum(modified_pr_subset_winter, na.rm = TRUE)
  time(total_ppt) = end_year
  
  # ============================================================================#
  # 5. MAX OF 21 SNOW WINDOW
  # ============================================================================#
  snow_window <- max(terra::roll(
    x = modified_pr_subset_winter,
    n = 21, fun = sum, type = "around", na.rm = TRUE
  ))
  time(snow_window) = end_year
  
  # ============================================================================#
  # 6. get top 3 storms by summation
  # num_zeros=3 means two storms separated by three zeros will be one storm
  # ============================================================================#
  storm <- app(modified_pr_subset, fun = find_top_storm_sums_w_index, 
               num_zeros = 3, cores=12)
  
  storm_one <- storm[[1]]
  storm_two <- storm[[4]]
  storm_three <- storm[[7]]
  time(storm_one) = end_year
  time(storm_two) = end_year
  time(storm_three) = end_year
  
  # ============================================================================#
  # 7. get top 3 storms mean temp
  # ============================================================================#
  storm_one_temp <- terra::rapp(
    x = temp_subset, first = storm[[2]],
    last = storm[[3]], fun = "mean"
  )
  
  storm_two_temp <- terra::rapp(
    x = temp_subset, first = storm[[5]],
    last = storm[[6]], fun = "mean"
  )
  
  storm_three_temp <- terra::rapp(
    x = temp_subset, first = storm[[8]],
    last = storm[[9]], fun = "mean"
  )
  
  time(storm_one_temp) = end_year
  time(storm_two_temp) = end_year
  time(storm_three_temp) = end_year
  
  # Store the max SWE and corresponding temp layer in the lists
  # and convert TMEAN to K
  max_swe_list[[j]] <- max_swe
  temp_diff_list[[j]] <- temp_diff + 273.15
  mean_temp_list[[j]] <- mean_temp + 273.15
  total_ppt_list[[j]] <- total_ppt 
  snow_window_list[[j]] <- snow_window 
  
  storm_one_temp_list[[j]] <- storm_one_temp + 273.15
  storm_two_temp_list[[j]] <- storm_two_temp + 273.15
  storm_three_temp_list[[j]] <- storm_three_temp + 273.15
  storm_one_list[[j]] <- storm_one 
  storm_two_list[[j]] <- storm_two 
  storm_three_list[[j]] <- storm_three 
  
}
# Close the progress bar
close(pb)

tictoc::toc()



# Convert the lists to SpatRaster (multi-layer raster)
max_swe_layers_prism <- terra::rast(max_swe_list)
temp_diff_layers_prism <- terra::rast(temp_diff_list)
mean_temp_layers_prism <- terra::rast(mean_temp_list)
total_ppt_layers_prism <- terra::rast(total_ppt_list)
snow_window_layers_prism <- terra::rast(snow_window_list)

storm_one_temp_layers_prism <- terra::rast(storm_one_temp_list)
storm_two_temp_layers_prism <- terra::rast(storm_two_temp_list)
storm_three_temp_layers_prism <- terra::rast(storm_three_temp_list)
storm_one_layers_prism <- terra::rast(storm_one_list)
storm_two_layers_prism <- terra::rast(storm_two_list)
storm_three_layers_prism <- terra::rast(storm_three_list)


# Save the resulting multi-layer raster layers
writeRaster(max_swe_layers_prism,
            "data-raw/swe_model_vars/max_swe_layers_prism.tif",
            overwrite = TRUE
)

writeRaster(temp_diff_layers_prism,
            "data-raw/swe_model_vars/temp_diff_layers_prism.tif",
            overwrite = TRUE
)

writeRaster(mean_temp_layers_prism,
            "data-raw/swe_model_vars/mean_temp_layers_prism.tif",
            overwrite = TRUE
)

writeRaster(total_ppt_layers_prism,
            "data-raw/swe_model_vars/total_ppt_layers_prism.tif",
            overwrite = TRUE
)


writeRaster(snow_window_layers_prism,
            "data-raw/swe_model_vars/snow_window_layers_prism.tif",
            overwrite = TRUE
)


writeRaster(storm_one_temp_layers_prism,
            "data-raw/swe_model_vars/storm_one_temp_layers_prism.tif",
            overwrite = TRUE
)

writeRaster(storm_two_temp_layers_prism,
            "data-raw/swe_model_vars/storm_two_temp_layers_prism.tif",
            overwrite = TRUE
)
writeRaster(storm_three_temp_layers_prism,
            "data-raw/swe_model_vars/storm_three_temp_layers_prism.tif",
            overwrite = TRUE
)

writeRaster(storm_one_layers_prism,
            "data-raw/swe_model_vars/storm_one_layers_prism.tif",
            overwrite = TRUE
)

writeRaster(storm_two_layers_prism,
            "data-raw/swe_model_vars/storm_two_layers_prism.tif",
            overwrite = TRUE
)
writeRaster(storm_three_layers_prism,
            "data-raw/swe_model_vars/storm_three_layers_prism.tif",
            overwrite = TRUE
)
