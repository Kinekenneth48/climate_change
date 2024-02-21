################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(terra)

source("R/find_top_storm_sums_w_index.R")


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.85, verbose = TRUE)

# ============================================================================#
# load data
# ============================================================================#
pr_r1 <- terra::rast("E:/data-raw/loca/ACCESS-CM2/pr/future/day/pr.ACCESS-CM2.ssp585.r1i1p1f1.2015-2044.LOCA_16thdeg_v20220519.nc")
pr_r11 <- terra::rast("E:/data-raw/loca/ACCESS-CM2/pr/future/day/pr.ACCESS-CM2.ssp585.r1i1p1f1.2045-2074.LOCA_16thdeg_v20220519.nc")
pr_r111 <- terra::rast("E:/data-raw/loca/ACCESS-CM2/pr/future/day/pr.ACCESS-CM2.ssp585.r1i1p1f1.2075-2100.LOCA_16thdeg_v20220519.nc")

tmax_r1 <- terra::rast("data-raw/test_r1/tasmax.ACCESS-CM2.ssp585.r1i1p1f1.2015-2044.LOCA_16thdeg_v20220413.nc")
tmax_r11 <- terra::rast("data-raw/test_r1/tasmax.ACCESS-CM2.ssp585.r1i1p1f1.2045-2074.LOCA_16thdeg_v20220413.nc")
tmax_r111 <- terra::rast("data-raw/test_r1/tasmax.ACCESS-CM2.ssp585.r1i1p1f1.2075-2100.LOCA_16thdeg_v20220413.nc")

tmin_r1 <- terra::rast("data-raw/test_r1/tasmin.ACCESS-CM2.ssp585.r1i1p1f1.2015-2044.LOCA_16thdeg_v20220413.nc")
tmin_r11 <- terra::rast("data-raw/test_r1/tasmin.ACCESS-CM2.ssp585.r1i1p1f1.2045-2074.LOCA_16thdeg_v20220413.nc")
tmin_r111 <- terra::rast("data-raw/test_r1/tasmin.ACCESS-CM2.ssp585.r1i1p1f1.2075-2100.LOCA_16thdeg_v20220413.nc")




loca_pr_r1_ssp585_day <- c(pr_r1, pr_r11, pr_r111)
tmax <- c(tmax_r1, tmax_r11, tmax_r111)
tmin <- c(tmin_r1, tmin_r11, tmin_r111)

tictoc::tic()
loca_tmean_r1_ssp585_day <- (tmax + tmin) / 2
tictoc::toc()

tictoc::tic()
writeRaster(loca_pr_r1_ssp585_day, 
            "E:/data-raw/swe_model_vars/ssp585/r1/loca_pr_r1_ssp585_day.tif",
            overwrite=TRUE)
writeRaster(loca_tmean_r1_ssp585_day, "data-raw/test_r1/loca_tmean_r1_ssp585_day.tif")
tictoc::toc()

loca_pr <- rast("E:/data-raw/swe_model_vars/ssp585/r1/loca_pr_r1_ssp585_day.tif")
loca_tas <- rast("E:/data-raw/swe_model_vars/ssp585/r1/loca_tmean_r1_ssp585_day.tif")

# ============================================================================#
# match the dates for both rasters
# ============================================================================#
# Extract time attributes from both rasters
time_tas <- terra::time(loca_tas)
time_pr <- terra::time(loca_pr)

# set temp threshold to classify pr as snow (3 cel is 276.15K)
temp_threshold <- 276.15


################################################################################
## STEP 1: get max annual snow and corresponding variables
################################################################################

# Extracting years and months from the time attributes
years_loca_tas <- as.numeric(format(time(loca_tas), "%Y"))
months_loca_tas <- as.numeric(format(time(loca_tas), "%m"))

# Identify the unique years in your dataset
unique_years <- unique(years_loca_tas)

# Initialize vectors to store the maximum SWE and corresponding temperature
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


tictoc::tic() 
# Loop through each water year (October to September next year)
for (j in 1:(length(unique_years) - 1)) {
  # Update the progress bar
  setTxtProgressBar(pb, j)

  start_year <- unique_years[j]
  end_year <- unique_years[j + 1]

  # Create a mask for the water year (October of start_year to September of end_year)
  water_year_mask <- (years_loca_tas == start_year & months_loca_tas >= 10) |
    (years_loca_tas == end_year & months_loca_tas <= 9)

  # Create a mask for the water year (winter months)
  water_year_mask_winter <- (years_loca_tas == start_year & months_loca_tas >= 12) |
    (years_loca_tas == end_year & months_loca_tas <= 3)

  # Subset the data for water year 
  temp_subset <- loca_tas[[which(water_year_mask)]]
  pr_subset <- loca_pr[[which(water_year_mask)]]

  # Subset the data for water year(winter) 
  temp_subset_winter <- loca_tas[[which(water_year_mask_winter)]]
  pr_subset_winter <- loca_pr[[which(water_year_mask_winter)]]


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
 # max_swe <- max(swe_subset, na.rm = TRUE)

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
  time(mean_temp) = end_year

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
  storm <- app(modified_pr_subset,
    fun = find_top_storm_sums_w_index,
    num_zeros = 3
  )

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
  # and convert ppt from kg m2 s to mm
  temp_diff_list[[j]] <- temp_diff
  mean_temp_list[[j]] <- mean_temp
  total_ppt_list[[j]] <- total_ppt * 86400
  snow_window_list[[j]] <- snow_window * 86400

  storm_one_temp_list[[j]] <- storm_one_temp
  storm_two_temp_list[[j]] <- storm_two_temp
  storm_three_temp_list[[j]] <- storm_three_temp
  storm_one_list[[j]] <- storm_one * 86400
  storm_two_list[[j]] <- storm_two * 86400
  storm_three_list[[j]] <- storm_three * 86400
}
# Close the progress bar
close(pb)

tictoc::toc()



# Convert the lists to SpatRaster (multi-layer raster)
temp_diff_r1_ssp585 <- terra::rast(temp_diff_list)
mean_temp_r1_ssp585 <- terra::rast(mean_temp_list)
total_ppt_r1_ssp585 <- terra::rast(total_ppt_list)
snow_window_r1_ssp585 <- terra::rast(snow_window_list)

storm_one_temp_r1_ssp585 <- terra::rast(storm_one_temp_list)
storm_two_temp_r1_ssp585 <- terra::rast(storm_two_temp_list)
storm_three_temp_r1_ssp585 <- terra::rast(storm_three_temp_list)
storm_one_r1_ssp585 <- terra::rast(storm_one_list)
storm_two_r1_ssp585 <- terra::rast(storm_two_list)
storm_three_r1_ssp585 <- terra::rast(storm_three_list)


# Save the resulting multi-layer raster layers
writeRaster(temp_diff_r1_ssp585,
  "E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/temp_diff_r1_ssp585.tif",
  overwrite = TRUE
)

writeRaster(mean_temp_r1_ssp585,
  "E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/mean_temp_r1_ssp585.tif",
  overwrite = TRUE
)

writeRaster(total_ppt_r1_ssp585,
  "E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/total_ppt_r1_ssp585.tif",
  overwrite = TRUE
)


writeRaster(snow_window_r1_ssp585,
  "E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/snow_window_r1_ssp585.tif",
  overwrite = TRUE
)


writeRaster(storm_one_temp_r1_ssp585,
  "E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/storm_one_temp_r1_ssp585.tif",
  overwrite = TRUE
)

writeRaster(storm_two_temp_r1_ssp585,
  "E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/storm_two_temp_r1_ssp585.tif",
  overwrite = TRUE
)
writeRaster(storm_three_temp_r1_ssp585,
  "E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/storm_three_temp_r1_ssp585.tif",
  overwrite = TRUE
)

writeRaster(storm_one_r1_ssp585,
  "E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/storm_one_r1_ssp585.tif",
  overwrite = TRUE
)

writeRaster(storm_two_r1_ssp585,
  "E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/storm_two_r1_ssp585s.tif",
  overwrite = TRUE
)
writeRaster(storm_three_r1_ssp585,
  "E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/storm_three_r1_ssp585.tif",
  overwrite = TRUE
)





