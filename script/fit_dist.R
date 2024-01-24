################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(fitdistrplus)
library(terra)


source("R/fit_log_normal.R")
source("R/fit_gev.R")


################################################################################
## STEP 1: FIT DISTRIBUTIONS FOR HINDCASTS AND SCENARIOS CMIP5
################################################################################
# 30502.36 sec elapsed OR 9 HRS
tictoc::tic()
# ============================================================================#
# Historical
# ============================================================================#
vic_hist <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")

tictoc::tic() # 265.6 sec elapsed- 5 mins
vic_hist_lnorm <- terra::app(vic_hist, fit_log_normal, cores = 10)
vic_hist_lnorm <- exp(vic_hist_lnorm)
names(vic_hist_lnorm) <- c("mean", "sd")
tictoc::toc()

tictoc::tic() # 30 mins
vic_hist_gev <- terra::app(vic_hist, fit_gev, cores = 10)
names(vic_hist_gev) <- c("location", "scale", "xi")
tictoc::toc()

terra::writeRaster(vic_hist_lnorm,
  filename = "E:/data-raw/dist_fit_vic/vic_hist_lnorm.tif",
  overwrite = TRUE
)

terra::writeRaster(vic_hist_gev,
  filename = "E:/data-raw/dist_fit_vic/vic_hist_gev.tif",
  overwrite = TRUE
)

vic_hist_lnorm <- rast("E:/data-raw/dist_fit_vic/vic_hist_lnorm.tif")
vic_hist_gev <- rast("E:/data-raw/dist_fit_vic/vic_hist_gev.tif")


plot(vic_hist_lnorm)
plot(vic_hist_lnorm[[1]], breaks = c(0, 100, 200, 500, 1000, 5000, 7000, 10000))
plot(vic_hist_lnorm[[2]], breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 60))


plot(vic_hist_gev)
plot(vic_hist_gev <= 0)
plot(vic_hist_gev[[1]], breaks = c(0, 100, 200, 500, 1000, 5000))
plot(vic_hist_gev[[2]], breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 60, 100, 500, 1000))
plot(vic_hist_gev[[3]], breaks = c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1))
# ============================================================================#
# Future RCP45
# ============================================================================#

vic_r45 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp45_access10.tif")

# Define the time periods
time_periods <- list(
  c(2007, 2056),
  c(2017, 2066),
  c(2027, 2076),
  c(2037, 2086),
  c(2047, 2096),
  c(2051, 2100)
)

# Define the directory to save the results
output_dir <- "E:/data-raw/dist_fit_vic"
dir.create(output_dir, recursive = TRUE)

# Loop over each time period
for (period in time_periods) {
  start_year <- period[1]
  end_year <- period[2]

  # Subset the raster
  vic_future <- subset(vic_r45, time(vic_r45) >= start_year & time(vic_r45)
  <= end_year)

  # Fit the log-normal distribution
  vic_future_lnorm <- terra::app(vic_future, fit_log_normal, cores = 10)
  vic_future_lnorm <- exp(vic_future_lnorm)
  names(vic_future_lnorm) <- c("mean", "sd")

  # Save the log-normal fit
  lnorm_file <- file.path(output_dir, paste0(
    "lnorm_fit_r45_", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_lnorm, lnorm_file, overwrite = TRUE)




  # Fit the GEV distribution
  vic_future_gev <- terra::app(vic_future, fit_gev, cores = 10)
  names(vic_future_gev) <- c("location", "scale", "xi")

  # Save the GEV fit
  gev_file <- file.path(output_dir, paste0(
    "gev_fit_r45_", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_gev, gev_file, overwrite = TRUE)
}







# ============================================================================#
# Future RCP85
# ============================================================================#

vic_r85 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp85_access10.tif")

# Define the time periods
time_periods <- list(
  c(2007, 2056),
  c(2017, 2066),
  c(2027, 2076),
  c(2037, 2086),
  c(2047, 2096),
  c(2051, 2100)
)

# Define the directory to save the results
output_dir <- "E:/data-raw/dist_fit_vic"
dir.create(output_dir, recursive = TRUE)

# Loop over each time period
for (period in time_periods) {
  start_year <- period[1]
  end_year <- period[2]

  # Subset the raster
  vic_future <- subset(vic_r85, time(vic_r85) >= start_year & time(vic_r85)
  <= end_year)

  # Fit the log-normal distribution
  vic_future_lnorm <- terra::app(vic_future, fit_log_normal, cores = 10)
  vic_future_lnorm <- exp(vic_future_lnorm)
  names(vic_future_lnorm) <- c("mean", "sd")

  # Save the log-normal fit
  lnorm_file <- file.path(output_dir, paste0(
    "lnorm_fit_r85_", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_lnorm, lnorm_file, overwrite = TRUE)




  # Fit the GEV distribution
  vic_future_gev <- terra::app(vic_future, fit_gev, cores = 10)
  names(vic_future_gev) <- c("location", "scale", "xi")

  # Save the GEV fit
  gev_file <- file.path(output_dir, paste0(
    "gev_fit_r85_", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_gev, gev_file, overwrite = TRUE)
}
tictoc::toc()





################################################################################
## STEP 2: DIST FITTING ANALYSIS
################################################################################


# ============================================================================#
# load data
# ============================================================================#

# Define the path
path <- "E:/data-raw/dist_fit_vic"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = "\\.tif$",
  full.names = TRUE
)


# Iterate over the file list and load each file as a separate variable
for (file in file_list) {
  # Extract the base name of the file (without path and extension)
  base_name <- tools::file_path_sans_ext(basename(file))

  # Read the TIFF file as a raster object and assign it to a variable with the same name as the base_name
  assign(base_name, terra::rast(file))
}


# ============================================================================#
# create a mask
# ============================================================================#
prism <- terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]
prism_mask <- project(prism, gev_fit_r45_2007_2056[[1]])


# ============================================================================#
# Mask the layers for US
# ============================================================================#
for (file in file_list) {
  base_name <- tools::file_path_sans_ext(basename(file))
  raster_data <- get(base_name)
  masked_data <- terra::mask(raster_data, prism_mask)
  assign(base_name, masked_data)
}


# ============================================================================#
# example plots
# ============================================================================#

plot(lnorm_fit_r85_2051_2100)
plot(lnorm_fit_r85_2051_2100[[1]], breaks = c(0, 100, 200, 500, 1000, 5000, 7000, 10000, 30000))
plot(lnorm_fit_r85_2051_2100[[2]], breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 60))


plot(gev_fit_r45_2007_2056)
plot(gev_fit_r45_2007_2056 <= 0)
plot(gev_fit_r45_2007_2056[[1]], breaks = c(0, 100, 200, 500, 1000, 5000))
plot(gev_fit_r45_2007_2056[[2]], breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 60, 100, 500, 1000))
plot(gev_fit_r45_2007_2056[[3]], breaks = c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1))
