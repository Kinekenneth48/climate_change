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
# Historical-event
# ============================================================================#
vic_hist <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")

tictoc::tic() # 265.6 sec elapsed- 5 mins
vic_hist_lnorm_event <- terra::app(vic_hist, fit_log_normal_event, cores = 10)
tictoc::toc()

tictoc::tic() # 30 mins
vic_hist_gev_event <- terra::app(vic_hist, fit_gev_event, cores = 10)
tictoc::toc()

terra::writeRaster(vic_hist_lnorm_event,
  filename = "E:/data-raw/dist_fit_vic/vic_hist_lnorm_event.tif",
  overwrite = TRUE
)

terra::writeRaster(vic_hist_gev_event,
  filename = "E:/data-raw/dist_fit_vic/vic_hist_gev_event.tif",
  overwrite = TRUE
)


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
# Future RCP45 -EVENT
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
  vic_future_lnorm_event <- terra::app(vic_future, fit_log_normal_event, cores = 10)

  # Save the log-normal fit
  lnorm_file <- file.path(output_dir, paste0(
    "lnorm_fit_r45_event_", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_lnorm_event, lnorm_file, overwrite = TRUE)




  # Fit the GEV distribution
  vic_future_gev_event <- terra::app(vic_future, fit_gev_event, cores = 10)


  # Save the GEV fit
  gev_file <- file.path(output_dir, paste0(
    "gev_fit_r45_event_", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_gev_event, gev_file, overwrite = TRUE)
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






# ============================================================================#
# Future RCP85 -event
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
  vic_future_lnorm_event <- terra::app(vic_future, fit_log_normal_event, cores = 10)

  # Save the log-normal fit
  lnorm_file <- file.path(output_dir, paste0(
    "lnorm_fit_r85_event_", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_lnorm_event, lnorm_file, overwrite = TRUE)




  # Fit the GEV distribution
  vic_future_gev_event <- terra::app(vic_future, fit_gev_event, cores = 10)

  # Save the GEV fit
  gev_file <- file.path(output_dir, paste0(
    "gev_fit_r85_event_", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_gev_event, gev_file, overwrite = TRUE)
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
# Mask the layers for US boundaries
# ============================================================================#
for (file in file_list) {
  base_name <- tools::file_path_sans_ext(basename(file))
  raster_data <- get(base_name)
  masked_data <- terra::mask(raster_data, prism_mask)
  assign(base_name, masked_data)
}


# ============================================================================#
# Example plots of distr. parameters
# ============================================================================#

plot(lnorm_fit_r85_2051_2100)
plot(lnorm_fit_r85_2051_2100[[1]], breaks = c(0, 100, 200, 500, 1000, 5000, 7000, 10000, 30000))
plot(lnorm_fit_r85_2051_2100[[2]], breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 60))


plot(gev_fit_r45_2007_2056)
plot(gev_fit_r45_2007_2056 <= 0)
plot(gev_fit_r45_2007_2056[[1]], breaks = c(0, 100, 200, 500, 1000, 5000, 10000, 30000, 300000))
plot(gev_fit_r45_2007_2056[[2]], breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 60, 100, 500, 1000, 30000))
plot(gev_fit_r45_2007_2056[[3]], breaks = c(-30, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 5))



# ============================================================================#
# Take an average of the dist. parameters across overlapping scenarios
# ============================================================================#
# take mean across overlapping scenarios -LNORM
lnorm_fit_r45 <- mean(lnorm_fit_r45_2007_2056, lnorm_fit_r45_2017_2066,
  lnorm_fit_r45_2027_2076,
  lnorm_fit_r45_2037_2086, lnorm_fit_r45_2047_2096, lnorm_fit_r45_2051_2100,
  na.rm = TRUE
)

# plot LNORM dist parameters
plot(lnorm_fit_r45[[1]], breaks = c(0, 100, 200, 500, 1000, 5000, 7000, 10000, 30000))
plot(lnorm_fit_r45[[2]], breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 60))



# take mean across overlapping scenarios -LNORM
lnorm_fit_r85 <- mean(lnorm_fit_r85_2007_2056, lnorm_fit_r85_2017_2066,
  lnorm_fit_r85_2027_2076,
  lnorm_fit_r85_2037_2086, lnorm_fit_r85_2047_2096, lnorm_fit_r85_2051_2100,
  na.rm = TRUE
)

# plot LNORM dist parameters
plot(lnorm_fit_r85[[1]], breaks = c(
  0, 100, 200, 500, 1000, 5000, 7000, 10000,
  30000
))
plot(lnorm_fit_r85[[2]], breaks = c(
  0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50,
  60
))




# take mean across overlapping scenarios -GEV
gev_fit_r45 <- mean(gev_fit_r45_2007_2056, gev_fit_r45_2017_2066,
  gev_fit_r45_2027_2076,
  gev_fit_r45_2037_2086, gev_fit_r45_2047_2096, gev_fit_r45_2051_2100,
  na.rm = TRUE
)

# plot GEV dist parameters
plot(gev_fit_r45[[1]], breaks = c(
  0, 100, 200, 500, 1000, 5000, 10000, 30000,
  300000
))
plot(gev_fit_r45[[2]], breaks = c(
  0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50,
  60, 100, 500, 1000, 30000
))
plot(gev_fit_r45[[3]], breaks = c(-30, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 5))




# take mean across overlapping scenarios -GEV
gev_fit_r85 <- mean(gev_fit_r85_2007_2056, gev_fit_r85_2017_2066,
  gev_fit_r85_2027_2076,
  gev_fit_r85_2037_2086, gev_fit_r85_2047_2096, gev_fit_r85_2051_2100,
  na.rm = TRUE
)

# plot GEV dist parameters
plot(gev_fit_r85[[1]], breaks = c(
  0, 100, 200, 500, 1000, 5000, 10000, 30000,
  300000
))
plot(gev_fit_r85[[2]], breaks = c(
  0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50,
  60, 100, 500, 1000, 30000
))
plot(gev_fit_r85[[3]], breaks = c(-30, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 5))





# ============================================================================#
# Take an average of the 50 YEAR EVENT across overlapping scenarios
# ============================================================================#

# AVERAGE 50 YEAR FOR LNORM
lnorm_fit_r45_event <- mean(lnorm_fit_r45_event_2007_2056,
  lnorm_fit_r45_event_2017_2066,
  lnorm_fit_r45_event_2027_2076,
  lnorm_fit_r45_event_2037_2086, lnorm_fit_r45_event_2047_2096,
  lnorm_fit_r45_event_2051_2100,
  na.rm = TRUE
)

lnorm_fit_r85_event <- mean(lnorm_fit_r85_event_2007_2056,
  lnorm_fit_r85_event_2017_2066,
  lnorm_fit_r85_event_2027_2076,
  lnorm_fit_r85_event_2037_2086, lnorm_fit_r85_event_2047_2096,
  lnorm_fit_r85_event_2051_2100,
  na.rm = TRUE
)



# AVERAGE 50 YEAR FOR GEV
gev_fit_r45_event <- mean(gev_fit_r45_event_2007_2056,
  gev_fit_r45_event_2017_2066,
  gev_fit_r45_event_2027_2076,
  gev_fit_r45_event_2037_2086, gev_fit_r45_event_2047_2096,
  gev_fit_r45_event_2051_2100,
  na.rm = TRUE
)

gev_fit_r85_event <- mean(gev_fit_r85_event_2007_2056, gev_fit_r85_event_2017_2066,
  gev_fit_r85_event_2027_2076,
  gev_fit_r85_event_2037_2086, gev_fit_r85_event_2047_2096,
  gev_fit_r85_event_2051_2100,
  na.rm = TRUE
)



# ============================================================================#
# Percentage difference between hindcasts and scenarios distr parameters
# ============================================================================#
lnorm_fit_r45_diff_per <- ((lnorm_fit_r45 - vic_hist_lnorm) / vic_hist_lnorm) * 100
lnorm_fit_r85_diff_per <- ((lnorm_fit_r85 - vic_hist_lnorm) / vic_hist_lnorm) * 100


plot(lnorm_fit_r45_diff_per[[1]],
  main = "Mean: lnorm r45 diff percenatge (positive for r45)",
  breaks = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100, 1000, 2000, 140000000000)
)
plot(lnorm_fit_r45_diff_per[[2]],
  main = "SD: lnorm r45 diff percenatge (positive for r45)",
  breaks = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100, 500, 1000)
)

plot(lnorm_fit_r85_diff_per[[1]],
  main = "Mean: lnorm r85 diff percenatge (positive for r85)",
  breaks = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100, 1000, 2000, 140000000000)
)
plot(lnorm_fit_r85_diff_per[[2]],
  main = "SD: lnorm r85 diff percenatge (positive for r85)",
  breaks = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100, 500, 1000)
)



gev_fit_r45_diff_per <- ((gev_fit_r45 - vic_hist_gev) / vic_hist_gev) * 100
gev_fit_r85_diff_per <- ((gev_fit_r85 - vic_hist_gev) / vic_hist_gev) * 100

plot(gev_fit_r45_diff_per[[1]],
  main = "LOC: GEV r45 diff percenatge (positive for r45)",
  breaks = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100, 1000, 2000, 140000000000)
)

plot(gev_fit_r45_diff_per[[2]],
  main = "Scale: GEV r45 diff percenatge (positive for r45)",
  breaks = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100, 500, 1000)
)

plot(gev_fit_r45_diff_per[[3]],
  main = "Shape: GEV r45 diff percenatge (positive for r45)",
  breaks = c(-3000000, -30, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 5, 3000000)
)


plot(gev_fit_r85_diff_per[[1]],
  main = "LOC: GEV r85 diff percenatge (positive for r85)",
  breaks = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100, 1000, 2000, 140000000000)
)

plot(gev_fit_r85_diff_per[[2]],
  main = "Scale:GEV r85 diff percenatge (positive for r85)",
  breaks = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100, 500, 1000)
)

plot(gev_fit_r85_diff_per[[3]],
  main = "Shape: V r85 diff percenatge (positive for r85)",
  breaks = c(-3000000, -30, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 5, 3000000)
)







# ============================================================================#
# Percentage difference between hindcasts and scenarios - 50 year event
# ============================================================================#
# take diff percent
lnorm_fit_r45_diff_per_event <- ((lnorm_fit_r45_event - vic_hist_lnorm_event)
/ vic_hist_lnorm_event) * 100

lnorm_fit_r85_diff_per_event <- ((lnorm_fit_r85_event - vic_hist_lnorm_event)
/ vic_hist_lnorm_event) * 100

gev_fit_r45_diff_per_event <- ((gev_fit_r45_event - vic_hist_gev_event) /
  vic_hist_gev_event) * 100

gev_fit_r85_diff_per_event <- ((gev_fit_r85_event - vic_hist_gev_event) /
  vic_hist_gev_event) * 100


# load USA boundaries shapefile
usa_boundary <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))
usa_boundary <- terra::vect(usa_boundary)
usa_boundary <- terra::project(usa_boundary, lnorm_fit_r45_diff_per_event)


# make plots
plot(lnorm_fit_r45_diff_per_event,
  main = "LNORM: 50 year event -R45",
  breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100, 300, 500),
  col = c(
    "#40004b", "#762a83", "#9970ab", "#c2a5cf",
    "#ccece6", "#99d8c9", "#66c2a4", "#41ae76", "#238b45",
    "#006d2c", "#00441b"
  )
)
plot(usa_boundary, add = T)

plot(lnorm_fit_r85_diff_per_event,
  main = "LNORM: 50 year event -R85",
  breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100, 300, 500),
  col = c(
    "#40004b", "#762a83", "#9970ab", "#c2a5cf",
    "#ccece6", "#99d8c9", "#66c2a4", "#41ae76", "#238b45",
    "#006d2c", "#00441b"
  )
)
plot(usa_boundary, add = T)




# make plots
plot(gev_fit_r45_diff_per_event,
  main = "GEV: 50 year event -R45",
  breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100, 300, 500, 1000, 104022036),
  col = c(
    "#40004b", "#762a83", "#9970ab", "#c2a5cf",
    "#f7f7f7", "#99d8c9", "#66c2a4", "#41ae76", "#238b45",
    "#006d2c", "#00441b", "#00441b", "black"
  )
)
plot(usa_boundary, add = T)

plot(gev_fit_r85_diff_per_event,
  main = "GEV: 50 year event -R85",
  breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100, 300, 500, 1000, 104022036),
  col = c(
    "#40004b", "#762a83", "#9970ab", "#c2a5cf",
    "#f7f7f7", "#99d8c9", "#66c2a4", "#41ae76", "#238b45",
    "#006d2c", "#00441b", "#00441b", "black"
  )
)
plot(usa_boundary, add = T)
