################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(fitdistrplus)
library(terra)

source("R/fit_log_normal_less.R")
source("R/fit_log_normal.R")
source("R/fit_gev.R")


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

# ============================================================================#
# load the data
# ============================================================================#
vic_hist <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")
vic_r45 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp45_access10.tif")
vic_r85 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp85_access10.tif")

################################################################################
## STEP 1: CREATE BLOCKS
################################################################################


# Define the time periods
time_periods <- list(
  c(2007, 2057),
  c(2025, 2075),
  c(2050, 2100)
)

# Define the directory to save the results
output_dir <- "E:/data-raw/dist_fit_vic"
dir.create(output_dir, recursive = TRUE)

# Loop over each time period
for (period in time_periods) {
  start_year <- period[1]
  end_year <- period[2]
  
  # Subset the raster
  vic_r85 <- subset(vic_r85, time(vic_r85) >= start_year & time(vic_r85)
                       <= end_year)
  vic_r45 <- subset(vic_r45, time(vic_r45) >= start_year & time(vic_r45)
                       <= end_year)
  
  # Fit the log-normal distribution
  vic_future_lnorm_r85 <- terra::app(vic_r85, fit_log_normal, cores = 10)
  vic_future_lnorm_r85 <- exp(vic_future_lnorm_r85)
  names(vic_future_lnorm_r85) <- c("mean", "sd")
  
  # Save the log-normal fit
  lnorm_file <- file.path(output_dir, paste0(
    "lnorm_fit_r85_", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_lnorm_r85, lnorm_file, overwrite = TRUE)
  
  # Fit the log-normal distribution
  vic_future_lnorm_r45 <- terra::app(vic_r45, fit_log_normal, cores = 10)
  vic_future_lnorm_r45 <- exp(vic_future_lnorm_r45)
  names(vic_future_lnorm_r45) <- c("mean", "sd")
  
  # Save the log-normal fit
  lnorm_file <- file.path(output_dir, paste0(
    "lnorm_fit_r45_", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_lnorm_r45, lnorm_file, overwrite = TRUE)
  
  
  
  
  
  # Fit the GEV distribution
  vic_future_gev_r45 <- terra::app(vic_r45, fit_gev, cores = 10)
  names(vic_future_gev_r45) <- c("location", "scale", "xi")
  
  # Save the GEV fit
  gev_file <- file.path(output_dir, paste0(
    "gev_fit_r45_", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_gev_r45, gev_file, overwrite = TRUE)
  
  # Fit the GEV distribution
  vic_future_gev_r85 <- terra::app(vic_r85, fit_gev, cores = 10)
  names(vic_future_gev_r85) <- c("location", "scale", "xi")
  
  # Save the GEV fit
  gev_file <- file.path(output_dir, paste0(
    "gev_fit_r85_", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_gev_r85, gev_file, overwrite = TRUE)
}





################################################################################
## STEP2: FIT DISTRIBUTIONS (n=55)
################################################################################


gev_fit_r45_2007_2057 = rast("E:/data-raw/dist_fit_vic/gev_fit_r45_2007_2057.tif")
gev_fit_r45_2025_2075 = rast("E:/data-raw/dist_fit_vic/gev_fit_r45_2025_2075.tif")
gev_fit_r45_2050_2100 = rast("E:/data-raw/dist_fit_vic/gev_fit_r45_2050_2100.tif")

gev_fit_r85_2007_2057 = rast("E:/data-raw/dist_fit_vic/gev_fit_r85_2007_2057.tif")
gev_fit_r85_2025_2075 = rast("E:/data-raw/dist_fit_vic/gev_fit_r85_2025_2075.tif")
gev_fit_r85_2050_2100 = rast("E:/data-raw/dist_fit_vic/gev_fit_r85_2050_2100.tif")


lnorm_fit_r45_2007_2057 = rast("E:/data-raw/dist_fit_vic/lnorm_fit_r45_2007_2057.tif")
lnorm_fit_r45_2025_2075 = rast("E:/data-raw/dist_fit_vic/lnorm_fit_r45_2025_2075.tif")
lnorm_fit_r45_2050_2100 = rast("E:/data-raw/dist_fit_vic/lnorm_fit_r45_2050_2100.tif")

lnorm_fit_r85_2007_2057 = rast("E:/data-raw/dist_fit_vic/lnorm_fit_r85_2007_2057.tif")
lnorm_fit_r85_2025_2075 = rast("E:/data-raw/dist_fit_vic/lnorm_fit_r85_2025_2075.tif")
lnorm_fit_r85_2050_2100 = rast("E:/data-raw/dist_fit_vic/lnorm_fit_r85_2050_2100.tif")

# ============================================================================#
# fit distr.
# ============================================================================#
tictoc::tic()
vic_hist_lnorm_full <- terra::app(vic_hist, fit_log_normal, cores = 10)
vic_hist_lnorm_full <- exp(vic_hist_lnorm_full)
names(vic_hist_lnorm_full) <- c("mean", "sd")
tictoc::toc()

tictoc::tic()
vic_hist_gev_full <- terra::app(vic_hist, fit_gev, cores = 10)
names(vic_hist_gev_full) <- c("location", "scale", "shape")
tictoc::toc()


# ============================================================================#
# create a mask
# ============================================================================#
prism <- terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]
prism_mask <- project(prism, vic_hist_lnorm_full[[1]])

vic_hist_lnorm_full <- terra::mask(vic_hist_lnorm_full, prism_mask)
vic_hist_gev_full <- terra::mask(vic_hist_gev_full, prism_mask)

gev_fit_r45_2007_2057 <- terra::mask(gev_fit_r45_2007_2057, prism_mask)
gev_fit_r45_2025_2075 <- terra::mask(gev_fit_r45_2025_2075, prism_mask)
gev_fit_r45_2050_2100 <- terra::mask(gev_fit_r45_2050_2100, prism_mask)
gev_fit_r85_2007_2057 <- terra::mask(gev_fit_r85_2007_2057, prism_mask)
gev_fit_r85_2025_2075 <- terra::mask(gev_fit_r85_2025_2075, prism_mask)
gev_fit_r85_2050_2100 <- terra::mask(gev_fit_r85_2050_2100, prism_mask)

lnorm_fit_r45_2007_2057 <- terra::mask(lnorm_fit_r45_2007_2057, prism_mask)
lnorm_fit_r45_2025_2075 <- terra::mask(lnorm_fit_r45_2025_2075, prism_mask)
lnorm_fit_r45_2050_2100 <- terra::mask(lnorm_fit_r45_2050_2100, prism_mask)
lnorm_fit_r85_2007_2057 <- terra::mask(lnorm_fit_r85_2007_2057, prism_mask)
lnorm_fit_r85_2025_2075 <- terra::mask(lnorm_fit_r85_2025_2075, prism_mask)
lnorm_fit_r85_2050_2100 <- terra::mask(lnorm_fit_r85_2050_2100, prism_mask)



################################################################################
## STEP3: plot dist fit parameters lnorm/gev (2007_2057)
################################################################################
# ============================================================================#
# plot dist fit parameters lnorm (2007_2057)
# ============================================================================#
# mean parameters
# Set up a 1x2 layout for side-by-side plots
par(mfcol = c(3, 2))


plot(vic_hist_lnorm_full[[1]],
     main = "Mean: lnorm of VIC(hist): n=55",
     breaks = c(0,10,20,30,40, 50, 60,70,80,90,100,  500, 1000,  2000, 2500)
)

# Plot vic_hist_lnorm[[1]]
plot(lnorm_fit_r45_2007_2057[[1]],
     main = "Mean: lnorm of VIC(r45-2007_2057): n=50",
     breaks = c(0,10,20,30,40, 50, 60,70,80,90,100,  500, 1000, 2000, 2500),
     legend = FALSE
)

plot(lnorm_fit_r85_2007_2057[[1]],
     main = "Mean: lnorm of VIC(r85-2007_2057): n=50",
     breaks = c(0,10,20,30,40, 50, 60,70,80,90,100,  500, 1000,  2000, 2500),
     legend = FALSE
)




plot(vic_hist_lnorm_full[[2]],
     main = "SD: lnorm of VIC(hist): n=55",
     breaks = c(0, 2,4,6,8 ,10, 15, 20,25,  30, 40, 50, 70)
)

plot(lnorm_fit_r45_2007_2057[[2]],
     main = "SD: lnorm of VIC(r45-2007_2057): n=50",
     breaks = c(0, 2,4,6,8 ,10, 15, 20,25,  30, 40, 50, 70),
     legend = FALSE
)

plot(lnorm_fit_r85_2007_2057[[2]],
     main = "SD: lnorm of VIC(r85-2007_2057): n=50",
     breaks = c(0, 2,4,6,8 ,10, 15, 20,25,  30, 40, 50, 70),
     legend = FALSE
)

# Reset the layout to the default (1 plot per page)
par(mfrow = c(1, 1))




# ============================================================================#
# plot dist fit parameters GEV
# ============================================================================#
# mean parameters
# Set up a 1x2 layout for side-by-side plots
par(mfcol = c(3, 3))

plot(vic_hist_gev_full[[1]],
     main = "location: GEV of UA(hist): n=55",
     breaks = c(0,5,10,20,30,40, 50, 60,70,80,90,100,  1000, 80000)
)

plot(gev_fit_r45_2007_2057[[1]],
     main = "location: GEV of VIC(r45): n=50",
     breaks = c(0,5,10,20,30,40, 50, 60,70,80,90,100,  1000, 80000),
     legend = FALSE
)


plot(gev_fit_r85_2007_2057[[1]],
     main = "location: GEV of VIC(r85): n=50",
     breaks = c(0,5,10,20,30,40, 50, 60,70,80,90,100,  1000, 80000),
     legend = FALSE
)





# sd parameters
plot(vic_hist_gev_full[[2]],
     main = "SD: GEV of UA(hist): n=55",
     breaks = c(0, 5 ,10, 15, 20,25,  30, 40, 50, 70, 100,50000)
)

plot(gev_fit_r45_2007_2057[[2]],
     main = "SD: GEV of VIC(r45): n=50",
     breaks = c(0, 5 ,10, 15, 20,25,  30, 40, 50, 70, 100,50000),
     legend = FALSE
)

plot(gev_fit_r85_2007_2057[[2]],
     main = "SD: GEV of VIC(r85): n=50",
     breaks = c(0, 5 ,10, 15, 20,25,  30, 40, 50, 70, 100,50000),
     legend = FALSE
)

# Reset the layout to the default (1 plot per page)
#par(mfrow = c(1, 1))



# shape parameters
# Set up a 1x2 layout for side-by-side plots
#par(mfrow = c(3, 1))
plot(vic_hist_gev_full[[3]],
     main = "SHAPE: GEV of UA: n=55",
     breaks = c(-3,  -0.5, -0.25, 0,  0.25,0.5,  3),
)

plot(gev_fit_r45_2007_2057[[3]],
     main = "SHAPE: GEV of VIC: n=94",
     breaks = c(-3,  -0.5, -0.25, 0,  0.25,0.5,  3),
     legend = FALSE
)

plot(gev_fit_r85_2007_2057[[3]],
     main = "SHAPE: GEV of VIC: n=94",
     breaks = c(-3,  -0.5, -0.25, 0,  0.25,0.5,  3),
     legend = FALSE
)


# Reset the layout to the default (1 plot per page)
par(mfrow = c(1, 1))



################################################################################
## STEP4: plot dist fit parameters lnorm/gev (2025_2075)
################################################################################
# ============================================================================#
# plot dist fit parameters lnorm (2025_2075)
# ============================================================================#
# mean parameters
# Set up a 1x2 layout for side-by-side plots
par(mfcol = c(3, 2))


plot(vic_hist_lnorm_full[[1]],
     main = "Mean: lnorm of VIC(hist): n=55",
     breaks = c(0,10,20,30,40, 50, 60,70,80,90,100,  500, 1000,  2000, 2500)
)

# Plot vic_hist_lnorm[[1]]
plot(lnorm_fit_r45_2025_2075[[1]],
     main = "Mean: lnorm of VIC(r45-2007_2057): n=50",
     breaks = c(0,10,20,30,40, 50, 60,70,80,90,100,  500, 1000, 2000, 2500),
     legend = FALSE
)

plot(lnorm_fit_r85_2025_2075[[1]],
     main = "Mean: lnorm of VIC(r85-2007_2057): n=50",
     breaks = c(0,10,20,30,40, 50, 60,70,80,90,100,  500, 1000,  2000, 2500),
     legend = FALSE
)




plot(vic_hist_lnorm_full[[2]],
     main = "SD: lnorm of VIC(hist): n=55",
     breaks = c(0, 2,4,6,8 ,10, 15, 20,25,  30, 40, 50, 70)
)

plot(lnorm_fit_r45_2025_2075[[2]],
     main = "SD: lnorm of VIC(r45-2007_2057): n=50",
     breaks = c(0, 2,4,6,8 ,10, 15, 20,25,  30, 40, 50, 70),
     legend = FALSE
)

plot(lnorm_fit_r85_2025_2075[[2]],
     main = "SD: lnorm of VIC(r85-2007_2057): n=50",
     breaks = c(0, 2,4,6,8 ,10, 15, 20,25,  30, 40, 50, 70),
     legend = FALSE
)

# Reset the layout to the default (1 plot per page)
par(mfrow = c(1, 1))




# ============================================================================#
# plot dist fit parameters GEV
# ============================================================================#
# mean parameters
# Set up a 1x2 layout for side-by-side plots
par(mfcol = c(3, 3))

plot(vic_hist_gev_full[[1]],
     main = "location: GEV of UA(hist): n=55",
     breaks = c(0,5,10,20,30,40, 50, 60,70,80,90,100,  1000, 80000)
)

plot(gev_fit_r45_2025_2075[[1]],
     main = "location: GEV of VIC(r45-2025_2075): n=50",
     breaks = c(0,5,10,20,30,40, 50, 60,70,80,90,100,  1000, 80000),
     legend = FALSE
)


plot(gev_fit_r85_2025_2075[[1]],
     main = "location: GEV of VIC(r85-2025_2075): n=50",
     breaks = c(0,5,10,20,30,40, 50, 60,70,80,90,100,  1000, 80000),
     legend = FALSE
)





# sd parameters
plot(vic_hist_gev_full[[2]],
     main = "SD: GEV of UA(hist): n=55",
     breaks = c(0, 5 ,10, 15, 20,25,  30, 40, 50, 70, 100,50000)
)

plot(gev_fit_r45_2025_2075[[2]],
     main = "SD: GEV of VIC(r45-2025_2075): n=50",
     breaks = c(0, 5 ,10, 15, 20,25,  30, 40, 50, 70, 100,50000),
     legend = FALSE
)

plot(gev_fit_r85_2025_2075[[2]],
     main = "SD: GEV of VIC(r85-2025_2075): n=50",
     breaks = c(0, 5 ,10, 15, 20,25,  30, 40, 50, 70, 100,50000),
     legend = FALSE
)

# Reset the layout to the default (1 plot per page)
#par(mfrow = c(1, 1))



# shape parameters
# Set up a 1x2 layout for side-by-side plots
#par(mfrow = c(3, 1))
plot(vic_hist_gev_full[[3]],
     main = "SHAPE: GEV of UA: n=55",
     breaks = c(-3,  -0.5, -0.25, 0,  0.25,0.5,  3),
)

plot(gev_fit_r45_2025_2075[[3]],
     main = "SHAPE: GEV of VIC: n=50",
     breaks = c(-3,  -0.5, -0.25, 0,  0.25,0.5,  3),
     legend = FALSE
)

plot(gev_fit_r85_2025_2075[[3]],
     main = "SHAPE: GEV of VIC: n=50",
     breaks = c(-3,  -0.5, -0.25, 0,  0.25,0.5,  3),
     legend = FALSE
)


# Reset the layout to the default (1 plot per page)
par(mfrow = c(1, 1))




################################################################################
## STEP5: plot dist fit parameters lnorm/gev (2050_2100)
################################################################################
# ============================================================================#
# plot dist fit parameters lnorm (2050_2100)
# ============================================================================#
# mean parameters
# Set up a 1x2 layout for side-by-side plots
par(mfcol = c(3, 2))


plot(vic_hist_lnorm_full[[1]],
     main = "Mean: lnorm of VIC(hist): n=55",
     breaks = c(0,10,20,30,40, 50, 60,70,80,90,100,  500, 1000,  2000, 2500)
)

# Plot vic_hist_lnorm[[1]]
plot(lnorm_fit_r45_2050_2100[[1]],
     main = "Mean: lnorm of VIC(r45-2050_2100): n=50",
     breaks = c(0,10,20,30,40, 50, 60,70,80,90,100,  500, 1000, 2000, 2500),
     legend = FALSE
)

plot(lnorm_fit_r85_2050_2100[[1]],
     main = "Mean: lnorm of VIC(r85-2050_2100): n=50",
     breaks = c(0,10,20,30,40, 50, 60,70,80,90,100,  500, 1000,  2000, 2500),
     legend = FALSE
)




plot(vic_hist_lnorm_full[[2]],
     main = "SD: lnorm of VIC(hist): n=55",
     breaks = c(0, 2,4,6,8 ,10, 15, 20,25,  30, 40, 50, 70)
)

plot(lnorm_fit_r45_2050_2100[[2]],
     main = "SD: lnorm of VIC(r45-2050_2100): n=50",
     breaks = c(0, 2,4,6,8 ,10, 15, 20,25,  30, 40, 50, 70),
     legend = FALSE
)

plot(lnorm_fit_r85_2050_2100[[2]],
     main = "SD: lnorm of VIC(r85-2050_2100): n=50",
     breaks = c(0, 2,4,6,8 ,10, 15, 20,25,  30, 40, 50, 70),
     legend = FALSE
)

# Reset the layout to the default (1 plot per page)
par(mfrow = c(1, 1))




# ============================================================================#
# plot dist fit parameters GEV
# ============================================================================#
# mean parameters
# Set up a 1x2 layout for side-by-side plots
par(mfcol = c(3, 3))

plot(vic_hist_gev_full[[1]],
     main = "location: GEV of UA(hist): n=55",
     breaks = c(0,5,10,20,30,40, 50, 60,70,80,90,100,  1000, 80000)
)

plot(gev_fit_r45_2050_2100[[1]],
     main = "location: GEV of VIC(r45-2050_2100): n=50",
     breaks = c(0,5,10,20,30,40, 50, 60,70,80,90,100,  1000, 80000),
     legend = FALSE
)


plot(gev_fit_r85_2050_2100[[1]],
     main = "location: GEV of VIC(r85-2050_2100): n=50",
     breaks = c(0,5,10,20,30,40, 50, 60,70,80,90,100,  1000, 80000),
     legend = FALSE
)





# sd parameters
plot(vic_hist_gev_full[[2]],
     main = "SD: GEV of UA(hist): n=55",
     breaks = c(0, 5 ,10, 15, 20,25,  30, 40, 50, 70, 100,50000)
)

plot(gev_fit_r45_2050_2100[[2]],
     main = "SD: GEV of VIC(r45-2050_2100): n=50",
     breaks = c(0, 5 ,10, 15, 20,25,  30, 40, 50, 70, 100,50000),
     legend = FALSE
)

plot(gev_fit_r85_2050_2100[[2]],
     main = "SD: GEV of VIC(r85-2050_2100): n=50",
     breaks = c(0, 5 ,10, 15, 20,25,  30, 40, 50, 70, 100,50000),
     legend = FALSE
)

# Reset the layout to the default (1 plot per page)
#par(mfrow = c(1, 1))



# shape parameters
# Set up a 1x2 layout for side-by-side plots
#par(mfrow = c(3, 1))
plot(vic_hist_gev_full[[3]],
     main = "SHAPE: GEV of UA: n=55",
     breaks = c(-3,  -0.5, -0.25, 0,  0.25,0.5,  3),
)

plot(gev_fit_r45_2050_2100[[3]],
     main = "SHAPE: GEV of VIC: n=50",
     breaks = c(-3,  -0.5, -0.25, 0,  0.25,0.5,  3),
     legend = FALSE
)

plot(gev_fit_r85_2050_2100[[3]],
     main = "SHAPE: GEV of VIC: n=50",
     breaks = c(-3,  -0.5, -0.25, 0,  0.25,0.5,  3),
     legend = FALSE
)


# Reset the layout to the default (1 plot per page)
par(mfrow = c(1, 1))
