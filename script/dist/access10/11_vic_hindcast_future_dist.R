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
## STEP 1: FIT DISTRIBUTIONS (n=55)
################################################################################

# ============================================================================#
# fit distr.
# ============================================================================#
tictoc::tic()
vic_hist_lnorm_full <- terra::app(vic_hist, fit_log_normal, cores = 10)
vic_hist_lnorm_full <- exp(vic_hist_lnorm_full)
names(vic_hist_lnorm_full) <- c("mean", "sd")

vic_r45_lnorm <- terra::app(vic_r45, fit_log_normal, cores = 10)
vic_r45_lnorm <- exp(vic_r45_lnorm)
names(vic_r45_lnorm) <- c("mean", "sd")

vic_r85_lnorm <- terra::app(vic_r85, fit_log_normal, cores = 10)
vic_r85_lnorm <- exp(vic_r85_lnorm)
names(vic_r85_lnorm) <- c("mean", "sd")
tictoc::toc()


tictoc::tic()
vic_hist_gev_full <- terra::app(vic_hist, fit_gev, cores = 10)
names(vic_hist_gev_full) <- c("location", "scale", "shape")

vic_r45_gev <- terra::app(vic_r45, fit_gev, cores = 10)
names(vic_r45_gev) <- c("location", "scale", "shape")

vic_r85_gev <- terra::app(vic_r85, fit_gev, cores = 10)
names(vic_r85_gev) <- c("location", "scale", "shape")
tictoc::toc()



# ============================================================================#
# create a mask
# ============================================================================#
prism <- terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]
prism_mask <- project(prism, vic_hist_lnorm_full[[1]])

vic_hist_lnorm_full <- terra::mask(vic_hist_lnorm_full, prism_mask)
vic_r45_lnorm <- terra::mask(vic_r45_lnorm, prism_mask)
vic_r85_lnorm <- terra::mask(vic_r85_lnorm, prism_mask)

vic_hist_gev_full <- terra::mask(vic_hist_gev_full, prism_mask)
vic_r45_gev <- terra::mask(vic_r45_gev, prism_mask)
vic_r85_gev <- terra::mask(vic_r85_gev, prism_mask)




# ============================================================================#
# plot dist fit parameters lnorm
# ============================================================================#
# mean parameters
# Set up a 1x2 layout for side-by-side plots
par(mfcol = c(3, 2))

# Plot ua_hist_lnorm[[1]]
plot(vic_hist_lnorm_full[[1]],
  main = "Mean: lnorm of VIC(hist): n=55",
  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 500, 1000, 2000, 2500)
)

# Plot vic_hist_lnorm[[1]]
plot(vic_r45_lnorm[[1]],
  main = "Mean: lnorm of VIC(r45): n=94",
  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 500, 1000, 2000, 2500),
  legend = FALSE
)

plot(vic_r85_lnorm[[1]],
  main = "Mean: lnorm of VIC(r85): n=94",
  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 500, 1000, 2000, 2500),
  legend = FALSE
)




plot(vic_hist_lnorm_full[[2]],
  main = "SD: lnorm of VIC(hist): n=55",
  breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 70)
)

plot(vic_r45_lnorm[[2]],
  main = "SD: lnorm of VIC(r45): n=94",
  breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 70),
  legend = FALSE
)

plot(vic_r85_lnorm[[2]],
  main = "SD: lnorm of VIC(r85): n=94",
  breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 70),
  legend = FALSE
)

# Reset the layout to the default (1 plot per page)
par(mfrow = c(1, 1))





vic_hist_gev_full <- terra::mask(vic_hist_gev_full, prism_mask)
vic_r45_gev <- terra::mask(vic_r45_gev, prism_mask)
vic_r85_gev <- terra::mask(vic_r85_gev, prism_mask)



# ============================================================================#
# plot dist fit parameters GEV
# ============================================================================#
# mean parameters
# Set up a 1x2 layout for side-by-side plots
par(mfcol = c(3, 3))

plot(vic_hist_gev_full[[1]],
  main = "location: GEV of UA(hist): n=55",
  breaks = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 1000, 80000)
)

plot(vic_r45_gev[[1]],
  main = "location: GEV of VIC(r45): n=94",
  breaks = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 1000, 80000),
  legend = FALSE
)


plot(vic_r85_gev[[1]],
  main = "location: GEV of VIC(r85): n=94",
  breaks = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 1000, 80000),
  legend = FALSE
)





# sd parameters
plot(vic_hist_gev_full[[2]],
  main = "SD: GEV of UA(hist): n=55",
  breaks = c(0, 5, 10, 15, 20, 25, 30, 40, 50, 70, 100, 50000)
)

plot(vic_r45_gev[[2]],
  main = "SD: GEV of VIC(r45): n=94",
  breaks = c(0, 5, 10, 15, 20, 25, 30, 40, 50, 70, 100, 50000),
  legend = FALSE
)

plot(vic_r85_gev[[2]],
  main = "SD: GEV of VIC(r85): n=94",
  breaks = c(0, 5, 10, 15, 20, 25, 30, 40, 50, 70, 100, 50000),
  legend = FALSE
)

# Reset the layout to the default (1 plot per page)
# par(mfrow = c(1, 1))



# shape parameters
# Set up a 1x2 layout for side-by-side plots
# par(mfrow = c(3, 1))
plot(vic_hist_gev_full[[3]],
  main = "SHAPE: GEV of UA: n=55",
  breaks = c(-3, -0.5, -0.25, 0, 0.25, 0.5, 3),
)

plot(vic_r45_gev[[3]],
  main = "SHAPE: GEV of VIC: n=94",
  breaks = c(-3, -0.5, -0.25, 0, 0.25, 0.5, 3),
  legend = FALSE
)

plot(vic_r85_gev[[3]],
  main = "SHAPE: GEV of VIC: n=94",
  breaks = c(-3, -0.5, -0.25, 0, 0.25, 0.5, 3),
  legend = FALSE
)


# Reset the layout to the default (1 plot per page)
par(mfrow = c(1, 1))





################################################################################
## STEP 2: FIT DISTRIBUTIONS full
################################################################################

# ============================================================================#
# fit distr.
# ============================================================================#
tictoc::tic()
vic_hist_lnorm_full <- terra::app(vic_hist, fit_log_normal, cores = 10)
vic_hist_lnorm_full <- exp(vic_hist_lnorm_full)
names(vic_hist_lnorm_full) <- c("mean", "sd")

vic_r45_lnorm_full <- terra::app(c(vic_hist, vic_r45), fit_log_normal, cores = 10)
vic_r45_lnorm_full <- exp(vic_r45_lnorm_full)
names(vic_r45_lnorm_full) <- c("mean", "sd")

vic_r85_lnorm_full <- terra::app(c(vic_hist, vic_r85), fit_log_normal, cores = 10)
vic_r85_lnorm_full <- exp(vic_r85_lnorm_full)
names(vic_r85_lnorm_full) <- c("mean", "sd")
tictoc::toc()


tictoc::tic()
vic_hist_gev_full <- terra::app(vic_hist, fit_gev, cores = 10)
names(vic_hist_gev_full) <- c("location", "scale", "shape")

vic_r45_gev_full <- terra::app(c(vic_hist, vic_r45), fit_gev, cores = 10)
names(vic_r45_gev_full) <- c("location", "scale", "shape")

vic_r85_gev_full <- terra::app(c(vic_hist, vic_r85), fit_gev, cores = 10)
names(vic_r85_gev_full) <- c("location", "scale", "shape")
tictoc::toc()



# ============================================================================#
# create a mask
# ============================================================================#
prism <- terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]
prism_mask <- project(prism, vic_hist_lnorm_full[[1]])

vic_hist_lnorm_full <- terra::mask(vic_hist_lnorm_full, prism_mask)
vic_r45_lnorm_full <- terra::mask(vic_r45_lnorm_full, prism_mask)
vic_r85_lnorm_full <- terra::mask(vic_r85_lnorm_full, prism_mask)

vic_hist_gev_full <- terra::mask(vic_hist_gev_full, prism_mask)
vic_r45_gev_full <- terra::mask(vic_r45_gev_full, prism_mask)
vic_r85_gev_full <- terra::mask(vic_r85_gev_full, prism_mask)




# ============================================================================#
# plot dist fit parameters lnorm
# ============================================================================#
# mean parameters
# Set up a 1x2 layout for side-by-side plots
par(mfcol = c(3, 2))

# Plot ua_hist_lnorm[[1]]
plot(vic_hist_lnorm_full[[1]],
  main = "Mean: lnorm of VIC(hist): n=55",
  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 500, 1000, 2000, 2500)
)

# Plot vic_hist_lnorm[[1]]
plot(vic_r45_lnorm_full[[1]],
  main = "Mean: lnorm of VIC(r45): full",
  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 500, 1000, 2000, 2500),
  legend = FALSE
)

plot(vic_r85_lnorm_full[[1]],
  main = "Mean: lnorm of VIC(r85): full",
  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 500, 1000, 2000, 2500),
  legend = FALSE
)




plot(vic_hist_lnorm_full[[2]],
  main = "SD: lnorm of VIC(hist): n=55",
  breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 70)
)

plot(vic_r45_lnorm_full[[2]],
  main = "SD: lnorm of VIC(r45): full",
  breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 70),
  legend = FALSE
)

plot(vic_r85_lnorm_full[[2]],
  main = "SD: lnorm of VIC(r85): full",
  breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 70),
  legend = FALSE
)

# Reset the layout to the default (1 plot per page)
par(mfrow = c(1, 1))





vic_hist_gev_full <- terra::mask(vic_hist_gev_full, prism_mask)
vic_r45_gev_full <- terra::mask(vic_r45_gev_full, prism_mask)
vic_r85_gev_full <- terra::mask(vic_r85_gev_full, prism_mask)



# ============================================================================#
# plot dist fit parameters GEV
# ============================================================================#
# mean parameters
# Set up a 1x2 layout for side-by-side plots
par(mfcol = c(3, 3))

plot(vic_hist_gev_full[[1]],
  main = "location: GEV of UA(hist): n=55",
  breaks = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 1000, 80000)
)

plot(vic_r45_gev_full[[1]],
  main = "location: GEV of VIC(r45): full",
  breaks = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 1000, 80000),
  legend = FALSE
)


plot(vic_r85_gev_full[[1]],
  main = "location: GEV of VIC(r85): full",
  breaks = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 1000, 80000),
  legend = FALSE
)





# sd parameters
plot(vic_hist_gev_full[[2]],
  main = "SD: GEV of UA(hist): n=55",
  breaks = c(0, 5, 10, 15, 20, 25, 30, 40, 50, 70, 100, 50000)
)

plot(vic_r45_gev_full[[2]],
  main = "SD: GEV of VIC(r45): full",
  breaks = c(0, 5, 10, 15, 20, 25, 30, 40, 50, 70, 100, 50000),
  legend = FALSE
)

plot(vic_r85_gev_full[[2]],
  main = "SD: GEV of VIC(r85): full",
  breaks = c(0, 5, 10, 15, 20, 25, 30, 40, 50, 70, 100, 50000),
  legend = FALSE
)

# Reset the layout to the default (1 plot per page)
# par(mfrow = c(1, 1))



# shape parameters
# Set up a 1x2 layout for side-by-side plots
# par(mfrow = c(3, 1))
plot(vic_hist_gev_full[[3]],
  main = "SHAPE: GEV of UA: n=55",
  breaks = c(-3, -0.5, -0.25, 0, 0.25, 0.5, 3),
)

plot(vic_r45_gev_full[[3]],
  main = "SHAPE: GEV of VIC: full",
  breaks = c(-3, -0.5, -0.25, 0, 0.25, 0.5, 3),
  legend = FALSE
)

plot(vic_r85_gev_full[[3]],
  main = "SHAPE: GEV of VIC: full",
  breaks = c(-3, -0.5, -0.25, 0, 0.25, 0.5, 3),
  legend = FALSE
)


# Reset the layout to the default (1 plot per page)
par(mfrow = c(1, 1))
