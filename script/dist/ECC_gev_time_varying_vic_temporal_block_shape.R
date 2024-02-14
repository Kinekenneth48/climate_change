################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(fitdistrplus)
library(terra)
library(extRemes)
library(evd)
library(ismev)
library(trend)

source("R/fit_log_normal_less.R")
source("R/fit_log_normal.R")
source("R/fit_gev.R")
source("R/gev_fit_time_varying.R")
source("R/gev_fit_stat_nonstat.R")
source("R/fit_gev_event.R")
source("R/mk.R")
source("R/fit_gev_shape.R")
source("R/rl_gev_time_data_w_shape.R")


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

set.seed(275349)

# ============================================================================#
# load the data
# ============================================================================#
vic_r45 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp45_access10.tif")
vic_r85 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp85_access10.tif")
vic_hist <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")





################################################################################
## STEP 1: CREATE BLOCK AND FIT DISTR.
################################################################################



# Define the time periods
time_periods <- list(
  c(2007, 2057),
  c(2025, 2075),
  c(2035, 2085),
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
  vic_r8 <- subset(vic_r85, time(vic_r85) >= start_year & time(vic_r85)
                    <= end_year)
  vic_r4 <- subset(vic_r45, time(vic_r45) >= start_year & time(vic_r45)
                    <= end_year)
  
  
  # Fit the GEV distribution
  vic_future_gev_r45 <- terra::app(vic_r4, fit_gev_shape, cores = 10)
  names(vic_future_gev_r45) <- c("shape")
  
  
  # Save the GEV fit
  gev_file <- file.path(output_dir, paste0(
    "gev_fit_r45_TP", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_gev_r45, gev_file, overwrite = TRUE)
  
  # Fit the GEV distribution
  vic_future_gev_r85 <- terra::app(vic_r8, fit_gev_shape, cores = 10)
  names(vic_future_gev_r85) <- c( "shape")
  
  # Save the GEV fit
  gev_file <- file.path(output_dir, paste0(
    "gev_fit_r85_TP", start_year, "_",
    end_year, ".tif"
  ))
  terra::writeRaster(vic_future_gev_r85, gev_file, overwrite = TRUE)
}



gev_fit_r45_TP2007_2057 = rast("E:/data-raw/dist_fit_vic/gev_fit_r45_TP2007_2057.tif")
gev_fit_r45_TP2025_2075 = rast("E:/data-raw/dist_fit_vic/gev_fit_r45_TP2025_2075.tif")
gev_fit_r45_TP2035_2085 = rast("E:/data-raw/dist_fit_vic/gev_fit_r45_TP2035_2085.tif")
gev_fit_r45_TP2050_2100 = rast("E:/data-raw/dist_fit_vic/gev_fit_r45_TP2050_2100.tif")

gev_fit_r85_TP2007_2057 = rast("E:/data-raw/dist_fit_vic/gev_fit_r85_TP2007_2057.tif")
gev_fit_r85_TP2025_2075 = rast("E:/data-raw/dist_fit_vic/gev_fit_r85_TP2025_2075.tif")
gev_fit_r85_TP2035_2085 = rast("E:/data-raw/dist_fit_vic/gev_fit_r85_TP2035_2085.tif")
gev_fit_r85_TP2050_2100 = rast("E:/data-raw/dist_fit_vic/gev_fit_r85_TP2050_2100.tif")

prism_mask_vic = rast("data-raw/mask/prism_mask_vic.tif")

gev_fit_r45_TP2007_2057 <- terra::mask(gev_fit_r45_TP2007_2057, prism_mask_vic)
gev_fit_r45_TP2025_2075 <- terra::mask(gev_fit_r45_TP2025_2075, prism_mask_vic)
gev_fit_r45_TP2035_2085 <- terra::mask(gev_fit_r45_TP2035_2085, prism_mask_vic)
gev_fit_r45_TP2050_2100 <- terra::mask(gev_fit_r45_TP2050_2100, prism_mask_vic)
gev_fit_r85_TP2007_2057 <- terra::mask(gev_fit_r85_TP2007_2057, prism_mask_vic)
gev_fit_r85_TP2025_2075 <- terra::mask(gev_fit_r85_TP2025_2075, prism_mask_vic)
gev_fit_r85_TP2035_2085 <- terra::mask(gev_fit_r85_TP2035_2085, prism_mask_vic)
gev_fit_r85_TP2050_2100 <- terra::mask(gev_fit_r85_TP2050_2100, prism_mask_vic)

# ============================================================================#
# fit distr.
# ============================================================================#
tictoc::tic()
vic_hist_gev <- terra::app(vic_hist, fit_gev, cores = 10)
names(vic_hist_gev) <- c("location", "scale", "shape")
tictoc::toc()

hist_shape=vic_hist_gev[[3]]
hist_shape <- terra::mask(hist_shape, prism_mask_vic)

plot(hist_shape,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "hist - shape",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     )
)




par(mfcol = c(2, 2), mar = c(5, 4, 4, 5) + 0.1)
plot(gev_fit_r45_TP2007_2057,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "BLOCK YR: 2007 - 2057 (r45)",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     )
)

plot(gev_fit_r45_TP2025_2075,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "BLOCK YR: 2025 - 2075 (r45)",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     ),
     legend = FALSE
)

plot(gev_fit_r45_TP2035_2085,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "BLOCK YR: 2035 - 2085 (r45)",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     ),
     legend = FALSE
)

plot(gev_fit_r45_TP2050_2100,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "BLOCK YR: 2050 -2100 (r45)",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     ),
     legend = FALSE
)
par(mfrow = c(1, 1))







par(mfcol = c(2, 2), mar = c(5, 4, 4, 5) + 0.1)
plot(gev_fit_r85_TP2007_2057,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "BLOCK YR: 2007 - 2057 (r85)",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     )
)

plot(gev_fit_r85_TP2025_2075,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "BLOCK YR: 2025 - 2075 (r85)",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     ),
     legend = FALSE
)

plot(gev_fit_r85_TP2035_2085,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "BLOCK YR: 2035 - 2085 (r85)",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     ),
     legend = FALSE
)

plot(gev_fit_r85_TP2050_2100,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "BLOCK YR: 2050 -2100 (r85)",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     ),
     legend = FALSE
)
par(mfrow = c(1, 1))



