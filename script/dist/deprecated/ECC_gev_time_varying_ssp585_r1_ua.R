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
ua_swe <- terra::rast("E:/data-raw/ua/raster/annual/max_swe_ua.tif")




# ============================================================================#
# create a single multilayer raster for r1 prediction
# ============================================================================#
# Get names of TIFF files in the folder
tif_files <- list.files("E:/data-raw/swe_model_vars/ssp585/r1/prediction/rf/",
  pattern = "\\.tif$", full.names = TRUE
)

data <- list()
for (i in 1:length(tif_files)) {
  data[[i]] <- terra::rast(tif_files[i])
}

ssp585_r1_swe <- terra::rast(data)

# ============================================================================#
# match the extent ssp585_r1_swe and ua_swe
# ============================================================================#

ua_swe <- terra::project(ua_swe, ssp585_r1_swe, method = "bilinear")

full_r1 <- c(ua_swe, ssp585_r1_swe)


################################################################################
## STEP 1: COMPARE OVERLAP B/N UA AND R1
################################################################################

# ============================================================================#
# get data from 2016 to 2021
# ============================================================================#
ua_test <- subset(ua_swe, time(ua_swe) >= 2016 & time(ua_swe) <= 2021)
r1_test <- subset(ssp585_r1_swe, time(ssp585_r1_swe) >= 2016 &
                    time(ssp585_r1_swe) <= 2021)

diff_mean_r1 <- r1_test - ua_test

plot(diff_mean_r1[[1]], breaks = c(-500, -250, -100, -50, -25, -10, 0, 10, 25, 50, 100, 250, 500))

# ============================================================================#
# MAE and RMSE
# ============================================================================#
rmse <- vector("numeric", length = 0)
mae <- vector("numeric", length = 0)

for (i in 1:nlyr(diff_mean_r1)) {
  rmse[i] <- sqrt(mean((values(diff_mean_r1[[i]]))^2, na.rm = TRUE))
  mae[i] <- mean(abs(values(diff_mean_r1[[i]])), na.rm = TRUE)
}

mae <- mean(mae)
rmse <- mean(rmse)
mae
rmse



################################################################################
## STEP 2: FIT DISTR.
################################################################################

# estimate shape parameter with Lmoments
ssp585_r1_shape <- terra::app(ssp585_r1_swe, fun = fit_gev_shape, cores = 12)
ua_shape <- terra::app(ua_swe, fun = fit_gev_shape, cores = 12)
full_shape <- terra::app(full_r1, fun = fit_gev_shape, cores = 12)

# add the shape parameter to the end of the data
ssp585_r1_swe_w_shape <- c(ssp585_r1_swe, ssp585_r1_shape)
ua_swe_w_shape <- c(ua_swe, ua_shape)
full_r1_w_shape <- c(full_r1, full_shape)


ECC_ssp585_r1 <- terra::app(ssp585_r1_swe_w_shape,
  fun = rl_gev_time_data_w_shape, cores = 12
)

ECC_ua_swe <- terra::app(ua_swe_w_shape,
  fun = fit_gev_event, cores = 12
)

ECC_full_r1 <- terra::app(full_r1_w_shape,
  fun = rl_gev_time_data_w_shape, cores = 12
)

writeRaster(ECC_ssp585_r1,
  "E:data-raw/dist_fit_vic/ECC_ssp585_r1.tif",
  overwrite = TRUE
)

writeRaster(ECC_ua_swe,
  "E:data-raw/dist_fit_vic/ECC_ua_swe.tif",
  overwrite = TRUE
)

writeRaster(ECC_full_r1,
  "E:data-raw/dist_fit_vic/ECC_full_r1.tif",
  overwrite = TRUE
)



ECC_ssp585_r1 <- rast("E:data-raw/dist_fit_vic/ECC_ssp585_r1.tif")
ECC_ua_swe <- rast("E:data-raw/dist_fit_vic/ECC_ua_swe.tif")
ECC_full_r1 <- rast("E:data-raw/dist_fit_vic/ECC_full_r1.tif")

mean_ecc_ssp585_r1 <- mean(ECC_ssp585_r1, na.rm = TRUE)
mean_ecc_full_r1 <- mean(ECC_full_r1, na.rm = TRUE)



diff_event_ssp585_r1 <- (mean_ecc_ssp585_r1 - ECC_ua_swe) / ECC_ua_swe
diff_event_ssp585_full <- (mean_ecc_full_r1 - ECC_ua_swe) / ECC_ua_swe



par(mfcol = c(1, 2), mar = c(5, 4, 4, 5) + 0.1)
plot(diff_event_ssp585_r1,
  breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
  main = "diff in 50 year event r1 vs UA ",
  col = c(
    "#543005", "#8c510a", "#bf812d", "#dfc27d",
    "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
  )
)

plot(diff_event_ssp585_full,
  breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
  main = "diff in 50 year event r1 vs UA (FULL)",
  col = c(
    "#543005", "#8c510a", "#bf812d", "#dfc27d",
    "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
  ),
  legend = FALSE
)
par(mfrow = c(1, 1))
