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
source("R/fit_gev_shape.R")
source("R/rl_gev_time_data_w_shape.R")
source("R/gev_fit_time_varying.R")
source("R/gev_fit_stat_nonstat.R")
source("R/fit_gev_event.R")
source("R/mk.R")

fit_gev_shape


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

 set.seed(34521)
# ============================================================================#
# load the data
# ============================================================================#
vic_r45 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp45_access10.tif")
vic_r85 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp85_access10.tif")
vic_hist <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")


vic_hist_shape <- terra::app(vic_hist, fun = fit_gev_shape, cores = 12)
shape_hist_coarser = vic_hist_shape
res(shape_hist_coarser) = 0.5

coarser_shape <- terra::resample(vic_hist_shape, shape_hist_coarser, method = "med")

finer_shape = terra::project(coarser_shape, vic_hist_shape, method = "bilinear")

vic_hist_data_w_shape = c(vic_hist,finer_shape)


vic_hist_rl <- terra::app(vic_hist_data_w_shape, fun = rl_gev_time_data_w_shape, cores = 10)







