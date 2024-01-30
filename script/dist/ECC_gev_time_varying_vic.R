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
source("R/gev_fit_time_varying.R")


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

# ============================================================================#
# load the data
# ============================================================================#
vic_r45 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp45_access10.tif")

tictoc::tic()
ECC_vic_future <- terra::app(vic_r45, fun = gev_fit_time_varying, cores = 10)
tictoc::toc()

ECC_vic_future = vic_future_lnorm_event
max_ecc = max(ECC_vic_future)
mean_ecc = mean(ECC_vic_future)
min_ecc = min(ECC_vic_future)


# constant risk plot: the risk that the highest snow load in a year is larger than
# 7.6 is less than 0.02
plot(max_ecc)
plot(mean_ecc)
plot(min_ecc)