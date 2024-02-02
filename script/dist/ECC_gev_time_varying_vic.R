################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(fitdistrplus)
library(terra)
library(extRemes)

source("R/fit_log_normal_less.R")
source("R/fit_log_normal.R")
source("R/fit_gev.R")
source("R/gev_fit_time_varying.R")


# manage memory usage so PC doesn't crash
# terraOptions(memfrac = 0.80, verbose = TRUE)

# ============================================================================#
# load the data
# ============================================================================#
vic_r45 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp45_access10.tif")
vic_r85 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp85_access10.tif")
vic_hist <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")


################################################################################
## STEP 1: check for Stationarity
################################################################################

# mk test
mk_test_hist <- terra::app(vic_hist, fun = mk)
mk_test_R45 <- terra::app(vic_r45, fun = mk)
mk_test_R85 <- terra::app(vic_r85, fun = mk)


# ============================================================================#
# create a mask
# ============================================================================#
prism <- terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]
prism_mask <- project(prism, mk_test_R45)


# ============================================================================#
# plot stat or non-stat(check for Stationarity)
# ============================================================================#
mk_test_hist <- terra::mask(mk_test_hist, prism_mask)
mk_test_R45 <- terra::mask(mk_test_R45, prism_mask)
mk_test_R85 <- terra::mask(mk_test_R85, prism_mask)

par(mfcol = c(2, 2))
plot(mk_test_hist, main = "hist")
plot(mk_test_R45, main = "future r45", legend = FALSE)
plot(mk_test_R85, main = "future r85", legend = FALSE)
par(mfrow = c(1, 1))



# ============================================================================#
# plot diff in mean of vic hindcast and future
# ============================================================================#
plot(vic_hist[[4]], breaks = c(0, 10, 25, 50, 100, 500, 5000))

mean_hist <- mean(vic_hist, na.rm = TRUE)
mean_future_r45 <- mean(vic_r45, na.rm = TRUE)
mean_future_r85 <- mean(vic_r85, na.rm = TRUE)



par(mfcol = c(2, 2))
plot(mean_hist, breaks = c(0, 10, 25, 50, 100, 500, 5000), main = "hist")
plot(mean_future_r45, breaks = c(0, 10, 25, 50, 100, 500, 5000), main = "future r45", legend = FALSE)
plot(mean_future_r85, breaks = c(0, 10, 25, 50, 100, 500, 5000), main = "future r85", legend = FALSE)
par(mfrow = c(1, 1))




################################################################################
## STEP 2: fit dist for stationarity and non-stationarity
################################################################################
# ============================================================================#
# ECC / CONVENTIONAL CONCEPT MIXED
# ============================================================================#

ECC_vic_r45 <- terra::app(vic_r45, fun = gev_fit_stat_nonstat)
ECC_vic_r85 <- terra::app(vic_r85, fun = gev_fit_stat_nonstat)
ECC_vic_hist <- terra::app(vic_hist, fun = gev_fit_stat_nonstat)


max_ecc_r45 <- max(ECC_vic_r45, na.rm = TRUE)
max_ecc_hist <- max(ECC_vic_hist, na.rm = TRUE)
max_ecc_r85 <- mean(ECC_vic_r85, na.rm = TRUE)


diff_event_r45 <- (max_ecc_r45 - max_ecc_hist) / max_ecc_hist
diff_event_r85 <- (max_ecc_r85 - max_ecc_hist) / max_ecc_hist

diff_event_r45 <- terra::mask(diff_event_r45, prism_mask)
diff_event_r85 <- terra::mask(diff_event_r85, prism_mask)

par(mfcol = c(1, 2), mar = c(5, 4, 4, 5) + 0.1)
plot(diff_event_r45,
  breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
  main = "diff in 50 year event r45 vs hindcast",
  col = c(
    "#543005", "#8c510a", "#bf812d", "#dfc27d",
    "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
  )
)

plot(diff_event_r85,
  breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
  main = "diff in 50 year event r85 vs hindcast",
  col = c(
    "#543005", "#8c510a", "#bf812d", "#dfc27d",
    "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
  ),
  legend = FALSE
)
par(mfrow = c(1, 1))





# ============================================================================#
# Hindcast -stat  future - non stat
# ============================================================================#





# ============================================================================#
# Hindcast -stat  future - stat
# ============================================================================#






points <- terra::as.data.frame(max_ecc, xy = TRUE, na.rm = FALSE)

lon <- -94.03125
lat <- 32.46875
t <- data.frame(lon, lat)

future_points <- (as.vector(unlist(extract(vic_r45, t))))
hist_points <- (as.vector(unlist(extract(vic_hist, t))))
future_points <- future_points[future_points > 1]
hist_points <- hist_points[hist_points > 1]

time <- 1:length(future_points)
data <- data.frame(future_points, time)
n <- length

sp <- fevd(future_points, type = c("GEV"), method = c("Lmoments"))[["results"]][["shape"]]
M3 <- fgev(future_points, shape = sp)

sp <- fevd(hist_points, type = c("GEV"), method = c("Lmoments"))[["results"]][["shape"]]
M33 <- fgev(hist_points, shape = sp)

sp <- fevd(hist_points, type = c("GEV"), method = c("Lmoments"))[["results"]][["shape"]]
M33 <- fgev(hist_points, nsloc = 1:length(hist_points), shape = sp)


r <- ismev::gev.fit(
  xdat = future_points, ydat = matrix(rep(1:n, each = 1), nrow = n, ncol = 1),
  mul = 1,
  method = "Nelder-Mead", maxit = 100000
)

plot(future_points)
plot(hist_points)

fit00 <- fevd(hist_points, type = c("GEV"), method = c("Lmoments"))
fit01 <- fevd(hist_points, type = c("GEV"))

fit21 <- fevd(future_points, type = c("GEV"), method = c("Lmoments"))
fit22 <- fevd(future_points, type = c("GEV"))

return.level(fit00, return.period = c(50))
return.level(fit01, return.period = c(50))

return.level(fit21, return.period = c(50))
return.level(fit22, return.period = c(50))

estimate_quantile(mu = 0.9307, sigma = 1.4606, xi = 0.5693192, p = 0.02)
estimate_quantile(mu = 3.797, sigma = 4.998, xi = 0.2189842, p = 0.02)

estimate_quantile(mu = 5.301, sigma = 5.008, xi = 0.3997378, p = 0.02)
estimate_quantile(mu = 7.369, sigma = 6.513, xi = 0.07120665, p = 0.02)
qgev(p = 0.02, loc = 7.369, scale = 6.513, shape = 0.07120665, lower.tail = FALSE)

par(mfcol = c(2, 2))
plot(max_ecc, breaks = c(0, 50, 100, 500, 1000, 2000, 5000), main = "ECC max")
plot(mean_ecc, breaks = c(0, 50, 100, 500, 1000, 2000, 5000), main = "ECC mean")
plot(min_ecc, breaks = c(0, 50, 100, 500, 1000, 2000, 5000), main = "ECC min")

par(mfrow = c(1, 1))

# constant risk plot: the risk that the highest snow load in a year is larger than
# 7.6 is less than 0.02
plot(max_ecc)
plot(mean_ecc)
plot(min_ecc)
