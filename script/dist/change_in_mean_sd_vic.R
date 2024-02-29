
################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
pacman::p_load(fitdistrplus,terra,extRemes,evd,
               ismev,trend)


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

set.seed(275349)

# ============================================================================#
# load the data
# ============================================================================#

vic_hist <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")
r45 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp45_access10.tif")
r85 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp85_access10.tif")



################################################################################
## STEP 1: r1
################################################################################

hist_mean = mean(vic_hist, na.rm =TRUE)
hist_sd = stdev(vic_hist, na.rm =TRUE)

r45_mean = mean(r45, na.rm =TRUE)
r85_mean = mean(r85, na.rm =TRUE)

r45_sd = mean(r45, na.rm =TRUE)
r85_sd = mean(r85, na.rm =TRUE)

r45_mean_diff = r45_mean - hist_mean
r85_mean_diff = r85_mean - hist_mean


r45_sd_diff = r45_sd - hist_sd
r85_sd_diff = r85_sd - hist_sd


prism_mask_vic = rast("data-raw/mask/prism_mask_vic.tif")

r45_mean_diff <- terra::mask(r45_mean_diff, prism_mask_vic)
r85_mean_diff <- terra::mask(r85_mean_diff, prism_mask_vic)
r45_sd_diff <- terra::mask(r45_sd_diff, prism_mask_vic)
r85_sd_diff <- terra::mask(r85_sd_diff, prism_mask_vic)


par(mfcol = c(1, 2), mar = c(5, 4, 4, 5) + 0.1)
plot(r45_mean_diff, breaks =c(-1000, -250, -50, -25, 0, 25, 50, 250, 1000))
plot(r45_sd_diff, breaks =c( -100, -50, -25, 0, 25, 50, 250, 600))
par(mfrow = c(1, 1))

plot(r45_mean_diff, breaks =c(-1000, -250, -50, -25, 0, 25, 50, 250, 1000))
plot(r45_sd_diff, breaks =c( -100, -50, -25, 0, 25, 50, 250, 600))
plot(r85_mean_diff, breaks =c(-1000, -250, -50, -25, 0, 25, 50, 250, 1000))
plot(r85_sd_diff, breaks =c( -100, -50, -25, 0, 25, 50, 250, 600))
