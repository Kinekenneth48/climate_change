
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

tif_hist <- list.files("E:/data-raw/swe_model_vars/ssp585/hist/prediction/rf/",
                        pattern = "\\.tif$", full.names = TRUE
)


tif_r1 <- list.files("E:/data-raw/swe_model_vars/ssp585/r1/prediction/rf/",
                        pattern = "\\.tif$", full.names = TRUE
)

tif_r2 <- list.files("E:/data-raw/swe_model_vars/ssp585/r2/prediction/rf/",
                     pattern = "\\.tif$", full.names = TRUE
)

tif_r3 <- list.files("E:/data-raw/swe_model_vars/ssp585/r3/prediction/rf/",
                     pattern = "\\.tif$", full.names = TRUE
)

ssp585_hist_swe <- terra::rast(tif_hist)
ssp585_r1_swe <- terra::rast(tif_r1)
ssp585_r2_swe <- terra::rast(tif_r2)
ssp585_r3_swe <- terra::rast(tif_r3)

################################################################################
## STEP 1: r1
################################################################################

hist_mean = mean(ssp585_hist_swe, na.rm =TRUE)
hist_sd = stdev(ssp585_hist_swe, na.rm =TRUE)

r1_mean = mean(ssp585_r1_swe, na.rm =TRUE)
r2_mean = mean(ssp585_r2_swe, na.rm =TRUE)
r3_mean = mean(ssp585_r3_swe, na.rm =TRUE)

r1_sd = stdev(ssp585_r1_swe, na.rm =TRUE)
r2_sd = stdev(ssp585_r2_swe, na.rm =TRUE)
r3_sd = stdev(ssp585_r3_swe, na.rm =TRUE)

r1_mean_diff = r1_mean - hist_mean
r2_mean_diff = r2_mean - hist_mean
r3_mean_diff = r3_mean - hist_mean

r1_sd_diff = r1_sd - hist_sd
r2_sd_diff = r2_sd - hist_sd
r3_sd_diff = r3_sd - hist_sd


r_mean_diff = mean(r1_mean_diff, r2_mean_diff,r3_mean_diff)
r_sd_diff =mean(r1_sd_diff,r2_sd_diff,r3_sd_diff)

par(mfcol = c(1, 2), mar = c(5, 4, 4, 5) + 0.1)
plot(r_mean_diff, breaks =c(-1000, -250, -50, -25, 0, 25, 50, 250, 1000))
plot(r_sd_diff, breaks =c( -100, -50, -25, 0, 25, 50, 250, 600))
par(mfrow = c(1, 1))


