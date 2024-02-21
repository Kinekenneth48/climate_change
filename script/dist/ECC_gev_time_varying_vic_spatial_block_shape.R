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

# ============================================================================#
# create a mask
# ============================================================================#
# prism <- terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]
# prism_mask_vic <- project(prism, vic_r45[[1]])
# 
# 
# writeRaster(prism_mask_vic,
#             "data-raw/mask/prism_mask_vic.tif",
#             overwrite = TRUE
# )

prism_mask_vic = rast("data-raw/mask/prism_mask_vic.tif")


################################################################################
## STEP 1: fit dist for hind vs future (both are assumed nonstationary)
################################################################################
# ============================================================================#
# shape spatial blocking
# ============================================================================#
vic_hist_shape <- terra::app(vic_hist, fun = fit_gev_shape, cores = 12)
vic_r45_shape <- terra::app(vic_r45, fun = fit_gev_shape, cores = 12)
vic_r85_shape <- terra::app(vic_r85, fun = fit_gev_shape, cores = 12)

shape_hist_coarser = vic_hist_shape
shape_r45_coarser = vic_r45_shape
shape_r85_coarser = vic_r85_shape

res(shape_hist_coarser) = 0.5
res(shape_r45_coarser) = 0.5
res(shape_r85_coarser) = 0.5

hist_coarser_shape <- terra::resample(vic_hist_shape, shape_hist_coarser, method = "med")
r45_coarser_shape <- terra::resample(vic_r45_shape, shape_r45_coarser, method = "med")
r85_coarser_shape <- terra::resample(vic_r85_shape, shape_r85_coarser, method = "med")

hist_finer_shape = terra::project(hist_coarser_shape, vic_hist_shape, method = "bilinear")
r45_finer_shape = terra::project(r45_coarser_shape, vic_r45_shape, method = "bilinear")
r85_finer_shape = terra::project(r85_coarser_shape, vic_r85_shape, method = "bilinear")

vic_hist_data_w_shape = c(vic_hist,hist_finer_shape)
vic_r45_data_w_shape = c(vic_r45,r45_finer_shape)
vic_r85_data_w_shape = c(vic_r85,r85_finer_shape)


ECC_vic_hist_spatial_block_shape <- terra::app(vic_hist_data_w_shape,
                                               fun = rl_gev_time_data_w_shape,  cores = 12)
ECC_vic_r45_spatial_block_shape <- terra::app(vic_r45_data_w_shape, 
                                              fun = rl_gev_time_data_w_shape, cores = 12)
ECC_vic_r85_spatial_block_shape <- terra::app(vic_r85_data_w_shape, 
                                              fun = rl_gev_time_data_w_shape, cores = 12)



writeRaster(ECC_vic_hist_spatial_block_shape,
            "E:data-raw/dist_fit_vic/ECC_vic_hist_spatial_block_shape.tif",
            overwrite = TRUE
)

writeRaster(ECC_vic_r45_spatial_block_shape,
            "E:data-raw/dist_fit_vic/ECC_vic_r45_spatial_block_shape.tif",
            overwrite = TRUE
)

writeRaster(ECC_vic_r85_spatial_block_shape,
            "E:data-raw/dist_fit_vic/ECC_vic_r85_spatial_block_shape.tif",
            overwrite = TRUE
)

ECC_vic_hist_spatial_block_shape = rast("E:data-raw/dist_fit_vic/ECC_vic_hist_spatial_block_shape.tif")
ECC_vic_r45_spatial_block_shape = rast("E:data-raw/dist_fit_vic/ECC_vic_r45_spatial_block_shape.tif")
ECC_vic_r85_spatial_block_shape = rast("E:data-raw/dist_fit_vic/ECC_vic_r85_spatial_block_shape.tif")

mean_ecc_hist <- mean(ECC_vic_hist_spatial_block_shape, na.rm = TRUE)
mean_ecc_r45 <- mean(ECC_vic_r45_spatial_block_shape, na.rm = TRUE)
mean_ecc_r85 <- mean(ECC_vic_r85_spatial_block_shape, na.rm = TRUE)


diff_event_r45 <- (mean_ecc_r45 - mean_ecc_hist) / mean_ecc_hist
diff_event_r85 <- (mean_ecc_r85 - mean_ecc_hist) / mean_ecc_hist

diff_event_r45 <- terra::mask(diff_event_r45, prism_mask_vic)
diff_event_r85 <- terra::mask(diff_event_r85, prism_mask_vic)

writeRaster(diff_event_r45,
            "E:data-raw/dist_fit_vic/diff_event_r45.tif",
            overwrite = TRUE
)

writeRaster(diff_event_r85,
            "E:data-raw/dist_fit_vic/diff_event_r85.tif",
            overwrite = TRUE
)


par(mfcol = c(1, 2), mar = c(5, 4, 4, 5) + 0.1)
plot(diff_event_r45,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "diff in 50 year event r45 vs hindcast (both nonstationary) - 
     spatial blocking at 50 km",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     )
)

plot(diff_event_r85,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "diff in 50 year event r85 vs hindcast (both nonstationary)- 
     spatial blocking at 50 km",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     ),
     legend = FALSE
)
par(mfrow = c(1, 1))




################################################################################
## STEP 2: fit dist for hind vs future -full (both are assumed nonstationary)
################################################################################
# ============================================================================#
# shape spatial blocking
# ============================================================================#
vic_r45_full <- c(vic_hist, vic_r45)
vic_r85_full <- c(vic_hist, vic_r85)

vic_hist_full_shape <- terra::app(vic_hist, fun = fit_gev_shape, cores = 12)
vic_r45_full_shape <- terra::app(vic_r45_full, fun = fit_gev_shape, cores = 12)
vic_r85_full_shape <- terra::app(vic_r85_full, fun = fit_gev_shape, cores = 12)

shape_hist_full_coarser = vic_hist_full_shape
shape_r45_full_coarser = vic_r45_full_shape
shape_r85_full_coarser = vic_r85_full_shape

res(shape_hist_full_coarser) = 0.5
res(shape_r45_full_coarser) = 0.5
res(shape_r85_full_coarser) = 0.5

hist_full_coarser_shape <- terra::resample(vic_hist_full_shape, shape_hist_full_coarser, method = "med")
r45_full_coarser_shape <- terra::resample(vic_r45_full_shape, shape_r45_full_coarser, method = "med")
r85_full_coarser_shape <- terra::resample(vic_r85_full_shape, shape_r85_full_coarser, method = "med")

hist_full_finer_shape = terra::project(hist_full_coarser_shape, vic_hist_full_shape, method = "bilinear")
r45_full_finer_shape = terra::project(r45_full_coarser_shape, vic_r45_full_shape, method = "bilinear")
r85_full_finer_shape = terra::project(r85_full_coarser_shape, vic_r85_full_shape, method = "bilinear")

vic_hist_full_data_w_shape = c(vic_hist,hist_full_finer_shape)
vic_r45_full_data_w_shape = c(vic_r45_full,r45_full_finer_shape)
vic_r85_full_data_w_shape = c(vic_r85_full,r85_full_finer_shape)


ECC_vic_hist_full_spatial_block_shape <- terra::app(vic_hist_full_data_w_shape,
                                               fun = rl_gev_time_data_w_shape,  cores = 12)
ECC_vic_r45_full_spatial_block_shape <- terra::app(vic_r45_full_data_w_shape, 
                                              fun = rl_gev_time_data_w_shape, cores = 12)
ECC_vic_r85_full_spatial_block_shape <- terra::app(vic_r85_full_data_w_shape, 
                                              fun = rl_gev_time_data_w_shape, cores = 12)



writeRaster(ECC_vic_hist_full_spatial_block_shape,
            "E:data-raw/dist_fit_vic/ECC_vic_hist_full_spatial_block_shape.tif",
            overwrite = TRUE
)

writeRaster(ECC_vic_r45_full_spatial_block_shape,
            "E:data-raw/dist_fit_vic/ECC_vic_r45_full_spatial_block_shape.tif",
            overwrite = TRUE
)

writeRaster(ECC_vic_r85_full_spatial_block_shape,
            "E:data-raw/dist_fit_vic/ECC_vic_r85_full_spatial_block_shape.tif",
            overwrite = TRUE
)


ECC_vic_hist_full_spatial_block_shape = rast("E:data-raw/dist_fit_vic/ECC_vic_hist_full_spatial_block_shape.tif")
ECC_vic_r45_full_spatial_block_shape = rast("E:data-raw/dist_fit_vic/ECC_vic_r45_full_spatial_block_shape.tif")
ECC_vic_r85_full_spatial_block_shape = rast("E:data-raw/dist_fit_vic/ECC_vic_r85_full_spatial_block_shape.tif")


mean_ecc_hist_full <- mean(ECC_vic_hist_full_spatial_block_shape, na.rm = TRUE)
mean_ecc_r45_full <- mean(ECC_vic_r45_full_spatial_block_shape, na.rm = TRUE)
mean_ecc_r85_full <- mean(ECC_vic_r85_full_spatial_block_shape, na.rm = TRUE)


diff_event_r45_full <- (mean_ecc_r45_full - mean_ecc_hist_full) / mean_ecc_hist_full
diff_event_r85_full <- (mean_ecc_r85_full - mean_ecc_hist_full) / mean_ecc_hist_full

diff_event_r45_full <- terra::mask(diff_event_r45_full, prism_mask)
diff_event_r85_full <- terra::mask(diff_event_r85_full, prism_mask)

par(mfcol = c(1, 2), mar = c(5, 4, 4, 5) + 0.1)
plot(diff_event_r45_full,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "diff in 50 year event r45 vs hindcast (nonstationary) 
     spatial block at 50km",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     )
)

plot(diff_event_r85_full,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "diff in 50 year event r85 vs hindcast (nonstationary)
     spatial block at 50km",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     ),
     legend = FALSE
)
par(mfrow = c(1, 1))
