################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
pacman::p_load(
  fitdistrplus, terra, extRemes, evd, ismev, trend, tidyterra,
  tidyverse
)


source("R/gev_fit_time_varying.R")
source("R/fit_gev_shape.R")
source("R/fit_gev_event.R")
source("R/mk.R")
source("R/rl_gev_time_data_w_shape.R")


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

# ============================================================================#
# load the data
# ============================================================================#

rf_hist <- terra::rast(list.files("E:/data-raw/swe_model_vars/ssp585/hist/prediction/rf/",
  pattern = "\\.tif$", full.names = TRUE
))

rf_r1 <- terra::rast(list.files("E:/data-raw/swe_model_vars/ssp585/r1/prediction/rf/",
  pattern = "\\.tif$", full.names = TRUE
))

rf_r2 <- terra::rast(list.files("E:/data-raw/swe_model_vars/ssp585/r2/prediction/rf/",
  pattern = "\\.tif$", full.names = TRUE
))

rf_r3 <- terra::rast(list.files("E:/data-raw/swe_model_vars/ssp585/r3/prediction/rf/",
  pattern = "\\.tif$", full.names = TRUE
))


# ==============================================================================
# get USA map with state boundaries
# ==============================================================================

conus <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

conus <- terra::vect(conus)


################################################################################
## STEP 1: check for Stationarity
################################################################################


# mk test
mk_rf_hist <- terra::app(rf_hist, fun = mk)
mk_rf_r1 <- terra::app(rf_r1, fun = mk)
mk_rf_r2 <- terra::app(rf_r2, fun = mk)
mk_rf_r3 <- terra::app(rf_r3, fun = mk)


cls <- data.frame(id = 0:1, cover = c("stationary", "non-stationary"))
levels(mk_rf_hist) <- cls
levels(mk_rf_r1) <- cls
levels(mk_rf_r2) <- cls
levels(mk_rf_r3) <- cls


mk_rf <- c(mk_rf_hist, mk_rf_r1, mk_rf_r2, mk_rf_r3)
names(mk_rf) <- c("hindcast", "r1", "r2", "r3")



# ============================================================================#
# plot stat or non-stat(check for Stationarity)
# ============================================================================#


ggplot() +
  geom_spatraster(
    data = mk_rf
  ) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_manual(
    name = "Level",
    values = c(
      "#ffff33", "#a65628"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    # legend.position = c(0.85, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 30)
  )






################################################################################
## STEP 2: Fit dist (HIST- STAT: FUTURE-NONSTAT) independent
################################################################################


# estimate shape parameter with Lmoments
rf_r1_shape <- terra::app(rf_r1, fun = fit_gev_shape, cores = 12)
rf_r2_shape <- terra::app(rf_r2, fun = fit_gev_shape, cores = 12)
rf_r3_shape <- terra::app(rf_r3, fun = fit_gev_shape, cores = 12)

# add the shape parameter to the end of the data
rf_r1_w_shape <- c(rf_r1, rf_r1_shape)
rf_r2_w_shape <- c(rf_r2, rf_r2_shape)
rf_r3_w_shape <- c(rf_r3, rf_r3_shape)


# fit dist and estimate MRI (mean is varying for future scenarios)
ECC_rf_r1_stat_nonstat <- terra::app(rf_r1_w_shape,
  fun = rl_gev_time_data_w_shape, cores = 12
)

ECC_rf_r2_stat_nonstat <- terra::app(rf_r2_w_shape,
  fun = rl_gev_time_data_w_shape, cores = 12
)

ECC_rf_r3_stat_nonstat <- terra::app(rf_r3_w_shape,
  fun = rl_gev_time_data_w_shape, cores = 12
)

ECC_rf_hist_stat_nonstat <- terra::app(rf_hist,
  fun = fit_gev_event, cores = 12
)


writeRaster(ECC_rf_r1_stat_nonstat,
  "E:data-raw/dist_fit_vic/ECC_rf_r1_stat_nonstat.tif",
  overwrite = TRUE
)

writeRaster(ECC_rf_r2_stat_nonstat,
  "E:data-raw/dist_fit_vic/ECC_rf_r2_stat_nonstat.tif",
  overwrite = TRUE
)

writeRaster(ECC_rf_r3_stat_nonstat,
  "E:data-raw/dist_fit_vic/ECC_rf_r3_stat_nonstat.tif",
  overwrite = TRUE
)

writeRaster(ECC_rf_hist_stat_nonstat,
  "E:data-raw/dist_fit_vic/ECC_rf_hist_stat_nonstat.tif",
  overwrite = TRUE
)



ECC_rf_hist_stat_nonstat <- rast("E:data-raw/dist_fit_vic/LOCA2_RF/ECC_rf_hist_stat_nonstat.tif")
ECC_rf_r3_stat_nonstat <- rast("E:data-raw/dist_fit_vic/LOCA2_RF/ECC_rf_r3_stat_nonstat.tif")
ECC_rf_r2_stat_nonstat <- rast("E:data-raw/dist_fit_vic/LOCA2_RF/ECC_rf_r2_stat_nonstat.tif")
ECC_rf_r1_stat_nonstat <- rast("E:data-raw/dist_fit_vic/LOCA2_RF/ECC_rf_r1_stat_nonstat.tif")



mean_ecc_rf_r1_stat_nonstat <- mean(ECC_rf_r1_stat_nonstat, na.rm = TRUE)
mean_ecc_rf_r2_stat_nonstat <- mean(ECC_rf_r2_stat_nonstat, na.rm = TRUE)
mean_ecc_rf_r3_stat_nonstat <- mean(ECC_rf_r3_stat_nonstat, na.rm = TRUE)
mean_ecc_rf_hist_stat_nonstat <- mean(ECC_rf_hist_stat_nonstat, na.rm = TRUE)



diff_event_r1_stat_nonstat <- (mean_ecc_rf_r1_stat_nonstat -
  mean_ecc_rf_hist_stat_nonstat) / mean_ecc_rf_hist_stat_nonstat

diff_event_r2_stat_nonstat <- (mean_ecc_rf_r2_stat_nonstat -
  mean_ecc_rf_hist_stat_nonstat) / mean_ecc_rf_hist_stat_nonstat

diff_event_r3_stat_nonstat <- (mean_ecc_rf_r3_stat_nonstat -
  mean_ecc_rf_hist_stat_nonstat) / mean_ecc_rf_hist_stat_nonstat

diff_event_r_stat_nonstat <- mean(c(
  diff_event_r1_stat_nonstat,
  diff_event_r2_stat_nonstat,
  diff_event_r3_stat_nonstat
), na.rm = TRUE)

ggplot() +
  geom_spatraster_contour_filled(
    data = diff_event_r_stat_nonstat,
    breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
  ) +
  scale_fill_manual(
    name = "MRI Percent \n Change",
    values = c(
     # "#543005", 
      "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3",
      "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    legend.position = c(0.91, 0.28),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  )






################################################################################
## STEP 3: Fit dist (HIST- STAT: FUTURE-NONSTAT) full
################################################################################
rf_r1_full <- c(rf_hist, rf_r1)
rf_r2_full <- c(rf_hist, rf_r2)
rf_r3_full <- c(rf_hist, rf_r3)

# estimate shape parameter with Lmoments
rf_r1_shape_full <- terra::app(rf_r1_full, fun = fit_gev_shape, cores = 12)
rf_r2_shape_full <- terra::app(rf_r2_full, fun = fit_gev_shape, cores = 12)
rf_r3_shape_full <- terra::app(rf_r3_full, fun = fit_gev_shape, cores = 12)


# add the shape parameter to the end of the data
rf_r1_w_shape_full <- c(rf_r1, rf_r1_shape_full)
rf_r2_w_shape_full <- c(rf_r2, rf_r2_shape_full)
rf_r3_w_shape_full <- c(rf_r3, rf_r3_shape_full)


# fit dist and estimate MRI (mean is varying for future scenarios)
ECC_rf_r1_stat_nonstat_full <- terra::app(rf_r1_w_shape_full,
  fun = rl_gev_time_data_w_shape, cores = 12
)

ECC_rf_r2_stat_nonstat_full <- terra::app(rf_r2_w_shape_full,
  fun = rl_gev_time_data_w_shape, cores = 12
)

ECC_rf_r3_stat_nonstat_full <- terra::app(rf_r3_w_shape_full,
  fun = rl_gev_time_data_w_shape, cores = 12
)

ECC_rf_hist_stat_nonstat_full <- terra::app(rf_hist,
  fun = fit_gev_event, cores = 12
)


writeRaster(ECC_rf_r1_stat_nonstat_full,
  "E:data-raw/dist_fit_vic/ECC_rf_r1_stat_nonstat_full.tif",
  overwrite = TRUE
)

writeRaster(ECC_rf_r2_stat_nonstat_full,
  "E:data-raw/dist_fit_vic/ECC_rf_r2_stat_nonstat_full.tif",
  overwrite = TRUE
)

writeRaster(ECC_rf_r3_stat_nonstat_full,
  "E:data-raw/dist_fit_vic/ECC_rf_r3_stat_nonstat_full.tif",
  overwrite = TRUE
)


writeRaster(ECC_rf_hist_stat_nonstat_full,
  "E:data-raw/dist_fit_vic/ECC_rf_hist_stat_nonstat_full.tif",
  overwrite = TRUE
)



ECC_rf_hist_stat_nonstat_full <- rast("E:data-raw/dist_fit_vic/LOCA2_RF/ECC_rf_hist_stat_nonstat_full.tif")
ECC_rf_r3_stat_nonstat_full <- rast("E:data-raw/dist_fit_vic/LOCA2_RF/ECC_rf_r3_stat_nonstat_full.tif")
ECC_rf_r2_stat_nonstat_full <- rast("E:data-raw/dist_fit_vic/LOCA2_RF/ECC_rf_r2_stat_nonstat_full.tif")
ECC_rf_r1_stat_nonstat_full <- rast("E:data-raw/dist_fit_vic/LOCA2_RF/ECC_rf_r1_stat_nonstat_full.tif")


mean_ecc_r1_stat_nonstat_full <- mean(ECC_rf_r1_stat_nonstat_full, na.rm = TRUE)
mean_ecc_r2_stat_nonstat_full <- mean(ECC_rf_r2_stat_nonstat_full, na.rm = TRUE)
mean_ecc_r3_stat_nonstat_full <- mean(ECC_rf_r3_stat_nonstat_full, na.rm = TRUE)
mean_ecc_hist_stat_nonstat_full <- mean(ECC_rf_hist_stat_nonstat_full, na.rm = TRUE)



diff_event_r1_stat_nonstat_full <- (mean_ecc_r1_stat_nonstat_full -
  mean_ecc_hist_stat_nonstat_full) /
  mean_ecc_hist_stat_nonstat_full

diff_event_r2_stat_nonstat_full <- (mean_ecc_r2_stat_nonstat_full -
  mean_ecc_hist_stat_nonstat_full) /
  mean_ecc_hist_stat_nonstat_full

diff_event_r3_stat_nonstat_full <- (mean_ecc_r3_stat_nonstat_full -
  mean_ecc_hist_stat_nonstat_full) /
  mean_ecc_hist_stat_nonstat_full


diff_event_r_stat_nonstat_full <- mean(
  c(
    diff_event_r1_stat_nonstat_full,
    diff_event_r2_stat_nonstat_full,
    diff_event_r3_stat_nonstat_full
  ),
  na.rm = TRUE
)

ggplot() +
  geom_spatraster_contour_filled(
    data = diff_event_r_stat_nonstat_full,
    breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
  ) +
  scale_fill_manual(
    name = "MRI Percent \n Change",
    values = c(
      "#543005",
      "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3",
      "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    # legend.position = "none",
    legend.position = c(0.91, 0.28),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  )
