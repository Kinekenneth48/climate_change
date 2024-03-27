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


source("R/mk.R")
source("R/gev_fit_stat_nonstat.R")

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

rf_r <- (rf_r1 + rf_r2 + rf_r3) / 3
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
## STEP 2: Fit dist (HIST- mix STAT: FUTURE-mix STAT) independent
################################################################################


# fit dist and estimate MRI (mean is varying for future scenarios)
ECC_rf_r1_mixstat <- terra::app(rf_r1,
  fun = gev_fit_stat_nonstat, cores = 12
)

ECC_rf_r2_mixstat <- terra::app(rf_r2,
  fun = gev_fit_stat_nonstat, cores = 12
)

ECC_rf_r3_mixstat <- terra::app(rf_r3,
  fun = gev_fit_stat_nonstat, cores = 12
)

ECC_rf_hist_mixstat <- terra::app(rf_hist,
  fun = gev_fit_stat_nonstat, cores = 12
)


writeRaster(ECC_rf_r1_mixstat,
  "E:data-raw/dist_fit_vic/ECC_rf_r1_mixstat.tif",
  overwrite = TRUE
)

writeRaster(ECC_rf_r2_mixstat,
  "E:data-raw/dist_fit_vic/ECC_rf_r2_mixstat.tif",
  overwrite = TRUE
)

writeRaster(ECC_rf_r3_mixstat,
  "E:data-raw/dist_fit_vic/ECC_rf_r3_mixstat.tif",
  overwrite = TRUE
)

writeRaster(ECC_rf_hist_mixstat,
  "E:data-raw/dist_fit_vic/ECC_rf_hist_mixstat.tif",
  overwrite = TRUE
)


mean_ecc_rf_r1_mixstat <- mean(ECC_rf_r1_mixstat, na.rm = TRUE)
mean_ecc_rf_r2_mixstat <- mean(ECC_rf_r2_mixstat, na.rm = TRUE)
mean_ecc_rf_r3_mixstat <- mean(ECC_rf_r3_mixstat, na.rm = TRUE)
mean_ecc_rf_hist_mixstat <- mean(ECC_rf_hist_mixstat, na.rm = TRUE)



diff_event_r1_mixstat <- (mean_ecc_rf_r1_mixstat - mean_ecc_rf_hist_mixstat) /
  mean_ecc_rf_hist_mixstat

diff_event_r2_mixstat <- (mean_ecc_rf_r2_mixstat - mean_ecc_rf_hist_mixstat) /
  mean_ecc_rf_hist_mixstat

diff_event_r3_mixstat <- (mean_ecc_rf_r3_mixstat - mean_ecc_rf_hist_mixstat) /
  mean_ecc_rf_hist_mixstat

diff_event_r_mixstat <- mean(c(diff_event_r1_mixstat , diff_event_r2_mixstat ,
  diff_event_r3_mixstat), na.rm = TRUE)


ggplot() +
  geom_spatraster_contour_filled(
    data = diff_event_r_mixstat,
    breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
  ) +
  scale_fill_manual(
    name = "MRI Percent \n Change",
    values = c(
      "#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3",
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


# fit dist and estimate MRI (mean is varying for future scenarios)
ECC_rf_r1_mixstat_full <- terra::app(rf_r1_full,
  fun = gev_fit_stat_nonstat, cores = 12
)

ECC_rf_r2_mixstat_full <- terra::app(rf_r2_full,
                                     fun = gev_fit_stat_nonstat, cores = 12
)

ECC_rf_r3_mixstat_full <- terra::app(rf_r3_full,
                                     fun = gev_fit_stat_nonstat, cores = 12
)

ECC_rf_hist_mixstat_full <- terra::app(rf_hist,
  fun = gev_fit_stat_nonstat, cores = 12
)


writeRaster(ECC_rf_r1_mixstat_full,
  "E:data-raw/dist_fit_vic/ECC_rf_r1_mixstat_full.tif",
  overwrite = TRUE
)

writeRaster(ECC_rf_r2_mixstat_full,
  "E:data-raw/dist_fit_vic/ECC_rf_r2_mixstat_full.tif",
  overwrite = TRUE
)

writeRaster(ECC_rf_r3_mixstat_full,
  "E:data-raw/dist_fit_vic/ECC_rf_r3_mixstat_full.tif",
  overwrite = TRUE
)

writeRaster(ECC_rf_hist_mixstat_full,
            "E:data-raw/dist_fit_vic/ECC_rf_hist_mixstat_full.tif",
            overwrite = TRUE
)

mean_ecc_rf_r1_mixstat_full <- mean(ECC_rf_r1_mixstat_full, na.rm = TRUE)
mean_ecc_rf_r2_mixstat_full <- mean(ECC_rf_r2_mixstat_full, na.rm = TRUE)
mean_ecc_rf_r3_mixstat_full <- mean(ECC_rf_r3_mixstat_full, na.rm = TRUE)
mean_ecc_rf_hist_mixstat_full <- mean(ECC_rf_hist_mixstat_full, na.rm = TRUE)



diff_event_r1_mixstat_full <- (mean_ecc_rf_r1_mixstat_full -
                   mean_ecc_rf_hist_mixstat_full) / mean_ecc_rf_hist_mixstat_full

diff_event_r2_mixstat_full <- (mean_ecc_rf_r2_mixstat_full -
                                 mean_ecc_rf_hist_mixstat_full) / mean_ecc_rf_hist_mixstat_full
diff_event_r3_mixstat_full <- (mean_ecc_rf_r3_mixstat_full -
                                 mean_ecc_rf_hist_mixstat_full) / mean_ecc_rf_hist_mixstat_full


diff_event_r_mixstat_full <- mean(c(diff_event_r1_mixstat_full , 
                                    diff_event_r2_mixstat_full ,
                                    diff_event_r3_mixstat_full), na.rm = TRUE)

ggplot() +
  geom_spatraster_contour_filled(
    data = diff_event_r_mixstat_full,
    breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
  ) +
  scale_fill_manual(
    name = "MRI Percent \n Change",
    values = c(
      "#bf812d", "#dfc27d", "#f6e8c3",
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
