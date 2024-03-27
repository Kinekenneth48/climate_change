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
vic_r45 <- terra::rast("E:/data-raw/NCAR/CCSM4/future/max_swe_vic_rcp45_ccsm4.tif")
vic_r85 <- terra::rast("E:/data-raw/NCAR/CCSM4/future/max_swe_vic_rcp85_ccsm4.tif")
vic_hist <- terra::rast("E:/data-raw/NCAR/CCSM4/historical/max_swe_vic_hist_ccsm4.tif")


# ==============================================================================
# get USA map with state boundaries
# ==============================================================================

conus <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

conus <- terra::vect(conus)


################################################################################
## STEP 1: check for Stationarity
################################################################################

# mk test
mk_vic_hist <- terra::app(vic_hist, fun = mk)
mk_vic_r45 <- terra::app(vic_r45, fun = mk)
mk_vic_r85 <- terra::app(vic_r85, fun = mk)

cls <- data.frame(id = 0:1, cover = c("stationary", "non-stationary"))
levels(mk_vic_hist) <- cls
levels(mk_vic_r45) <- cls
levels(mk_vic_r85) <- cls

mk_vic <- c(mk_vic_hist, mk_vic_r45, mk_vic_r85)
names(mk_vic) <- c("hindcast", "rcp45", "rcp85")

# ============================================================================#
# create/load a mask
# ============================================================================#

prism_mask_vic <- terra::rast("data-raw/mask/prism_mask_vic.tif")


# ============================================================================#
# plot stat or non-stat(check for Stationarity)
# ============================================================================#
mk_vic <- terra::mask(mk_vic, prism_mask_vic)


ggplot() +
  geom_spatraster(
    data = mk_vic
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
    legend.position = c(0.85, 0.23),
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
ECC_vic_r45_mixstat_ccsm4 <- terra::app(vic_r45,
  fun = gev_fit_stat_nonstat, cores = 12
)

ECC_vic_r85_mixstat_ccsm4 <- terra::app(vic_r85,
  fun = gev_fit_stat_nonstat, cores = 12
)

ECC_vic_hist_mixstat_ccsm4 <- terra::app(vic_hist,
  fun = gev_fit_stat_nonstat, cores = 12
)


writeRaster(ECC_vic_r45_mixstat_ccsm4,
  "E:data-raw/dist_fit_vic/ccsm4/ECC_vic_r45_mixstat_ccsm4.tif",
  overwrite = TRUE
)

writeRaster(ECC_vic_r85_mixstat_ccsm4,
  "E:data-raw/dist_fit_vic/ccsm4/ECC_vic_r85_mixstat_ccsm4.tif",
  overwrite = TRUE
)

writeRaster(ECC_vic_hist_mixstat_ccsm4,
  "E:data-raw/dist_fit_vic/ccsm4/ECC_vic_hist_mixstat_ccsm4.tif",
  overwrite = TRUE
)

ECC_vic_hist_mixstat_ccsm4 <-
  rast("E:data-raw/dist_fit_vic/ccsm4/ECC_vic_hist_mixstat_ccsm4.tif")

ECC_vic_r85_mixstat_ccsm4 <-
  rast("E:data-raw/dist_fit_vic/ccsm4/ECC_vic_r85_mixstat_ccsm4.tif")

ECC_vic_r45_mixstat_ccsm4 <-
  rast("E:data-raw/dist_fit_vic/ccsm4/ECC_vic_r45_mixstat_ccsm4.tif")

mean_ecc_r45_mixstat_ccsm4 <- mean(ECC_vic_r45_mixstat_ccsm4, na.rm = TRUE)
mean_ecc_hist_mixstat_ccsm4 <- mean(ECC_vic_hist_mixstat_ccsm4, na.rm = TRUE)
mean_ecc_r85_mixstat_ccsm4 <- mean(ECC_vic_r85_mixstat_ccsm4, na.rm = TRUE)

prism_mask_vic <- rast("data-raw/mask/prism_mask_vic.tif")

diff_event_r45_mixstat_ccsm4 <- (mean_ecc_r45_mixstat_ccsm4 - mean_ecc_hist_mixstat_ccsm4) /
  mean_ecc_hist_mixstat_ccsm4
diff_event_r85_mixstat_ccsm4 <- (mean_ecc_r85_mixstat_ccsm4 - mean_ecc_hist_mixstat_ccsm4) /
  mean_ecc_hist_mixstat_ccsm4

diff_event_r45_mixstat_ccsm4 <- terra::mask(diff_event_r45_mixstat_ccsm4, prism_mask_vic)
diff_event_r85_mixstat_ccsm4 <- terra::mask(diff_event_r85_mixstat_ccsm4, prism_mask_vic)

ggplot() +
  geom_spatraster_contour_filled(
    data = diff_event_r45_mixstat_ccsm4,
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

ggplot() +
  geom_spatraster_contour_filled(
    data = diff_event_r85_mixstat_ccsm4,
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
  ggtitle("ccsm4 R45 FULL") +
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
diff_r45_r85 <- c(diff_event_r45_mixstat_ccsm4, diff_event_r85_mixstat_ccsm4)
names(diff_r45_r85) <- c("r45", "r85")


ggplot() +
  geom_spatraster_contour_filled(
    data = diff_r45_r85,
    breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
  ) +
  facet_wrap(~lyr, nrow = 2) +
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
  ggtitle("ccsm4: MRI Percent Change (MS method)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    # legend.position = c(0.95, 0.4),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 30)
  )




################################################################################
## STEP 3: Fit dist (HIST- STAT: FUTURE-NONSTAT) full
################################################################################
vic_r45_full <- c(vic_hist, vic_r45)
vic_r85_full <- c(vic_hist, vic_r85)



# fit dist and estimate MRI (mean is varying for future scenarios)
ECC_vic_r45_mixstat_full_ccsm4 <- terra::app(vic_r45_full,
  fun = gev_fit_stat_nonstat, cores = 12
)

ECC_vic_r85_mixstat_full_ccsm4 <- terra::app(vic_r85_full,
  fun = gev_fit_stat_nonstat, cores = 12
)

ECC_vic_hist_mixstat_full_ccsm4 <- terra::app(vic_hist,
  fun = gev_fit_stat_nonstat, cores = 12
)


writeRaster(ECC_vic_r45_mixstat_full_ccsm4,
  "E:data-raw/dist_fit_vic/ccsm4/ECC_vic_r45_mixstat_full_ccsm4.tif",
  overwrite = TRUE
)

writeRaster(ECC_vic_r85_mixstat_full_ccsm4,
  "E:data-raw/dist_fit_vic/ccsm4/ECC_vic_r85_mixstat_full_ccsm4.tif",
  overwrite = TRUE
)

writeRaster(ECC_vic_hist_mixstat_full_ccsm4,
  "E:data-raw/dist_fit_vic/ccsm4/ECC_vic_hist_mixstat_full_ccsm4.tif",
  overwrite = TRUE
)


ECC_vic_r45_mixstat_full_ccsm4 <-
  rast("E:data-raw/dist_fit_vic/ccsm4/ECC_vic_r45_mixstat_full_ccsm4.tif")

ECC_vic_r85_mixstat_full_ccsm4 <-
  rast("E:data-raw/dist_fit_vic/ccsm4/ECC_vic_r85_mixstat_full_ccsm4.tif")

ECC_vic_hist_mixstat_full_ccsm4 <-
  rast("E:data-raw/dist_fit_vic/ccsm4/ECC_vic_hist_mixstat_full_ccsm4.tif")

mean_ecc_r45_mixstat_full_ccsm4 <- mean(ECC_vic_r45_mixstat_full_ccsm4, na.rm = TRUE)
mean_ecc_hist_mixstat_full_ccsm4 <- mean(ECC_vic_hist_mixstat_full_ccsm4, na.rm = TRUE)
mean_ecc_r85_mixstat_full_ccsm4 <- mean(ECC_vic_r85_mixstat_full_ccsm4, na.rm = TRUE)

prism_mask_vic <- rast("data-raw/mask/prism_mask_vic.tif")

diff_event_r45_mixstat_full_ccsm4 <- (mean_ecc_r45_mixstat_full_ccsm4 -
  mean_ecc_hist_mixstat_full_ccsm4) / mean_ecc_hist_mixstat_full_ccsm4

diff_event_r85_mixstat_full_ccsm4 <- (mean_ecc_r85_mixstat_full_ccsm4 -
  mean_ecc_hist_mixstat_full_ccsm4) / mean_ecc_hist_mixstat_full_ccsm4

diff_event_r45_mixstat_full_ccsm4 <- terra::mask(diff_event_r45_mixstat_full_ccsm4, prism_mask_vic)
diff_event_r85_mixstat_full_ccsm4 <- terra::mask(diff_event_r85_mixstat_full_ccsm4, prism_mask_vic)

ggplot() +
  geom_spatraster_contour_filled(
    data = diff_event_r45_mixstat_full_ccsm4,
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
  ggtitle("ccsm4 R45") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    legend.position = c(0.91, 0.28),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  )

ggplot() +
  geom_spatraster_contour_filled(
    data = diff_event_r85_mixstat_full_ccsm4,
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
  ggtitle("ccsm4 R85 FULL") +
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
diff_r45_r85 <- c(diff_event_r45_mixstat_full_ccsm4, diff_event_r85_mixstat_full_ccsm4)
names(diff_r45_r85) <- c("r45", "r85")


ggplot() +
  geom_spatraster_contour_filled(
    data = diff_r45_r85,
    breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
  ) +
  facet_wrap(~lyr, nrow = 2) +
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
  ggtitle("ccsm4: MRI Percent Change -FULL (MS method)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    # legend.position = c(0.95, 0.4),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 30)
  )
