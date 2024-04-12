################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
pacman::p_load(
  fitdistrplus, terra, extRemes, evd,
  ismev, trend, tidyterra, scales, geodata, tidyverse
)


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

set.seed(275349)

# ==============================================================================
# get USA map with state boundaries
# ==============================================================================

conus <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

conus <- terra::vect(conus)


# ============================================================================#
# load the data
# ============================================================================#

loca2_swe_hist <- rast(list.files("E:/data-raw/swe_model_vars/ssp585/hist/prediction/rf/",
                       pattern = "\\.tif$", full.names = TRUE
))


loca2_swe_r1 <- rast(list.files("E:/data-raw/swe_model_vars/ssp585/r1/prediction/rf/",
                     pattern = "\\.tif$", full.names = TRUE
))

loca2_swe_r2 <- rast(list.files("E:/data-raw/swe_model_vars/ssp585/r2/prediction/rf/",
                     pattern = "\\.tif$", full.names = TRUE
))

loca2_swe_r3 <- rast(list.files("E:/data-raw/swe_model_vars/ssp585/r3/prediction/rf/",
                     pattern = "\\.tif$", full.names = TRUE
))


vic_swe_hist <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")
vic_swe_r85 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp85_access10.tif")


################################################################################
## STEP 1: CLEAN DATA FoOR PLOTTING
################################################################################

# ============================================================================#
# HINDCAST
# ============================================================================#
loca2_swe_hist =subset(loca2_swe_hist, time(loca2_swe_hist) %in% time(vic_swe_hist))

loca2_swe_hist = project(loca2_swe_hist, vic_swe_hist[[1]] )


diff_hist = vic_swe_hist - loca2_swe_hist
mean_diff_hist = mean(diff_hist, na.rm =TRUE)

# ============================================================================#
# FUTURE
# ============================================================================#
loca2_swe_future =mean(loca2_swe_r1, loca2_swe_r2, loca2_swe_r3)

loca2_swe_future =subset(loca2_swe_future, time(loca2_swe_future) %in% time(vic_swe_r85))
vic_swe_r85 =subset(vic_swe_r85, time(vic_swe_r85) %in% time(loca2_swe_future))

loca2_swe_future = project(loca2_swe_future, vic_swe_r85[[1]] )

diff_future = vic_swe_r85 - loca2_swe_future
mean_diff_future = mean(diff_future, na.rm =TRUE)



################################################################################
## STEP 2: plot
################################################################################

ggplot() +
  geom_spatraster_contour_filled(
    data = mean_diff_hist,
    breaks = c(-1000, -250, -50, -25, 0, 25, 50, 250, 1000)
  ) +
  scale_fill_manual(
    name = "Mean Change",
    values = c(
      "#b35806", "#e08214", "#fdb863", "#fee0b6",
      "#d8daeb", "#b2abd2", "#8073ac", "#542788"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle('HIST MEAN DIFF: ACCESS10 VS LOCA2 SWE') +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    legend.position = c(0.9, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  )



ggplot() +
  geom_spatraster_contour_filled(
    data = mean_diff_future,
    breaks = c(-1000, -250, -50, -25, 0, 25, 50, 250, 1000)
  ) +
  scale_fill_manual(
    name = "Mean Change",
    values = c(
      "#b35806", "#e08214", "#fdb863", "#fee0b6",
      "#d8daeb", "#b2abd2", "#8073ac", "#542788"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle('FUTURE MEAN DIFF: ACCESS10 VS LOCA2 SWE') +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    legend.position = c(0.9, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  )
