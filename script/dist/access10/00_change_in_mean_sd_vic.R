################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
pacman::p_load(
  fitdistrplus, terra, extRemes, evd,
  ismev, trend
)


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

set.seed(275349)

# ============================================================================#
# load the data
# ============================================================================#

vic_hist <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")
r45 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp45_access10.tif")
r85 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp85_access10.tif")

# vic_swe_access10_median = median(vic_hist, na.rm =TRUE)
# vic_swe_access10_median <- terra::mask(vic_swe_access10_median, prism_mask_vic)
# 
# terra::writeRaster(vic_swe_access10_median,
#                    filename = "E:/data-raw/vic_swe_access10_median.tif",
#                    overwrite = TRUE)
# ==============================================================================
# get USA map with state boundaries
# ==============================================================================

conus <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

conus <- terra::vect(conus)


################################################################################
## STEP 1: r1
################################################################################

hist_mean <- mean(vic_hist, na.rm = TRUE)
hist_sd <- stdev(vic_hist, na.rm = TRUE)

r45_mean <- mean(r45, na.rm = TRUE)
r85_mean <- mean(r85, na.rm = TRUE)

r45_sd <- stdev(r45, na.rm = TRUE)
r85_sd <- stdev(r85, na.rm = TRUE)

r45_mean_diff <- r45_mean - hist_mean
r85_mean_diff <- r85_mean - hist_mean


r45_sd_diff <- r45_sd - hist_sd
r85_sd_diff <- r85_sd - hist_sd


prism_mask_vic <- rast("data-raw/mask/prism_mask_vic.tif")

r45_mean_diff <- terra::mask(r45_mean_diff, prism_mask_vic)
r85_mean_diff <- terra::mask(r85_mean_diff, prism_mask_vic)
r45_sd_diff <- terra::mask(r45_sd_diff, prism_mask_vic)
r85_sd_diff <- terra::mask(r85_sd_diff, prism_mask_vic)


par(mfcol = c(1, 2), mar = c(5, 4, 4, 5) + 0.1)
plot(r45_mean_diff, breaks = c(-1000, -250, -50, -25, 0, 25, 50, 250, 1000))
plot(r45_sd_diff, breaks = c(-100, -50, -25, 0, 25, 50, 250, 600))
par(mfrow = c(1, 1))

plot(r45_mean_diff, breaks = c(-1000, -250, -50, -25, 0, 25, 50, 250, 1000))
plot(r45_sd_diff, breaks = c(-100, -50, -25, 0, 25, 50, 250, 600))
plot(r85_mean_diff, breaks = c(-1000, -250, -50, -25, 0, 25, 50, 250, 1000))
plot(r85_sd_diff, breaks = c(-100, -50, -25, 0, 25, 50, 250, 600))

#=============================================================================#
# RCP45
#=============================================================================#

ggplot() +
  geom_spatraster_contour_filled(
    data = r45_mean_diff,
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
  ggtitle('ACCESS10: RCP45 MEAN CHANGE') +
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
    data = r45_sd_diff,
    breaks = c(-600, -250, -50, -25, 0, 25, 50, 250, 600)
  ) +
  scale_fill_manual(
    name = "SD Change",
    values = c(
      "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3",
      "#c7eae5", "#80cdc1", "#35978f", "#01665e"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle('ACCESS10: RCP45 SD CHANGE') +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    legend.position = c(0.9, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  )


#=============================================================================#
# RCP85
#=============================================================================#

ggplot() +
  geom_spatraster_contour_filled(
    data = r85_mean_diff,
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
  ggtitle('ACCESS10: RCP85 MEAN CHANGE') +
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
    data = r85_sd_diff,
    breaks = c(-600, -250, -50, -25, 0, 25, 50, 250, 600)
  ) +
  scale_fill_manual(
    name = "SD Change",
    values = c(
     "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3",
      "#c7eae5", "#80cdc1", "#35978f", "#01665e"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle('ACCESS10: RCP85 SD CHANGE') +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    legend.position = c(0.9, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  )

