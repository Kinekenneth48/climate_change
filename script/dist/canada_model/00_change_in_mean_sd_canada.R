################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
pacman::p_load(
  fitdistrplus, terra, extRemes, evd,
  ismev, trend, tidyverse, tidyterra
)


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

set.seed(275349)

# ============================================================================#
# load the data
# ============================================================================#

r1 <- terra::rast("E:/data-raw/canada_model/raster/snw/r1/r1_raster.tif")
r2 <- terra::rast("E:/data-raw/canada_model/raster/snw/r2/r2_raster.tif")
r3 <- terra::rast("E:/data-raw/canada_model/raster/snw/r3/r3_raster.tif")
r4 <- terra::rast("E:/data-raw/canada_model/raster/snw/r4/r4_raster.tif")
r5 <- terra::rast("E:/data-raw/canada_model/raster/snw/r5/r5_raster.tif")

raster <- mean(r1, r2, r3, r4, r5)

r1_hist =  subset(r1, time(r1) <= 2000)
r2_hist =  subset(r2, time(r2) <= 2000)
r3_hist =  subset(r3, time(r3) <= 2000)
r4_hist =  subset(r4, time(r4) <= 2000)
r5_hist =  subset(r5, time(r5) <= 2000)
hist = mean(r1_hist, r2_hist, r3_hist,r4_hist,r5_hist)

r1_future =  subset(r1, time(r1) > 2000)
r2_future =  subset(r2, time(r2) >  2000)
r3_future =  subset(r3, time(r3) >  2000)
r4_future =  subset(r4, time(r4) > 2000)
r5_future =  subset(r5, time(r5) >  2000)
future = mean(r1_future, r2_future, r3_future,r4_future,r5_future)



# ==============================================================================
# get USA map with state boundaries
# ==============================================================================

conus <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

conus <- terra::vect(conus)


################################################################################
## STEP 1: r1 -r5
################################################################################

hist_mean <- mean(hist, na.rm = TRUE)
hist_sd <- stdev(hist, na.rm = TRUE)

future_mean <- mean(future, na.rm = TRUE)
future_sd <- stdev(future, na.rm = TRUE)


mean_diff <- future_mean - hist_mean
sd_diff <- future_sd - hist_sd

plot(mean_diff, breaks = c(-1000, -250, -50, -25, 0, 25, 50))
plot(conus, add = TRUE)


plot(sd_diff, breaks = c(-600, -250, -50, -25, 0, 25, 50, 250, 600))
plot(conus, add = TRUE)
# =============================================================================#
# plot
# =============================================================================#
# conus = project(conus,mean_diff )

ggplot() +
  geom_spatraster_contour_filled(
    data = mean_diff,
    breaks = c(-1000, -250, -50, -25, 0, 25, 50)
  ) +
  scale_fill_manual(
    name = "Mean Change",
    values = c(
      "#b35806", "#e08214", "#fdb863", "#fee0b6",
      "#d8daeb", "#b2abd2"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  coord_sf(crs = 4326) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("CCSM4: MEAN CHANGE") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  theme(
    legend.position = c(0.9, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  )

ggplot() +
  geom_spatraster_contour_filled(
    data = sd_diff,
    breaks = c(-600, -250, -50, -25, 0, 25, 50, 250, 600)
  ) +
  scale_fill_manual(
    name = "SD Change",
    values = c(
      # "#bf812d",
      # "#dfc27d",
      "#f6e8c3",
      "#c7eae5", "#80cdc1", "#35978f", "#01665e"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("CCSM4:  SD CHANGE") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    legend.position = c(0.9, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  )
