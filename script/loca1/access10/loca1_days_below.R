pacman::p_load(terra)

hist_days_below <- terra::rast("E:/data-raw/loca1/access10/hist_days_below.tif")
rcp45_days_below <- terra::rast("E:/data-raw/loca1/access10/rcp45_days_below.tif")
rcp85_days_below <- terra::rast("E:/data-raw/loca1/access10/rcp85_days_below.tif")

hist_s_loca1 <- terra::rast("E:/data-raw/loca1/access10/summary/hist_s_loca1.tif")
rcp45_s_loca1 <- terra::rast("E:/data-raw/loca1/access10/summary/rcp45_s_loca1.tif")
rcp85_s_loca1 <- terra::rast("E:/data-raw/loca1/access10/summary/rcp85_s_loca1.tif")

# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)


# ============================================================================#
# monthly summary
# ============================================================================#

mean_hist_days_below <- mean(hist_days_below, na.rm = TRUE)
mean_rcp45_days_below <- mean(rcp45_days_below, na.rm = TRUE)
mean_rcp85_days_below <- mean(rcp85_days_below, na.rm = TRUE)



prism_mask_loca1 <- rast("data-raw/mask/prism_mask_loca1.tif")
mean_hist_days_below <- terra::mask(mean_hist_days_below, prism_mask_loca1)
mean_rcp45_days_below <- terra::mask(mean_rcp45_days_below, prism_mask_loca1)
mean_rcp85_days_below <- terra::mask(mean_rcp85_days_below, prism_mask_loca1)


plot(mean_rcp45_days_below-mean_hist_days_below, breaks = c(-100,-80,-60,-40,-20,0))
plot(mean_rcp85_days_below-mean_hist_days_below, breaks = c(-100,-80,-60,-40,-20,0))

plot(mean_rcp45_days_below, breaks = c(0, 25, 50, 75, 100,125, 150, 175, 200))
plot(mean_rcp85_days_below, breaks = c(0, 25, 50, 75, 100,125, 150, 175, 200))



snowfall_days_r45 = rcp45_s_loca1 > 0
snowfall_days_r45t = sum(snowfall_days_r45)
