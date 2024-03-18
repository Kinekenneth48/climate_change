pacman::p_load(terra)


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

rcp45_tmean_loca1 <- terra::rast("E:/data-raw/loca1/rcp45_tmean_loca1.tif")
rcp85_tmean_loca1 <- terra::rast("E:/data-raw/loca1/rcp85_tmean_loca1.tif")
hist_tmean_loca1 <- terra::rast("E:/data-raw/loca1/hist_tmean_loca1.tif")

# ============================================================================#
# monthly summary
# ============================================================================#

rcp45_tmean_monthly <- tapp(rcp45_tmean_loca1, index = "months", fun = mean, cores = 12)
rcp85_tmean_monthly <- tapp(rcp85_tmean_loca1, index = "months", fun = mean, cores = 12)
hist_tmean_monthly <- tapp(hist_tmean_loca1, index = "months", fun = mean, cores = 12)

prism_mask_loca1 <- rast("data-raw/mask/prism_mask_loca1.tif")
rcp45_tmean_monthly <- terra::mask(rcp45_tmean_monthly, prism_mask_loca1)
rcp85_tmean_monthly <- terra::mask(rcp85_tmean_monthly, prism_mask_loca1)
hist_tmean_monthly <- terra::mask(hist_tmean_monthly, prism_mask_loca1)


writeRaster(rcp45_tmean_monthly,
  "E:/data-raw/loca1/summary/rcp45_tmean_monthly.tif",
  overwrite = TRUE
)

writeRaster(rcp85_tmean_monthly,
  "E:/data-raw/loca1/summary/rcp85_tmean_monthly.tif",
  overwrite = TRUE
)

writeRaster(hist_tmean_monthly,
  "E:/data-raw/loca1/summary/hist_tmean_monthly.tif",
  overwrite = TRUE
)

library(terra)

# Open a PDF device to capture all plots
pdf("monthly_summary_loca1_tmean.pdf", width=11, height=8.5)

# December Comparison Plots
par(mfrow = c(1, 2))
plot(rcp45_tmean_monthly[[12]] - hist_tmean_monthly[[12]],
     main = "RCP 4.5 - Historical (December)")
plot(rcp85_tmean_monthly[[12]] - hist_tmean_monthly[[12]],
     main = "RCP 8.5 - Historical (December)")
par(mfrow = c(1, 1))

# January Comparison Plots
par(mfrow = c(1, 2))
plot(rcp45_tmean_monthly[[1]] - hist_tmean_monthly[[1]],
     main = "RCP 4.5 - Historical (January)")
plot(rcp85_tmean_monthly[[1]] - hist_tmean_monthly[[1]],
     main = "RCP 8.5 - Historical (January)")
par(mfrow = c(1, 1))

# February Comparison Plots
par(mfrow = c(1, 2))
plot(rcp45_tmean_monthly[[2]] - hist_tmean_monthly[[2]],
     main = "RCP 4.5 - Historical (February)")
plot(rcp85_tmean_monthly[[2]] - hist_tmean_monthly[[2]],
     main = "RCP 8.5 - Historical (February)")
par(mfrow = c(1, 1))

# Close the PDF device
dev.off()


# ============================================================================#
# winter summary
# ============================================================================#
dates_rcp <- time(rcp45_tmean_loca1)
dates_hist <- time(hist_tmean_loca1)

# Identify winter days (Dec, Jan, Feb) for each year
is_winter_rcp <- format(dates_rcp, "%m") %in% c("12", "01", "02")
is_winter_hist <- format(dates_hist, "%m") %in% c("12", "01", "02")

# Generate an index for winter days
winter_index_rcp <- which(is_winter_rcp)
winter_index_hist <- which(is_winter_hist)

# Apply tapp to calculate the mean temperature for winter days
rcp45_tmean_winter <- tapp(rcp45_tmean_loca1,
  index = winter_index_rcp, fun = "mean", cores = 12
)

rcp85_tmean_winter <- tapp(rcp85_tmean_loca1,
  index = winter_index_rcp, fun = "mean", cores = 12
)

hist_tmean_winter <- tapp(hist_tmean_loca1,
  index = winter_index_hist, fun = "mean", cores = 12
)



writeRaster(rcp45_tmean_winter,
  "E:/data-raw/loca1/summary/rcp45_tmean_winter.tif",
  overwrite = TRUE
)

writeRaster(rcp85_tmean_winter,
  "E:/data-raw/loca1/summary/rcp85_tmean_winter.tif",
  overwrite = TRUE
)

writeRaster(hist_tmean_winter,
  "E:/data-raw/loca1/summary/hist_tmean_winter.tif",
  overwrite = TRUE
)

loca_r45_tmean_winter_avg <-
  terra::rast("E:/data-raw/loca1/summary/loca_r45_tmean_winter_avg.tif")

loca_r85_tmean_winter_avg <-
  terra::rast("E:/data-raw/loca1/summary/loca_r85_tmean_winter_avg.tif")

loca_hist_tmean_winter_avg <-
  terra::rast("E:/data-raw/loca1/summary/loca_hist_tmean_winter_avg.tif")

loca_hist_tmean_winter_avg <- terra::mask(loca_hist_tmean_winter_avg, prism_mask_loca1)
loca_r45_tmean_winter_avg <- terra::mask(loca_r45_tmean_winter_avg, prism_mask_loca1)
loca_r85_tmean_winter_avg <- terra::mask(loca_r85_tmean_winter_avg, prism_mask_loca1)


pdf("winter_summary_loca1_tmean.pdf", width=11, height=8.5)


par(mfrow = c(1, 2))

# Plot the difference in winter averages between RCP 4.5 and historical data
plot(loca_r45_tmean_winter_avg - loca_hist_tmean_winter_avg, 
     main = "RCP 4.5 - Historical Winter Avg")

# Plot the difference in winter averages between RCP 8.5 and historical data
plot(loca_r85_tmean_winter_avg - loca_hist_tmean_winter_avg, 
     main = "RCP 8.5 - Historical Winter Avg")


par(mfrow = c(1, 1))
dev.off()
