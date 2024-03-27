pacman::p_load(terra)

hist_s_loca1 <- terra::rast("E:/data-raw/loca1/access10/summary/hist_s_loca1.tif")
rcp45_s_loca1 <- terra::rast("E:/data-raw/loca1/access10/summary/rcp45_s_loca1.tif")
rcp85_s_loca1 <- terra::rast("E:/data-raw/loca1/access10/summary/rcp85_s_loca1.tif")


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)


# ============================================================================#
# monthly summary
# ============================================================================#

hist_s_monthly <- tapp(hist_s_loca1, index = "months", fun = mean, cores = 12)
rcp45_s_monthly <- tapp(rcp45_s_loca1, index = "months", fun = mean, cores = 12)
rcp85_s_monthly <- tapp(rcp85_s_loca1, index = "months", fun = mean, cores = 12)


prism_mask_loca1 <- rast("data-raw/mask/prism_mask_loca1.tif")
hist_s_monthly <- terra::mask(hist_s_monthly, prism_mask_loca1)
rcp45_s_monthly <- terra::mask(rcp45_s_monthly, prism_mask_loca1)
rcp85_s_monthly <- terra::mask(rcp85_s_monthly, prism_mask_loca1)

hist_s_monthly <- hist_s_monthly * 86400
rcp45_s_monthly <- rcp45_s_monthly * 86400
rcp85_s_monthly <- rcp85_s_monthly * 86400


writeRaster(hist_s_monthly,
  "E:/data-raw/loca1/access10/summary/hist_s_monthly.tif",
  overwrite = TRUE
)


writeRaster(rcp45_s_monthly,
  "E:/data-raw/loca1/access10/summary/rcp45_s_monthly.tif",
  overwrite = TRUE
)


writeRaster(rcp85_s_monthly,
  "E:/data-raw/loca1/access10/summary/rcp85_s_monthly.tif",
  overwrite = TRUE
)

hist_s_monthly = rast("E:/data-raw/loca1/access10/summary/hist_s_monthly.tif")
rcp45_s_monthly = rast("E:/data-raw/loca1/access10/summary/rcp45_s_monthly.tif")
rcp85_s_monthly = rast("E:/data-raw/loca1/access10/summary/rcp85_s_monthly.tif")


# December Comparison Plots
par(mfrow = c(1, 2))
plot(rcp45_s_monthly[["m_12"]] - hist_s_monthly[["m_12"]],
  main = "RCP 4.5 - Snowfall Historical (December)"
)
plot(rcp85_s_monthly[["m_12"]] - hist_s_monthly[["m_12"]],
  main = "RCP 8.5 - Snowfall Historical (December)"
)
par(mfrow = c(1, 1))

# January Comparison Plots
par(mfrow = c(1, 2))
plot(rcp45_s_monthly[["m_1"]] - hist_s_monthly[["m_1"]],
  main = "RCP 4.5 - Snowfall Historical (January)"
)
plot(rcp85_s_monthly[["m_1"]] - hist_s_monthly[["m_1"]],
  main = "RCP 8.5 - Snowfall Historical (January)"
)
par(mfrow = c(1, 1))

# February Comparison Plots
par(mfrow = c(1, 2))
plot(rcp45_s_monthly[["m_2"]] - hist_s_monthly[["m_2"]],
  main = "RCP 4.5 - Snowfall Historical (February)"
)
plot(rcp85_s_monthly[["m_2"]] - hist_s_monthly[["m_2"]],
  main = "RCP 8.5 - Snowfall Historical (February)"
)
par(mfrow = c(1, 1))





# ============================================================================#
# winter summary
# ============================================================================#
hist_s_winter_mean <- mean(hist_s_loca1, na.rm = TRUE)
rcp45_s_winter_mean <- mean(rcp45_s_loca1, na.rm = TRUE)
rcp85_s_winter_mean <- mean(rcp85_s_loca1, na.rm = TRUE)

prism_mask_loca1 <- rast("data-raw/mask/prism_mask_loca1.tif")
hist_s_winter_mean <- terra::mask(hist_s_winter_mean, prism_mask_loca1)
rcp45_s_winter_mean <- terra::mask(rcp45_s_winter_mean, prism_mask_loca1)
rcp85_s_winter_mean <- terra::mask(rcp85_s_winter_mean, prism_mask_loca1)


hist_s_winter_mean <- hist_s_winter_mean * 86400
rcp45_s_winter_mean <- rcp45_s_winter_mean * 86400
rcp85_s_winter_mean <- rcp85_s_winter_mean * 86400

writeRaster(hist_s_winter_mean,
            "E:/data-raw/loca1/access10/summary/hist_s_winter_mean.tif",
            overwrite = TRUE
)

writeRaster(rcp45_s_winter_mean,
            "E:/data-raw/loca1/access10/summary/rcp45_s_winter_mean.tif",
            overwrite = TRUE
)

writeRaster(rcp85_s_winter_mean,
            "E:/data-raw/loca1/access10/summary/rcp85_s_winter_mean.tif",
            overwrite = TRUE
)



hist_s_winter_mean = rast("E:/data-raw/loca1/access10/summary/hist_s_winter_mean.tif")
rcp85_s_winter_mean = rast("E:/data-raw/loca1/access10/summary/rcp85_s_winter_mean.tif")
rcp45_s_winter_mean = rast("E:/data-raw/loca1/access10/summary/rcp45_s_winter_mean.tif")

par(mfrow = c(1, 2))
plot(rcp45_s_winter_mean - hist_s_winter_mean,
  main = "RCP 4.5 - Snowfall Winter Mean"
)
plot(rcp85_s_winter_mean - hist_s_winter_mean,
  main = "RCP 8.5 - Snowfall Winter Mean"
)
par(mfrow = c(1, 1))
