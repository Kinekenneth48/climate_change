pacman::p_load(terra)

rcp45_pr_loca1 = terra::rast("E:/data-raw/loca1/rcp45_pr_loca1.tif")
rcp85_pr_loca1 = terra::rast("E:/data-raw/loca1/rcp85_pr_loca1.tif")
hist_pr_loca1 = terra::rast("E:/data-raw/loca1/hist_pr_loca1.tif")


rcp45_tmean_loca1 <- terra::rast("E:/data-raw/loca1/rcp45_tmean_loca1.tif")
rcp85_tmean_loca1 <- terra::rast("E:/data-raw/loca1/rcp85_tmean_loca1.tif")
hist_tmean_loca1 <- terra::rast("E:/data-raw/loca1/hist_tmean_loca1.tif")

# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

# set temp threshold to classify pr as snow (3 cel is 276.15K)
temp_threshold <- 276.15

# ============================================================================#
# get winter month data
# ============================================================================#
dates_rcp <- time(rcp45_tmean_loca1)
dates_hist <- time(hist_tmean_loca1)

# Identify winter days (Dec, Jan, Feb, March) for each year
is_winter_rcp <- format(dates_rcp, "%m") %in% c("12", "01", "02", "03")
is_winter_hist <- format(dates_hist, "%m") %in% c("12", "01", "02", "03")

# Filter dates to keep only winter
winter_index_rcp <- which(is_winter_rcp)
winter_index_hist <- which(is_winter_hist)

# Extract layers corresponding to winter dates
rcp45_tmean_winter <- rcp45_tmean_loca1[[winter_index_rcp]]
rcp85_tmean_winter <- rcp85_tmean_loca1[[winter_index_rcp]]
hist_tmean_winter <- hist_tmean_loca1[[winter_index_hist]]

rcp45_pr_winter <- rcp45_pr_loca1[[winter_index_rcp]]
rcp85_pr_winter <- rcp85_pr_loca1[[winter_index_rcp]]
hist_pr_winter <- hist_pr_loca1[[winter_index_hist]]
# ============================================================================#
# monthly summary
# ============================================================================#

# if temperature is below the threshold, keep the precipitation value (as snow),
# else set to zero (15 mins)
rcp45_s_loca1 <- terra::ifel(rcp45_tmean_winter < temp_threshold, rcp45_pr_winter, 0)
rcp85_s_loca1 <- terra::ifel(rcp85_tmean_winter < temp_threshold, rcp85_pr_winter, 0)
hist_s_loca1 <- terra::ifel(hist_tmean_winter < temp_threshold, hist_pr_winter, 0)


writeRaster(rcp45_s_loca1,
            "E:/data-raw/loca1/summary/rcp45_s_loca1.tif",
            overwrite = TRUE
)

writeRaster(rcp85_s_loca1,
            "E:/data-raw/loca1/summary/rcp85_s_loca1.tif",
            overwrite = TRUE
)

writeRaster(hist_s_loca1,
            "E:/data-raw/loca1/summary/hist_s_loca1.tif",
            overwrite = TRUE
)

rcp45_s_monthly <- tapp(rcp45_s_loca1, index = "months", fun = mean, cores = 12)
rcp85_s_monthly <- tapp(rcp85_s_loca1, index = "months", fun = mean, cores = 12)
hist_s_monthly <- tapp(hist_s_loca1, index = "months", fun = mean, cores = 12)




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