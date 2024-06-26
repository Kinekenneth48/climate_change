################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(fitdistrplus)
library(terra)

source("R/fit_log_normal_less.R")
source("R/fit_log_normal.R")
source("R/fit_gev.R")
source("R/fit_gev_less.R")


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

# ============================================================================#
# load the data
# ============================================================================#
vic_hist <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")
ua_hist <- terra::rast("E:data-raw/ua/raster/combined/ua_swe_combined_daily_1982_2014_for_loca_res.tif")


years <- as.numeric(format(time(ua_hist), "%Y"))
months <- as.numeric(format(time(ua_hist), "%m"))
unique_years <- unique(years)

max_swe_list <- vector("list", length = length(unique_years) - 1)

for (i in 1:(length(unique_years) - 1)) {
  start_year <- unique_years[i]
  end_year <- unique_years[i + 1]

  # Create a mask for the water year (October of start_year to September of end_year)
  water_year_mask <- (years == start_year & months >= 10) |
    (years == end_year & months <= 9)

  # subset data
  swe_subset <- ua_hist[[which(water_year_mask)]]

  # take max of snow year
  swe_subset <- max(swe_subset, na.rm = FALSE)

  # add time
  time(swe_subset) <- end_year

  max_swe_list[[i]] <- swe_subset
}


max_swe_ua <- terra::rast(max_swe_list)

# Save the resulting multi-layer raster layers
writeRaster(max_swe_ua,
  "E:data-raw/ua/raster/combined/max_swe_ua.tif",
  overwrite = TRUE
)

# load annual UA SWE
#max_swe_ua <- terra::rast("E:data-raw/ua/raster/combined/max_swe_ua.tif")

max_swe_ua <- terra::rast("E:/data-raw/ua/raster/annual/max_swe_ua.tif")
# ============================================================================#
# subset data with common time
# ============================================================================#
time_ua <- time(max_swe_ua)
time_vic <- time(vic_hist)

common_time <- intersect(time_ua, time_vic)

# Subset the raster
ua <- subset(max_swe_ua, time_ua %in% common_time)
vic <- subset(vic_hist, time_vic %in% common_time)

# ============================================================================#
# match the extent ssp585_r1_swe and ua_swe
# ============================================================================#
ua <- terra::project(ua, vic, method = "bilinear")


# ============================================================================#
# MAE and RMSE
# ============================================================================#
diff_hist <- vic - ua

plot(diff_hist[[3]], breaks = c(-500, -250, -100, -50, -25, -10, 0, 10, 25, 50, 100, 250, 500))

rmse <- vector("numeric", length = 0)
mae <- vector("numeric", length = 0)

for (i in 1:nlyr(diff_hist)) {
  rmse[i] <- sqrt(mean((values(diff_hist[[i]]))^2, na.rm = TRUE))
  mae[i] <- mean(abs(values(diff_hist[[i]])), na.rm = TRUE)
}

mae <- mean(mae)
rmse <- mean(rmse)
mae
rmse


error_2002_2005 = diff_hist[[21:24]]
names(error_2002_2005) <- c("2002", "2003", "2004", "2005")

ggplot() +
  geom_spatraster_contour_filled(
    data = error_2002_2005,
    breaks = c(-2000,  -100, -50, -25, -10, 0, 10, 25, 50, 100,  2000)
  ) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_manual(
    name = "Diff. in SWE ",
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
    # legend.position = c(0.9, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 25),
    strip.text = element_text(size = 30, face = "bold") # Increase size of facet titles and make them bold
  )



par(mfcol = c(2, 2), mar = c(5, 4, 4, 5) + 0.1)
plot(diff_hist[[21]],
     breaks = c(-2000,  -100, -50, -25, -10, 0, 10, 25, 50, 100,  2000),
     main = "2002"
)

plot(diff_hist[[22]],
     breaks = c(-2000,  -100, -50, -25, -10, 0, 10, 25, 50, 100,  2000),
     main = "2003",
     legend = FALSE
)

plot(diff_hist[[23]],
     breaks = c(-2000,  -100, -50, -25, -10, 0, 10, 25, 50, 100,  2000),
     main = "2004",
     
     legend = FALSE
)

plot(diff_hist[[24]],
     breaks = c(-2000,  -100, -50, -25, -10, 0, 10, 25, 50, 100,  2000),
     main = "2005",
     
     legend = FALSE
)
par(mfrow = c(1, 1))

















################################################################################
## STEP 1: FIT DISTRIBUTIONS (n=24)
################################################################################

# # ============================================================================#
# # fit distr.
# # ============================================================================#
# tictoc::tic()
# vic_hist_lnorm <- terra::app(vic, fit_log_normal_less, cores = 10)
# vic_hist_lnorm <- exp(vic_hist_lnorm)
# names(vic_hist_lnorm) <- c("mean", "sd")
# 
# ua_hist_lnorm <- terra::app(ua, fit_log_normal_less, cores = 10)
# ua_hist_lnorm <- exp(ua_hist_lnorm)
# names(ua_hist_lnorm) <- c("mean", "sd")
# tictoc::toc()
# 
# 
# tictoc::tic()
# vic_hist_gev <- terra::app(vic, fit_gev_less, cores = 10)
# names(vic_hist_gev) <- c("location", "scale", "shape")
# 
# ua_hist_gev <- terra::app(ua, fit_gev_less, cores = 10)
# names(ua_hist_gev) <- c("location", "scale", "shape")
# tictoc::toc()
# 
# # ============================================================================#
# # create a mask
# # ============================================================================#
# prism <- terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]
# prism_mask <- project(prism, vic_hist_lnorm[[1]])
# 
# vic_hist_lnorm <- terra::mask(vic_hist_lnorm, prism_mask)
# vic_hist_gev <- terra::mask(vic_hist_gev, prism_mask)
# 
# 
# 
# # ============================================================================#
# # plot dist fit parameters lnorm
# # ============================================================================#
# # mean parameters
# # Set up a 1x2 layout for side-by-side plots
# par(mfcol = c(2, 2))
# 
# # Plot ua_hist_lnorm[[1]]
# plot(ua_hist_lnorm[[1]],
#   main = "Mean: lnorm of UA: n=24",
#   breaks = c(0, 50, 100, 150, 200, 500, 1000, 1500, 2000, 2500)
# )
# 
# # Plot vic_hist_lnorm[[1]]
# plot(vic_hist_lnorm[[1]],
#   main = "Mean: lnorm of VIC: n=24",
#   breaks = c(0, 50, 100, 150, 200, 500, 1000, 1500, 2000, 2500),
#   legend = FALSE
# )
# 
# 
# # Reset the layout to the default (1 plot per page)
# # par(mfrow = c(1, 1))
# 
# 
# 
# # sd parameters
# # Set up a 1x2 layout for side-by-side plots
# # par(mfrow = c(2, 1))
# plot(ua_hist_lnorm[[2]],
#   main = "SD: lnorm of UA: n=24",
#   breaks = c(0, 5, 10, 15, 20, 30, 50, 100, 500, 2100000)
# )
# 
# plot(vic_hist_lnorm[[2]],
#   main = "SD: lnorm of VIC: n=24",
#   breaks = c(0, 5, 10, 15, 20, 30, 50, 100, 500, 2100000),
#   legend = FALSE
# )
# 
# 
# # Reset the layout to the default (1 plot per page)
# par(mfrow = c(1, 1))
# 
# 
# 
# 
# 
# # ============================================================================#
# # plot dist fit parameters GEV
# # ============================================================================#
# # mean parameters
# # Set up a 1x2 layout for side-by-side plots
# par(mfcol = c(2, 2))
# 
# # Plot ua_hist_lnorm[[1]]
# plot(ua_hist_gev[[1]],
#   main = "location: GEV of UA: n=24",
#   breaks = c(0, 50, 100, 150, 200, 500, 1000, 1500, 2000, 2500)
# )
# 
# # Plot vic_hist_lnorm[[1]]
# plot(vic_hist_gev[[1]],
#   main = "location: GEV of VIC: n=24",
#   breaks = c(0, 50, 100, 150, 200, 500, 1000, 1500, 2000, 2500),
#   legend = FALSE
# )
# 
# 
# # Reset the layout to the default (1 plot per page)
# # par(mfrow = c(1, 1))
# 
# 
# 
# # sd parameters
# # Set up a 1x2 layout for side-by-side plots
# # par(mfrow = c(2, 1))
# plot(ua_hist_gev[[2]],
#   main = "SD: GEV of UA: n=24",
#   breaks = c(0, 5, 10, 15, 20, 50, 100, 1000, 20000),
# )
# 
# plot(vic_hist_gev[[2]],
#   main = "SD: GEV of VIC: n=24",
#   breaks = c(0, 5, 10, 15, 20, 50, 100, 1000, 20000),
#   legend = FALSE
# )
# 
# 
# # Reset the layout to the default (1 plot per page)
# par(mfrow = c(1, 1))
# 
# 
# 
# # shape parameters
# # Set up a 1x2 layout for side-by-side plots
# par(mfrow = c(2, 1))
# plot(ua_hist_gev[[3]],
#   main = "SHAPE: GEV of UA: n=24",
#   breaks = c(-3, -0.5, -0.25, 0, 0.25, 0.5, 3),
# )
# 
# plot(vic_hist_gev[[3]],
#   main = "SHAPE: GEV of VIC: n=24",
#   breaks = c(-3, -0.5, -0.25, 0, 0.25, 0.5, 3),
#   legend = FALSE
# )
# 
# 
# # Reset the layout to the default (1 plot per page)
# par(mfrow = c(1, 1))
