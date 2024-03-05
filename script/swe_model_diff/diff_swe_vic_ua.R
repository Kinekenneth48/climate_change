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
    axis.text = element_text(size = 25)
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
