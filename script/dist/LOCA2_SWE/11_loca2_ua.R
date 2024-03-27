################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
pacman::p_load(fitdistrplus,terra,extRemes,evd,
               ismev,trend)


source("R/fit_log_normal_less.R")
source("R/fit_log_normal.R")
source("R/fit_gev.R")
source("R/gev_fit_time_varying.R")
source("R/gev_fit_stat_nonstat.R")
source("R/fit_gev_event.R")
source("R/mk.R")
source("R/fit_gev_shape.R")
source("R/rl_gev_time_data_w_shape.R")


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

set.seed(275349)

# ============================================================================#
# load the data
# ============================================================================#
ua_swe <- terra::rast("E:/data-raw/ua/raster/annual/max_swe_ua.tif")




# ============================================================================#
# create a single multilayer raster for ssp585 hist prediction
# ============================================================================#
# Get names of TIFF files in the folder
tif_files <- list.files("E:/data-raw/swe_model_vars/ssp585/hist/prediction/rf/",
                        pattern = "\\.tif$", full.names = TRUE
)

ssp585_hist_swe <- terra::rast(tif_files)

# ============================================================================#
# match the extent ssp585_r1_swe and ua_swe
# ============================================================================#

ua_swe <- terra::project(ua_swe, ssp585_hist_swe, method = "bilinear")



################################################################################
## STEP 1: COMPARE OVERLAP B/N UA AND R1
################################################################################

# ============================================================================#
# get data from 1982 to 2014
# ============================================================================#
ua_test <- subset(ua_swe, time(ua_swe) >= 1982 & time(ua_swe) <= 2014)
ssp585_hist_test <- subset(ssp585_hist_swe, time(ssp585_hist_swe) >= 1982 &
                    time(ssp585_hist_swe) <= 2014)

diff_hist <- ssp585_hist_test - ua_test

plot(diff_hist[[3]], breaks = c(-500, -250, -100, -50, -25, -10, 0, 10, 25, 50, 100, 250, 500))

# ============================================================================#
# MAE and RMSE
# ============================================================================#
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

error_2011_2014 = diff_hist[[30:33]]
names(error_2011_2014) <- c("2011", "2012", "2013", "2014")

ggplot() +
  geom_spatraster_contour_filled(
    data = error_2011_2014,
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
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 25),
    strip.text = element_text(size = 30, face = "bold") # Increase size of facet titles and make them bold
  )



par(mfcol = c(2, 2), mar = c(5, 4, 4, 5) + 0.1)
plot(diff_hist[[30]],
     breaks = c(-2000,  -100, -50, -25, -10, 0, 10, 25, 50, 100,  2000),
     main = "2011"
)

plot(diff_hist[[31]],
     breaks = c(-2000,  -100, -50, -25, -10, 0, 10, 25, 50, 100,  2000),
     main = "2012",
     legend = FALSE
)

plot(diff_hist[[32]],
     breaks = c(-2000,  -100, -50, -25, -10, 0, 10, 25, 50, 100,  2000),
     main = "2013",

     legend = FALSE
)

plot(diff_hist[[33]],
     breaks = c(-2000,  -100, -50, -25, -10, 0, 10, 25, 50, 100,  2000),
     main = "2014",

     legend = FALSE
)
par(mfrow = c(1, 1))













################################################################################
## STEP 2: FIT DISTR.
################################################################################

# estimate shape parameter with Lmoments
ssp585_r1_shape <- terra::app(ssp585_r1_swe, fun = fit_gev_shape, cores = 12)
ua_shape <- terra::app(ua_swe, fun = fit_gev_shape, cores = 12)
full_shape <- terra::app(full_r1, fun = fit_gev_shape, cores = 12)

# add the shape parameter to the end of the data
ssp585_r1_swe_w_shape <- c(ssp585_r1_swe, ssp585_r1_shape)
ua_swe_w_shape <- c(ua_swe, ua_shape)
full_r1_w_shape <- c(full_r1, full_shape)


ECC_ssp585_r1 <- terra::app(ssp585_r1_swe_w_shape,
                            fun = rl_gev_time_data_w_shape, cores = 12
)

ECC_ua_swe <- terra::app(ua_swe_w_shape,
                         fun = fit_gev_event, cores = 12
)

ECC_full_r1 <- terra::app(full_r1_w_shape,
                          fun = rl_gev_time_data_w_shape, cores = 12
)

writeRaster(ECC_ssp585_r1,
            "E:data-raw/dist_fit_vic/ECC_ssp585_r1.tif",
            overwrite = TRUE
)

writeRaster(ECC_ua_swe,
            "E:data-raw/dist_fit_vic/ECC_ua_swe.tif",
            overwrite = TRUE
)

writeRaster(ECC_full_r1,
            "E:data-raw/dist_fit_vic/ECC_full_r1.tif",
            overwrite = TRUE
)



ECC_ssp585_r1 <- rast("E:data-raw/dist_fit_vic/ECC_ssp585_r1.tif")
ECC_ua_swe <- rast("E:data-raw/dist_fit_vic/ECC_ua_swe.tif")
ECC_full_r1 <- rast("E:data-raw/dist_fit_vic/ECC_full_r1.tif")

mean_ecc_ssp585_r1 <- mean(ECC_ssp585_r1, na.rm = TRUE)
mean_ecc_full_r1 <- mean(ECC_full_r1, na.rm = TRUE)



diff_event_ssp585_r1 <- (mean_ecc_ssp585_r1 - ECC_ua_swe) / ECC_ua_swe
diff_event_ssp585_full <- (mean_ecc_full_r1 - ECC_ua_swe) / ECC_ua_swe



par(mfcol = c(1, 2), mar = c(5, 4, 4, 5) + 0.1)
plot(diff_event_ssp585_r1,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "diff in 50 year event r1 vs UA ",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     )
)

plot(diff_event_ssp585_full,
     breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
     main = "diff in 50 year event r1 vs UA (FULL)",
     col = c(
       "#543005", "#8c510a", "#bf812d", "#dfc27d",
       "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
     ),
     legend = FALSE
)
par(mfrow = c(1, 1))
