################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
pacman::p_load(
  fitdistrplus, terra, extRemes, evd, ismev, trend, tidyterra,
  tidyverse
)


source("R/mk.R")
source("R/fit_gev_event.R")

# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

# ============================================================================#
# load the data
# ============================================================================#


r1 <- terra::rast("E:/data-raw/canada_model/raster/snw/r1/r1_raster.tif")
r2 <- terra::rast("E:/data-raw/canada_model/raster/snw/r2/r2_raster.tif")
r3 <- terra::rast("E:/data-raw/canada_model/raster/snw/r3/r3_raster.tif")
r4 <- terra::rast("E:/data-raw/canada_model/raster/snw/r4/r4_raster.tif")
r5 <- terra::rast("E:/data-raw/canada_model/raster/snw/r5/r5_raster.tif")

raster <- mean(r1, r2, r3, r4, r5)

# 1986 to 2016
r1_hist <- subset(r1, time(r1) >= 1981 &  time(r1) <= 2011)
r2_hist <- subset(r2,  time(r2) >= 1981 &  time(r2) <= 2011)
r3_hist <- subset(r3,time(r3) >= 1981 &  time(r3) <= 2011)
r4_hist <- subset(r4, time(r4) >= 1981 &  time(r4) <= 2011)
r5_hist <- subset(r5, time(r5) >= 1981 &  time(r5) <= 2011)
hist <- mean(r1_hist, r2_hist, r3_hist, r4_hist, r5_hist)

# 2025 to 2075
r1_future <- subset(r1, time(r1) >= 2020 & time(r1) <= 2070)
r2_future <- subset(r2, time(r2) >= 2020 & time(r2) <= 2070)
r3_future <- subset(r3, time(r3) >= 2020 & time(r3) <= 2070)
r4_future <- subset(r4, time(r4) >= 2020 & time(r4) <= 2070)
r5_future <- subset(r5, time(r5) >= 2020 & time(r5) <= 2070)
future <- mean(r1_future, r2_future, r3_future, r4_future, r5_future)


# ==============================================================================
# get USA map with state boundaries
# ==============================================================================

conus <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

conus <- terra::vect(conus)


################################################################################
## STEP 1: check for Stationarity
################################################################################

# mk test
mk_hist <- terra::app(hist, fun = mk)
mk_future <- terra::app(future, fun = mk)


cls <- data.frame(id = 0:1, cover = c("stationary", "non-stationary"))
levels(mk_hist) <- cls
levels(mk_future) <- cls


mk_canada <- c(mk_hist, mk_future)
names(mk_canada) <- c("hindcast", "future")

# ============================================================================#
# create/load a mask
# ============================================================================#

#prism_mask_vic <- terra::rast("data-raw/mask/prism_mask_vic.tif")


# ============================================================================#
# plot stat or non-stat(check for Stationarity)
# ============================================================================#
mk_vic <- terra::mask(mk_vic, prism_mask_vic)


ggplot() +
  geom_spatraster(
    data = mk_canada
  ) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_manual(
    name = "Level",
    values = c(
      "#ffff33", "#a65628"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    #legend.position = c(0.95, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 30)
  )






################################################################################
## STEP 2: Fit dist BLOCK approach
################################################################################


# fit dist and estimate MRI (mean is varying for future scenarios)
ECC_hist_block_canada <- terra::app(hist,
  fun = fit_gev_event, cores = 12
)


ECC_future_block_canada <- terra::app(future,
                   fun = fit_gev_event, cores = 12
)



writeRaster(ECC_hist_block_canada,
            "E:data-raw/canada_model/ECC_hist_block_canada.tif",
            overwrite = TRUE
)

writeRaster(ECC_future_block_canada,
  "E:data-raw/canada_model/ECC_future_block_canada.tif",
  overwrite = TRUE
)




ECC_hist_block_canada <-
  rast("E:data-raw/canada_model/ECC_hist_block_canada.tif")

ECC_future_block_canada <-
  rast("E:data-raw/canada_model/ECC_future_block_canada.tif")


mean_ecc_hist_block_canada <- mean(ECC_hist_block_canada, na.rm = TRUE)
mean_ecc_future_block_canada <- mean(ECC_future_block_canada, na.rm = TRUE)


#prism_mask_vic <- rast("data-raw/mask/prism_mask_vic.tif")

diff_event_future_block_canada <- (mean_ecc_future_block_canada - mean_ecc_hist_block_canada) /
  mean_ecc_hist_block_canada



ggplot() +
  geom_spatraster_contour_filled(
    data = diff_event_future_block_canada,
    breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
  ) +
  scale_fill_manual(
    name = "MRI Percent \n Change",
    values = c(
    #  "#543005", 
      "#8c510a", "#bf812d", "#dfc27d",
      "#f6e8c3",
      "#c7eae5"
       
      # , "#80cdc1","#35978f", "#01665e", "#003c30"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    legend.position = c(0.91, 0.28),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  )
