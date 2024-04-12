################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(terra)
library(tidyterra)
library(tidyverse)

# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.95, verbose = TRUE)

# set seed
set.seed(121)

# ============================================================================#
# load the data
# ============================================================================#

vic_hist_ms_rtl_30km =  rast("E:/data-raw/rtl/ACCESS1-0/vic_hist_ms_rtl_30km.tif")
vic_r45c_ms_rtl_30km =  rast("E:/data-raw/rtl/ACCESS1-0/vic_r45c_ms_rtl_30km.tif")
vic_r85c_ms_rtl_30km =  rast("E:/data-raw/rtl/ACCESS1-0/vic_r85c_ms_rtl_30km.tif")

# load mask raster
prism_mask_vic <- rast("data-raw/mask/prism_mask_vic.tif")

# ==============================================================================
# get USA map with state boundaries
# ==============================================================================

conus <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

conus <- terra::vect(conus)


# ============================================================================#
# create a mask
# ============================================================================#
prism <- terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]
prism_mask_rtl_30km <- project(prism, vic_hist_ms_rtl_30km[[1]])


writeRaster(prism_mask_rtl_30km,
            "data-raw/mask/prism_mask_rtl_30km.tif",
            overwrite = TRUE
)

prism_mask_rtl_30km = rast("data-raw/mask/prism_mask_rtl_30km.tif")


################################################################################
## STEP 1: RL
################################################################################

# ==============================================================================
# RCP45
# ==============================================================================

r45_rtl_diff_30km <- (vic_r45c_ms_rtl_30km - vic_hist_ms_rtl_30km) / vic_hist_ms_rtl_30km

r45_rtl_diff_mask_30km <- terra::mask(r45_rtl_diff_30km, prism_mask_rtl_30km)

ggplot() +
  geom_spatraster_contour_filled(
    data = r45_rtl_diff_mask_30km,
    breaks = c(-1, -0.8, -0.60, -0.4, -0.2, 0, 0.2, 0.4, 0.60, 0.8, 1)
  ) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_manual(
    name = "RTL Percent Change",
    values = c(
      "#7f3b08", "#b35806", "#e08214", "#fdb863", "#fee0b6",
      "#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("ACCESS10: RCP45 RTL Percent Change 30km ") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    #legend.position = c(0.9, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30),
    strip.text = element_text(size = 30, face = "bold") 
  )




# ==============================================================================
# RCP85
# ==============================================================================

r85_rtl_diff_30km <- (vic_r85c_ms_rtl_30km - vic_hist_ms_rtl_30km) / vic_hist_ms_rtl_30km

r85_rtl_diff_mask_30km <- terra::mask(r85_rtl_diff_30km, prism_mask_rtl_30km)

ggplot() +
  geom_spatraster_contour_filled(
    data = r85_rtl_diff_mask_30km,
    breaks = c(-1, -0.8, -0.60, -0.4, -0.2, 0, 0.2, 0.4, 0.60, 0.8, 1)
  ) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_manual(
    name = "RTL Percent Change",
    values = c(
      "#7f3b08", "#b35806", "#e08214", "#fdb863", "#fee0b6",
      "#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("ACCESS10: RCP85 RTL Percent Change 30km ") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    #legend.position = c(0.9, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30),
    strip.text = element_text(size = 30, face = "bold") 
  )
