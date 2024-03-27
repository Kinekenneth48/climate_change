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

# ==============================================================================
# get USA map with state boundaries
# ==============================================================================

conus <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

conus <- terra::vect(conus)

################################################################################
## STEP 1: INITIAL SETUP
################################################################################

# ==============================================================================
# get USA map with state boundaries
# ==============================================================================

d1 = (vic_r45c_ms_rtl_30km[[1]] - vic_hist_ms_rtl_30km[[1]])/ vic_hist_ms_rtl_30km[[1]]
plot(d1, breaks = c(-1,  -0.6, -0.2, 0, 0.2, 0.5, 1))

ggplot() +
  geom_spatraster_contour_filled(
    data = d1,
    breaks = c(-1,-0.8,  -0.6, -0.4, -0.2, 0, 0.2,0.4, 0.6, 0.8, 1)
  ) +
  scale_fill_manual(
    name = "Mean Change",
    values = c(
    "#7f3b08" , "#b35806", "#e08214", "#fdb863", "#fee0b6",
      "#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b"
    ), na.translate = F,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle('ACCESS10: RCP45 MEAN CHANGE') +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    legend.position = c(0.9, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30)
  )
