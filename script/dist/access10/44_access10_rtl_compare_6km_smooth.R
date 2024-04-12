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

vic_hist_ms_rtl_6km_smooth <- rast("E:/data-raw/rtl/ACCESS1-0/6km_smooth_shape/vic_hist_ms_rtl_6km_smooth.tif")
vic_r45c_ms_rtl_6km_smooth <- rast("E:/data-raw/rtl/ACCESS1-0/6km_smooth_shape/vic_r45c_ms_rtl_6km_smooth.tif")
vic_r85c_ms_rtl_6km_smooth =  rast("E:/data-raw/rtl/ACCESS1-0/6km_smooth_shape/vic_r85c_ms_rtl_6km_smooth.tif")

# load mask raster
prism_mask_vic <- rast("data-raw/mask/prism_mask_vic.tif")


# ============================================================================#
# Ecoregion for RTL
# ============================================================================#
#target raster
target_raster = vic_hist_ms_rtl_6km_smooth[[1]]

# Load your shapefile and raster
ecoregions <- terra::vect("data-raw/eco_regions/NA_CEC_Eco_Level3.shp")

# Align CRS of high_res_raster/ target raster with ecoregions if needed
high_res_raster <- terra::project(target_raster, crs(ecoregions))

# Rasterize the ecoregions, using the 'NA_L3CODE' attribute for the raster values
# Transfer values associated with the geometries of vector data to a raster
rasterized_ecoregions_L3 <- terra::rasterize(ecoregions, high_res_raster, field = "NA_L3CODE")

# Reproject the ecoregion raster to match the CRS of the target raster
reprojected_ecoregions_L3 <- terra::project(rasterized_ecoregions_L3, crs(target_raster))

# Resample the reprojected ecoregion raster to match the resolution of the target raster
ecoregions_L3_rtl <- terra::resample(reprojected_ecoregions_L3,
                                 target_raster,
                                 method = "near"
)

#crop for CONUS
ecoregions_L3_rtl <- terra::mask(ecoregions_L3_rtl, prism_mask_vic)

# exclude this ecoregion from raster
ecoregions_L3_rtl[ecoregions_L3_rtl == '9.6.1'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '9.5.1'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '9.4.7'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '9.4.6'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '8.5.3'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '8.3.8'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '8.3.7'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '8.3.5'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '7.1.8'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '15.4.1'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '11.1.3'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '11.1.2'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '11.1.1'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '10.2.2'] <- NA
ecoregions_L3_rtl[ecoregions_L3_rtl == '10.2.1'] <- NA

terra::writeRaster(ecoregions_L3_rtl,
                   filename = "data-raw/eco_regions/ecoregions_L3_rtl.tif",
                   overwrite = TRUE
)

# ==============================================================================
# get USA map with state boundaries
# ==============================================================================

conus <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

conus <- terra::vect(conus)


################################################################################
## STEP 1: RL
################################################################################

# ==============================================================================
# RCP45
# ==============================================================================

r45_rtl_diff <- (vic_r45c_ms_rtl_6km_smooth - vic_hist_ms_rtl_6km_smooth) / vic_hist_ms_rtl_6km_smooth

r45_rtl_diff_mask <- terra::mask(r45_rtl_diff, ecoregions_L3_rtl)

ggplot() +
  geom_spatraster_contour_filled(
    data = r45_rtl_diff_mask,
    breaks = c( -1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
  ) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_manual(
    name = "RTL Percent Change",
    values = c(
     "#7f3b08", "#b35806", "#e08214", "#fdb863", "#fee0b6",
      "#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b"
    ), 
    na.value = NA,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_spatvector(data = conus, fill = NA, color = "grey40") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("ACCESS10: RCP45 RTL Percent Change 6km") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    #legend.position = c(0.9, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 25),
    strip.text = element_text(size = 30, face = "bold") 
  )




# ==============================================================================
# RCP85
# ==============================================================================

r85_rtl_diff <- (vic_r85c_ms_rtl_6km_smooth - vic_hist_ms_rtl_6km_smooth) / vic_hist_ms_rtl_6km_smooth

r85_rtl_diff_mask <- terra::mask(r85_rtl_diff, prism_mask_vic)

ggplot() +
  geom_spatraster_contour_filled(
    data = r85_rtl_diff_mask,
    breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
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
  ggtitle("ACCESS10: RCP85 RTL Percent Change 6km ") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_sf(crs = 4326) +
  theme(
    #legend.position = c(0.2, 0.23),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 25),
    strip.text = element_text(size = 30, face = "bold") 
  )
