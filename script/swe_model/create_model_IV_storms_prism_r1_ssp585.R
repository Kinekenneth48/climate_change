################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(terra)
library(tidyverse)
library(elevatr)
library(datawizard)

terraOptions(memfrac = 0.8, verbose = TRUE)

# ============================================================================#
# load data
# ============================================================================#

temp_diff_layers <- terra::rast("E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/temp_diff_r1_ssp585.tif")
mean_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/mean_temp_r1_ssp585.tif")
total_ppt_layers <- terra::rast("E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/total_ppt_r1_ssp585.tif")
snow_window_layers <- terra::rast("E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/snow_window_r1_ssp585.tif")

storm_one_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/storm_one_temp_r1_ssp585.tif")
storm_two_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/storm_two_temp_r1_ssp585.tif")
storm_three_temp_layers <- terra::rast("E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/storm_three_temp_r1_ssp585.tif")
storm_one_layers <- terra::rast("E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/storm_one_r1_ssp585.tif")
storm_two_layers <- terra::rast("E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/storm_two_r1_ssp585s.tif")
storm_three_layers <- terra::rast("E:/data-raw/swe_model_vars/ssp585/r1/r1_model_vars/storm_three_r1_ssp585.tif")

ecoregions_L1_loca <- rep(terra::rast("data-raw/eco_regions/ecoregions_L1.tif"), nlyr(temp_diff_layers))
ecoregions_L2_loca <- rep(terra::rast("data-raw/eco_regions/ecoregions_L2.tif"), nlyr(temp_diff_layers))
ecoregions_L3_loca <- rep(terra::rast("data-raw/eco_regions/ecoregions_L3.tif"),nlyr(temp_diff_layers))

elev_raster_layer_ssp585 <- rep(terra::rast("E:/data-raw/swe_model_vars/elev_raster_layer_ssp585.tif"), nlyr(temp_diff_layers))

lon <- terra::rast("E:/data-raw/swe_model_vars/lon_ssp585.tif")
lat <- terra::rast("E:/data-raw/swe_model_vars/lat_ssp585.tif")

# ============================================================================#
# get lon and lat
# ============================================================================#
# raster <- terra::shift(temp_diff_layers[[1]], dx = -360)  #shift to match that of prism
# lon_ssp585 <- terra::init(raster, 'x')
# lat_ssp585 <- terra::init(raster, 'y')
# 
# terra::writeRaster(lon_ssp585,"E:/data-raw/swe_model_vars/lon_ssp585.tif",overwrite = TRUE)
# terra::writeRaster(lat_ssp585,"E:/data-raw/swe_model_vars/lat_ssp585.tif",overwrite = TRUE)


# # ============================================================================#
# # get elevation raster
# # ============================================================================#
# # download elevation raster
# raster <- terra::shift(temp_diff_layers[[1]], dx = -360)
# elev_raster <- elevatr::get_elev_raster(locations = raster, z = 5)
# 
# # match elevation raster with temp_diff_layers resolution
# elev_raster <- terra::rast(elev_raster)
# elev_raster <- terra::project(x = elev_raster, y = terra::crs(temp_diff_layers))
# 
# 
# elev_raster_layer_ssp585 <- terra::project(elev_raster, temp_diff_layers[[1]],
#   method = "bilinear"
# )
# 
# 
# # save data
# terra::writeRaster(elev_raster_layer_ssp585,
#   "E:/data-raw/swe_model_vars/elev_raster_layer_ssp585.tif",
#   overwrite = TRUE
# )



################################################################################
## STEP 1: CREATE THE TEST DATA FOR PREDICTION
################################################################################

# ============================================================================#
# shift long to match that used to train data
# ============================================================================#
temp_diff_layers <- terra::shift(temp_diff_layers, dx = -360)
mean_temp_layers <- terra::shift(mean_temp_layers, dx = -360)
total_ppt_layers <- terra::shift(total_ppt_layers, dx = -360)
snow_window_layers <- terra::shift(snow_window_layers, dx = -360)
storm_one_temp_layers <- terra::shift(storm_one_temp_layers, dx = -360)
storm_two_temp_layers <- terra::shift(storm_two_temp_layers, dx = -360)
storm_three_temp_layers <- terra::shift(storm_three_temp_layers, dx = -360)
storm_one_layers <- terra::shift(storm_one_layers, dx = -360)
storm_two_layers <- terra::shift(storm_two_layers, dx = -360)
storm_three_layers <- terra::shift(storm_three_layers, dx = -360)
ecoregions_L1_loca <- terra::shift(ecoregions_L1_loca, dx = -360)
ecoregions_L2_loca <- terra::shift(ecoregions_L2_loca, dx = -360)
ecoregions_L3_loca <- terra::shift(ecoregions_L3_loca, dx = -360)
elev_raster_layer_ssp585 <- terra::shift(elev_raster_layer_ssp585, dx = -360)

# ============================================================================#
# create test data
# ============================================================================#

# Create a sequence of years from 2016 to 2100
years <- time(temp_diff_layers)

for (i in 1:nlyr(temp_diff_layers)) {
  test_data_per_year <- c(
    lon, lat,
    total_ppt_layers[[i]], temp_diff_layers[[i]],
    mean_temp_layers[[i]], snow_window_layers[[i]],
    storm_one_temp_layers[[i]], storm_two_temp_layers[[i]],
    storm_three_temp_layers[[i]], storm_one_layers[[i]],
    storm_two_layers[[i]], storm_three_layers[[i]],
    ecoregions_L1_loca[[i]], ecoregions_L2_loca[[i]],
    ecoregions_L3_loca[[i]], elev_raster_layer_ssp585[[i]]
  )
  
  names(test_data_per_year) <- c(
    "lon", "lat", "total_ppt", "temp_diff", "mean_temp", "snow_window",
    "storm_one_temp", "storm_two_temp",
    "storm_three_temp", "storm_one",
    "storm_two", "storm_three", "L1", "L2", "L3", "elev"
  )
  
  
  # Extract year from 2016 to 2100 based on loop iteration
  year <- years[i]
  terra::time(test_data_per_year) = rep(year, nlyr(test_data_per_year))
  
  # Construct file name for the output raster file
  file_name <- paste0("E:/data-raw/swe_model_vars/ssp585/r1/test_data/test_data_storm_prism_", year, ".tif")
  
  # Write the raster file
  writeRaster(test_data_per_year, file_name, overwrite = TRUE)
}



################################################################################
## STEP 3: PREDICT SWE WITH RF MODEL
################################################################################

# ============================================================================#
# create a mask
# ============================================================================#
# prism <- terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]
# prism_mask_loca <- project(prism, total_ppt_layers[[1]])
# 
# 
# writeRaster(prism_mask_loca,
#             "data-raw/mask/prism_mask_loca.tif",
#             overwrite = TRUE
# )

prism_mask_loca = rast("data-raw/mask/prism_mask_loca.tif")

# ============================================================================#
# RF Model
# ============================================================================#


load("E:/data-raw/swe_model_vars/storm_prism/model/rf_storms_prism.RData")

# Get names of TIFF files in the folder
tif_files <- list.files("E:/data-raw/swe_model_vars/ssp585/r1/test_data/",
                        pattern = "\\.tif$", full.names = TRUE)

years = 2016:2100

# predict to a raster
for (i in 1:length(tif_files)) {
  test = rast(tif_files[i])
  

  rf_pred_raster <- predict(test, rf_storms_prism, na.rm = TRUE)
  
  # Extract year from 2016 to 2100 based on loop iteration
  year <- years[i]
  time(rf_pred_raster) = year
  rf_pred_raster <- terra::mask(rf_pred_raster, prism_mask_loca)
  
  # Construct file name for the output raster file
  file_name <- paste0("E:/data-raw/swe_model_vars/ssp585/r1/prediction/rf/pred_r1_ssp585_",
                      year, ".tif")
  
  # Write the raster file
  writeRaster(rf_pred_raster, file_name, overwrite = TRUE)
  gc()

}


