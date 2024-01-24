################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(raster)
library(terra)
library(tictoc)

# define the extent of usa
usa_extent <- terra::ext(235, 300, 24, 50)



################################################################################
## STEP 1: Create single raster files for CMIP6 
################################################################################
# ============================================================================#
#  day future
# ============================================================================#
raster_list <- list()
date_ranges <- c("20150101-20341231", "20350101-20541231","20550101-20741231",
                 "20750101-20941231","20950101-21001231") 

for (date_range in date_ranges) {
  file_path <- sprintf(
    "D:/data-raw/cmip6/models/GFDL-ESM4/snw/future/day/snw_day_GFDL-ESM4_ssp585_r1i1p1f1_gr1_%s.nc",
    date_range
  )
  raster_list[[length(raster_list) + 1]] <- terra::crop(terra::rast(file_path), usa_extent)
}


# Combine all individual SpatRasters into a single SpatRaster
GFDL_ESM4_day_snw_future_585 <- terra::rast(raster_list)




# Save the computed mean raster layer to disk
terra::writeRaster(GFDL_ESM4_day_snw_future_585,
                   filename = "E:/data-raw/cmip6/GFDL_ESM4_day_snw_future_585.tif",
                   overwrite = TRUE
)




# ============================================================================#
#  day hist
# ============================================================================#

raster_list <- list()  # List to store mean rasters for each date range

date_ranges <- c("18500101-18691231", "18700101-18891231", "18900101-19091231",
                 "19100101-19291231", "19300101-19491231", "19500101-19691231",
                 "19700101-19891231", "19900101-20091231", "20100101-20141231")

ensemble_numbers <- 1:3  # Adjust this based on your ensemble numbers

for (date_range in date_ranges) {
  ensemble_rasters <- list()  # Temporary list for a single date range
  
  for (ensemble in ensemble_numbers) {
    file_path <- sprintf(
      "D:/data-raw/cmip6/models/GFDL-ESM4/snw/historical/day/snw_day_GFDL-ESM4_historical_r%si1p1f1_gr1_%s.nc",
      ensemble, date_range
    )
    raster <- terra::crop(terra::rast(file_path), usa_extent)
    ensemble_rasters[[length(ensemble_rasters) + 1]] <- raster
  }
  
  # Calculate the mean raster for this date range
  mean_raster <- terra::app(terra::sds(ensemble_rasters), mean)
  raster_list[[length(raster_list) + 1]] <- mean_raster
}


# Combine all individual SpatRasters into a single SpatRaster
GFDL_ESM4_day_snw_hist <- terra::rast(raster_list)


# Save the computed mean raster layer to disk
terra::writeRaster(GFDL_ESM4_day_snw_hist,
                   filename = "E:/data-raw/cmip6/GFDL_ESM4_day_snw_hist.tif",
                   overwrite = TRUE
)



