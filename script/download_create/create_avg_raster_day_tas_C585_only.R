################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(stars)
library(raster)
library(terra)
library(tictoc)


terraOptions(memfrac = 0.8, verbose = TRUE)


# ################################################################################
# ## STEP 1: Create single raster files for SPEAR
# ################################################################################
# # ============================================================================#
# #  monthly historical
# # ============================================================================#
# # Initialize an empty list to store cropped SpatRaster objects
# raster_list <- list()
# 
# # Loop through 30 file paths to read each SpatRaster file
# for (ensemble in 1:30) {
#   file_path <- sprintf(
#     "data-raw/spear/tas/historical/month/tas_Amon_GFDL-SPEAR-MED_historical_r%di1p1f1_gr3_192101-201412.nc",
#     ensemble
#   )
#   raster_list[[length(raster_list) + 1]] <- terra::crop(terra::rast(file_path), usa_extent)
# }
# 
# 
# # Combine all individual SpatRasters into a single SpatRasterDataset
# raster_data <- terra::sds(raster_list)
# 
# 
# # Compute the mean raster layer across all SpatRasters in the dataset
# spear_hist_r_comb_tas <- terra::app(raster_data, mean)
# 
# 
# # Save the computed mean raster layer to disk
# terra::writeRaster(spear_hist_r_comb_tas,
#                    filename = "data-raw/raster/spear_hist_r_comb_tas.tif",
#                    overwrite = TRUE
# )
# 
# remove(raster_list, raster_data, spear_hist_r_comb_tas)
# 



################################################################################
## STEP 2: Create single raster files for LOCA
#################################################################################
# ============================================================================#
#  daily historical
# ============================================================================#

# Create an empty list to store individual SpatRaster objects
raster_list <- list()

# Define types of temperature ('tasmax' and 'tasmin')
temp_types <- c("tasmax", "tasmin")

# Loop through each type of temperature and file index
for (temp in temp_types) {
  for (ensemble in 1:3) {
    # Generate the file path based on the current temperature type and index
    file_path <- sprintf(
      "D:/data-raw/loca/ACCESS-CM2/%s/historical/day/%s.ACCESS-CM2.historical.r%di1p1f1.1950-2014.LOCA_16thdeg_v20220413.nc",
      temp, temp, ensemble
    )
    
    # Read, crop and append the raster in one go
    raster_list[[length(raster_list) + 1]] <- terra::rast(file_path)
  }
}


# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(raster_list)

tictoc::tic() #37 hours
# Compute the mean raster layer across all SpatRasters in the dataset
loca_hist_day_comb_tas <- terra::app(raster_data, mean)
tictoc::toc()



# Save the computed mean raster layer to disk
tictoc::tic() # 9 hrs
terra::writeRaster(loca_hist_day_comb_tas,
                   filename = "data-raw/loca/raster/loca_hist_day_comb_tas.tif",
                   overwrite = TRUE
)
tictoc::toc()

remove(raster_list, raster_data, loca_hist_day_comb_tas)




