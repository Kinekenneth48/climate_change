###############################################################################
### STEP 0 : INITIAL SET UP
#################################################################################

# ============================================================================#
# load the required R packages
# ============================================================================#
library(terra)

terraOptions(memfrac = 0.80, verbose = TRUE)

loca_tas <- terra::rast("D:/data-raw/loca/ACCESS-CM2/tasmax/historical/day/tasmax.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA_16thdeg_v20220413.nc")

# ============================================================================#
# get file path for netCDF files:UA
# ============================================================================#

# Define the path
path <- "D:/data-raw/ua/day"

# Define the pattern to match filenames from 1982 to 2014
pattern <- "4km_SWE_Depth_WY(19[8-9][0-9]|200[0-9]|201[0-4])_v01\\.nc"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = pattern,
  full.names = TRUE
)


# Initialize a list to hold the daily raster layers
all_daily_layers <- list()


# Initialize a vector to store the date information for each layer
all_dates <- c()



# Check CRS
crs_loca_tas <- crs(loca_tas)






# ============================================================================#
# create the UA daily SWE maps into a single multilayer file
# ============================================================================#
tictoc::tic() # 2 hours
# Loop through each file to extract daily raster layers
for (j in 1:length(file_list)) {
  # Load nc file using terra
  ua_raster <- terra::rast(x = file_list[j])

  # Check if it's a leap year or not
  if (terra::nlyr(ua_raster) == 732) {
    ua_raster <- ua_raster[[1:366]] # Get only SWE for leap year
  } else {
    ua_raster <- ua_raster[[1:365]] # Get only SWE for non-leap year
  }

  crs_ua_swe <- crs(ua_raster)


  # If CRSs are different, reproject ua_swe to match the CRS of loca_tas
  if (crs_loca_tas != crs_ua_swe) {
    ua_raster <- terra::project(x = ua_raster, y = crs_loca_tas, threads = TRUE)
  }

  # Shift raster data
  ua_raster <- terra::shift(ua_raster, dx = 360)

  # Resample 'ua_raster' to have the same resolution as 'loca_tas'
  ua_raster_resampled <- terra::project(ua_raster, loca_tas, method = "average")

  # Retrieve the date/time information from the raster
  raster_dates <- terra::time(ua_raster_resampled)

  # Determine the number of layers and append each layer to the list
  nlayers <- nlyr(ua_raster_resampled)
  for (i in 1:nlayers) {
    all_daily_layers[[length(all_daily_layers) + 1]] <- ua_raster_resampled[[i]]

    # Retrieve the date for the current layer
    layer_date <- raster_dates[i]
    all_dates <- c(all_dates, layer_date)
  }
}
tictoc::toc()


# Stack all daily layers into a single multi-layer raster
tictoc::tic() #20 mins
ua_swe_combined_daily_1982_2014_for_loca_tas_res <- terra::rast(all_daily_layers)
tictoc::toc()


#Set the file name for the combined raster
file_name <- "ua_swe_combined_daily_1982_2014_for_loca_tas_res.tif"

# Set the output directory
output_dir <- "data-raw/ua/raster/combined"

# Save the combined raster as a GeoTIFF file
tictoc::tic() # 12 hours
terra::writeRaster(ua_swe_combined_daily_1982_2014_for_loca_tas_res,
                   filename = file.path(output_dir, file_name),
                   overwrite = TRUE
)
tictoc::toc()