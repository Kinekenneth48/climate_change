###############################################################################
### STEP 0 : INITIAL SET UP
#################################################################################

# ============================================================================#
# load the required R packages
# ============================================================================#
library(terra)

# ============================================================================#
# get file path for netCDF files:UA
# ============================================================================#

path <- "D:/data-raw/ua/day"

# find all netCDF files that contain "4km_SWE_Depth" in their filename
file_list <- list.files(
  path = path, pattern = "4km_SWE_Depth.*\\.nc",
  full.names = TRUE
)


# Initialize a list to hold the MAX raster layers
max_swe_raster <- list()

# ============================================================================#
# create the UA max annual SWE maps
# ============================================================================#
tictoc::tic()
for (j in 1:length(file_list)) {
  # Load nc file using terra
  ua_raster <- terra::rast(x = file_list[j])
  
  # Check if it's a leap year or not
  if (terra::nlyr(ua_raster) == 732) {
    ua_raster <- ua_raster[[1:366]]  # Get only SWE for leap year
  } else {
    ua_raster <- ua_raster[[1:365]]  # Get only SWE for non-leap year
  }
  
  max_swe_raster[[j]] = max(ua_raster, na.rm = TRUE)
  }
tictoc::toc()


# Generate a sequence of years from 1982 to 2021
years <- seq(1982, 2021, by = 1)

#combine max SWE maps into a single multilayer file
max_swe_ua <- terra::rast(max_swe_raster)
time(max_swe_ua) <- years

#save data
terra::writeRaster(max_swe_ua,
                   filename = "E:/data-raw/ua/raster/annual/max_swe_ua.tif",
                   overwrite = TRUE
)

