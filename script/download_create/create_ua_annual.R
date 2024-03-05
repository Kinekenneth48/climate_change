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
  
  # Use grep to find indices of layers that contain "SWE"
  swe_indices <- grep("SWE", names(ua_raster))
  
  # Subset ua_raster to keep only SWE layers
  swe_raster <- ua_raster[[swe_indices]]
  
# get annual max and assign snow year
  annual_swe = max(swe_raster, na.rm = TRUE)
  snow_years = unique(as.numeric(format(time(swe_raster), "%Y")))
  time(annual_swe) = snow_years[2]
  
  max_swe_raster[[j]] = annual_swe
  }
tictoc::toc()


# Generate a sequence of years from 1982 to 2021


#combine max SWE maps into a single multilayer file
max_swe_ua <- terra::rast(max_swe_raster)


#save data
terra::writeRaster(max_swe_ua,
                   filename = "E:/data-raw/ua/raster/annual/max_swe_ua.tif",
                   overwrite = TRUE
)

