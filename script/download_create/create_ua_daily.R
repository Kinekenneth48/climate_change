###############################################################################
### STEP 0 : INITIAL SET UP
#################################################################################

# ============================================================================#
# load the required R packages
# ============================================================================#
library(terra)

terraOptions(memfrac = 0.80, verbose = TRUE)

#loca_tas <- terra::rast("D:/data-raw/loca/ACCESS-CM2/tasmax/historical/day/tasmax.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA_16thdeg_v20220413.nc")
prism_ppt = rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")
# ============================================================================#
# get file path for netCDF files:UA
# ============================================================================#

# Define the path
path <- "D:/data-raw/ua/day"

# Define the pattern to match filenames from 1982 to 2014
pattern <- "4km_SWE_Depth_WY[0-9]{4}_v01.nc"


# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = pattern,
  full.names = TRUE
)


# Initialize a list to hold the daily raster layers
all_daily_layers <- list()






# ============================================================================#
# create the UA daily SWE maps into a single multilayer file
# ============================================================================#
tictoc::tic() 
# Loop through each file to extract daily raster layers
for (i in 1:length(file_list)) {
  # Load nc file using terra
  ua_raster <- terra::rast(x = file_list[i])
  
  # Check if it's a leap year or not
  if (terra::nlyr(ua_raster) == 732) {
    ua_raster <- ua_raster[[1:366]] # Get only SWE for leap year
  } else {
    ua_raster <- ua_raster[[1:365]] # Get only SWE for non-leap year
  }

 
    all_daily_layers[[length(all_daily_layers) + 1]] <- ua_raster
    
}
tictoc::toc()


# Stack all daily layers into a single multi-layer raster
tictoc::tic() 
ua_swe_combined_daily <- terra::rast(all_daily_layers)
tictoc::toc()


# Save the combined raster as a GeoTIFF file
tictoc::tic() 
writeRaster(ua_swe_combined_daily,
            "data-raw/ua/raster/combined/ua_swe_combined_daily.tif",
            overwrite = TRUE
)
tictoc::toc()