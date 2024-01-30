library(terra)


shapefile <- vect("data-raw/bear_lake_shape_file/id_ut8_1601.shp")



# ============================================================================#
# get file path for netCDF files:UA
# ============================================================================#

# Define the path
path <- "D:/data-raw/ua/day"

# Define the pattern to match filenames from 1982 to 2014
pattern <- "4km_SWE_Depth_WY"

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
for (j in 1:length(file_list)) {
  # Load nc file using terra
  ua_raster <- terra::rast(x = file_list[j])

  # Check if it's a leap year or not
  if (terra::nlyr(ua_raster) == 732) {
    ua_raster <- ua_raster[[1:366]] # Get only SWE for leap year
  } else {
    ua_raster <- ua_raster[[1:365]] # Get only SWE for non-leap year
  }

  # crop raster
  crop_raster <- crop(x = ua_raster, y = shapefile, mask = FALSE)

  all_daily_layers[[j]] <- crop_raster
}
tictoc::toc()


# Stack all daily layers into a single multi-layer raster
ua_swe_daily_bearlake <- terra::rast(all_daily_layers)


# save data
terra::writeRaster(ua_swe_daily_bearlake,
                   filename = "data-raw/bear_lake_shape_file/data/ua_swe_daily_bearlake.tif",
                   overwrite = TRUE
)
