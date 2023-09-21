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

path <- "data-raw/ua/"

# find all netCDF files that contain "4km_SWE_Depth" in their filename
file_list <- list.files(
  path = path, pattern = "4km_SWE_Depth",
  full.names = TRUE
)

# create a list to hold the file paths
file_paths <- c()

# loop over each file and add its path to the list
for (file in file_list) {
  file_paths <- c(file_paths, file)
}


# ============================================================================#
# create the UA max monthly SWE maps
# ============================================================================#
for (j in 1:length(file_paths)) {
  # Load nc file using terra
  ua_raster <- terra::rast(x = file_paths[j])
  
  # Check if it's a leap year or not
  if (terra::nlyr(ua_raster) == 732) {
    ua_raster <- ua_raster[[1:366]]  # Get only SWE for leap year
    days_per_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  } else {
    ua_raster <- ua_raster[[1:365]]  # Get only SWE for non-leap year
    days_per_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }
  
  start_idx <- 1
  monthly_max_list <- list()
  
  # Loop through each month to find the monthly maximum
  for (month in 1:12) {
    end_idx <- start_idx + days_per_month[month] - 1
    monthly_data <- ua_raster[[start_idx:end_idx]]
    
    monthly_max <- monthly_data[[1]]
    
    for (i in 2:(terra::nlyr(monthly_data))) {
      monthly_max <- max(monthly_max, monthly_data[[i]])
    }
    
    monthly_max_list[[month]] <- monthly_max
    start_idx <- end_idx + 1
  }
  
  # Stack the monthly maximum layers into a single SpatRaster
  monthly_max_raster <- terra::rast(monthly_max_list)
  
  # Extract the year after "WY"
  year <- base::gsub("^.*WY(\\d{4}).*$", "\\1", file_paths[j])
  
  # Set the file name
  file_name <- paste0("ua_monthly_max_gridded_", year, ".tif")
  
  # Set the output directory
  output_dir <- "data-raw/ua/raster/month"
  
  # Save the raster as a GeoTIFF file
  terra::writeRaster(monthly_max_raster,
                     filename = file.path(output_dir, file_name),
                     overwrite = TRUE
  )
}
