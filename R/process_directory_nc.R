
# Function to process each directory
process_directory_nc <- function(dir_path) {
  # List .nc files in the directory
  nc_files <- list.files(dir_path, pattern = "\\.nc$", full.names = TRUE)
  
  # Initialize an empty list to store raster layers
  raster_layers <- list()
  
  # Loop over each file, convert to raster, and add to the list
  for (nc_file in nc_files) {
    # Read the NetCDF file as a SpatRaster
    r <- terra::rast(nc_file)
    
    # Assuming you want all layers, otherwise, you might need to select specific layers
    raster_layers[[length(raster_layers) + 1]] <- r
  }
  
  # Stack all rasters in the list to create a multi-layer raster
  stack_r <- terra::rast(raster_layers)
  
  # Determine the range of years to process
  years <- unique(year(time(stack_r)))
  years <- years[-length(years)]
  
  # Initialize a list to store the annual max rasters
  annual_max_rasters <- list()
  
  # Loop through each year and calculate the annual max snow depth
  for (year in years) {
    annual_max_rasters[[length(annual_max_rasters) + 1]] <- calculate_annual_max(stack_r, year)
  }
  
  # Calculate the average of each layer in the stack
  annual_raster <- rast(annual_max_rasters)
  
  # Return the average raster
  return(annual_raster)
}