################################################################################
## STEP 0: INITIAL SETUP
################################################################################
library(terra)

# bear lake shapefile
shapefile <- vect("data-raw/bear_lake_shape_file/id_ut8_1601.shp")


################################################################################
## STEP 1: ACCESS-CM2 pr historical
################################################################################

# ============================================================================#
# get file path for netCDF files - ACCESS-CM2 pr historical
# ============================================================================#

# Define the path
path <- "E:/data-raw/loca/ACCESS-CM2/pr/historical/day"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = "\\.nc$",
  full.names = TRUE
)


# Load and project the shapefile (assuming 'shapefile' is loaded)
shapefile_reprojected <- project(shapefile, crs(rast(file_list[1])))
shapefile_reprojected <- terra::shift(shapefile_reprojected, dx = 360)

# Directory to save the cropped files
new_folder <- "D:/data-raw/bear_lake/pr/historical/day"
dir.create(new_folder, recursive = TRUE)


# Loop through the files, crop and save them
tictoc::tic()
for (file in file_list) {
  # Load the file as a SpatRaster
  raster <- terra::rast(file)

  # Crop the raster
  cropped <- crop(x = raster, y = shapefile_reprojected, mask = TRUE)

  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0("bearlake_", file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(cropped, new_file,overwrite = TRUE)
}
tictoc::toc()




################################################################################
## STEP 2: ACCESS-CM2 pr future
################################################################################

# ============================================================================#
# get file path for netCDF files - ACCESS-CM2 pr future
# ============================================================================#

# Define the path
path <- "E:/data-raw/loca/ACCESS-CM2/pr/future/day"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = "\\.nc$",
  full.names = TRUE
)


# Load and project the shapefile (assuming 'shapefile' is loaded)
shapefile_reprojected <- project(shapefile, crs(rast(file_list[1])))
shapefile_reprojected <- terra::shift(shapefile_reprojected, dx = 360)

# Directory to save the cropped files
new_folder <- "D:/data-raw/bear_lake/pr/future/day"
dir.create(new_folder, recursive = TRUE)


# Loop through the files, crop and save them
tictoc::tic()
for (file in file_list) {
  # Load the file as a SpatRaster
  raster <- terra::rast(file)
  
  # Crop the raster
  cropped <- crop(x = raster, y = shapefile_reprojected, mask = TRUE)
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0("bearlake_", file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(cropped, new_file,overwrite = TRUE)
}
tictoc::toc()


################################################################################
## STEP 3: ACCESS-CM2 tasmax historical
################################################################################

# ============================================================================#
# get file path for netCDF files - ACCESS-CM2 tasmax historical
# ============================================================================#

# Define the path
path <- "E:/data-raw/loca/ACCESS-CM2/tasmax/historical/day"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = "\\.nc$",
  full.names = TRUE
)


# Load and project the shapefile (assuming 'shapefile' is loaded)
shapefile_reprojected <- project(shapefile, crs(rast(file_list[1])))
shapefile_reprojected <- terra::shift(shapefile_reprojected, dx = 360)

# Directory to save the cropped files
new_folder <- "D:/data-raw/bear_lake/tasmax/historical/day"
dir.create(new_folder, recursive = TRUE)


# Loop through the files, crop and save them
tictoc::tic()
for (file in file_list) {
  # Load the file as a SpatRaster
  raster <- terra::rast(file)
  
  # Crop the raster
  cropped <- crop(x = raster, y = shapefile_reprojected, mask = TRUE)
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0("bearlake_", file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(cropped, new_file,overwrite = TRUE)
}
tictoc::toc()




################################################################################
## STEP 4: ACCESS-CM2 tasmax future
################################################################################

# ============================================================================#
# get file path for netCDF files - ACCESS-CM2 tasmax future
# ============================================================================#

# Define the path
path <- "E:/data-raw/loca/ACCESS-CM2/tasmax/future/day"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = "\\.nc$",
  full.names = TRUE
)


# Load and project the shapefile (assuming 'shapefile' is loaded)
shapefile_reprojected <- project(shapefile, crs(rast(file_list[1])))
shapefile_reprojected <- terra::shift(shapefile_reprojected, dx = 360)

# Directory to save the cropped files
new_folder <- "D:/data-raw/bear_lake/tasmax/future/day"
dir.create(new_folder, recursive = TRUE)


# Loop through the files, crop and save them
tictoc::tic()
for (file in file_list) {
  # Load the file as a SpatRaster
  raster <- terra::rast(file)
  
  # Crop the raster
  cropped <- crop(x = raster, y = shapefile_reprojected, mask = TRUE)
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0("bearlake_", file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(cropped, new_file,overwrite = TRUE)
}
tictoc::toc()




################################################################################
## STEP 5: ACCESS-CM2 tasmin historical
################################################################################

# ============================================================================#
# get file path for netCDF files - ACCESS-CM2 tasmin historical
# ============================================================================#

# Define the path
path <- "E:/data-raw/loca/ACCESS-CM2/tasmin/historical/day"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = "\\.nc$",
  full.names = TRUE
)


# Load and project the shapefile (assuming 'shapefile' is loaded)
shapefile_reprojected <- project(shapefile, crs(rast(file_list[1])))
shapefile_reprojected <- terra::shift(shapefile_reprojected, dx = 360)

# Directory to save the cropped files
new_folder <- "D:/data-raw/bear_lake/tasmin/historical/day"
dir.create(new_folder, recursive = TRUE)


# Loop through the files, crop and save them
tictoc::tic()
for (file in file_list) {
  # Load the file as a SpatRaster
  raster <- terra::rast(file)
  
  # Crop the raster
  cropped <- crop(x = raster, y = shapefile_reprojected, mask = TRUE)
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0("bearlake_", file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(cropped, new_file,overwrite = TRUE)
}
tictoc::toc()




################################################################################
## STEP 6: ACCESS-CM2 tasmin future
################################################################################

# ============================================================================#
# get file path for netCDF files - ACCESS-CM2 tasmin future
# ============================================================================#

# Define the path
path <- "E:/data-raw/loca/ACCESS-CM2/tasmin/future/day"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = "\\.nc$",
  full.names = TRUE
)


# Load and project the shapefile (assuming 'shapefile' is loaded)
shapefile_reprojected <- project(shapefile, crs(rast(file_list[1])))
shapefile_reprojected <- terra::shift(shapefile_reprojected, dx = 360)

# Directory to save the cropped files
new_folder <- "D:/data-raw/bear_lake/tasmin/future/day"
dir.create(new_folder, recursive = TRUE)


# Loop through the files, crop and save them
tictoc::tic()
for (file in file_list) {
  # Load the file as a SpatRaster
  raster <- terra::rast(file)
  
  # Crop the raster
  cropped <- crop(x = raster, y = shapefile_reprojected, mask = TRUE)
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0("bearlake_", file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(cropped, new_file,overwrite = TRUE)
}
tictoc::toc()
