################################################################################
## STEP 0: INITIAL SETUP
################################################################################
pacman::p_load(terra)

# bear lake shapefile
shapefile <- vect("data-raw/bear_lake_shape_file/id_ut8_1601.shp")


################################################################################
## STEP 1: ACCESS-10  historical
################################################################################

# ============================================================================#
# get file path for netCDF files - ACCESS-10  historical
# ============================================================================#

# Define the path
path <- "D:/data-raw/NCAR/met/ACCESS1-0/historical/"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = "\\.nc$",
  full.names = TRUE
)


# Load and project the shapefile (assuming 'shapefile' is loaded)
shapefile_reprojected <- project(shapefile, crs(rast(file_list[1])))
#shapefile_reprojected <- terra::shift(shapefile_reprojected, dx = 360)

# Directory to save the cropped files
new_folder <- gsub("data-raw", "data-raw/bear_lake", path)
dir.create(new_folder, recursive = TRUE)


# Loop through the files, crop and save them
tictoc::tic()
for (file in file_list) {
  # Load the file as a SpatRaster
  raster <- terra::rast(file)
  
  # Crop the raster
  cropped <- crop(x = raster, y = shapefile_reprojected, mask = FALSE)
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0("bearlake_", file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(cropped, new_file,overwrite = TRUE)
}


################################################################################
## STEP 2: ACCESS-10  r45
################################################################################

# ============================================================================#
# get file path for netCDF files 
# ============================================================================#

# Define the path
path <- "D:/data-raw/NCAR/met/ACCESS1-0/rcp45/"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = "\\.nc$",
  full.names = TRUE
)


# Load and project the shapefile (assuming 'shapefile' is loaded)
shapefile_reprojected <- project(shapefile, crs(rast(file_list[1])))
#shapefile_reprojected <- terra::shift(shapefile_reprojected, dx = 360)

# Directory to save the cropped files
new_folder <- gsub("data-raw", "data-raw/bear_lake", path)
dir.create(new_folder, recursive = TRUE)


# Loop through the files, crop and save them
tictoc::tic()
for (file in file_list) {
  # Load the file as a SpatRaster
  raster <- terra::rast(file)
  
  # Crop the raster
  cropped <- crop(x = raster, y = shapefile_reprojected, mask = FALSE)
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0("bearlake_", file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(cropped, new_file,overwrite = TRUE)
}




################################################################################
## STEP 3: ACCESS-10  r85
################################################################################

# ============================================================================#
# get file path for netCDF files 
# ============================================================================#

# Define the path
path <- "D:/data-raw/NCAR/met/ACCESS1-0/rcp85/"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = "\\.nc$",
  full.names = TRUE
)


# Load and project the shapefile (assuming 'shapefile' is loaded)
shapefile_reprojected <- project(shapefile, crs(rast(file_list[1])))
#shapefile_reprojected <- terra::shift(shapefile_reprojected, dx = 360)

# Directory to save the cropped files
new_folder <- gsub("data-raw", "data-raw/bear_lake", path)
dir.create(new_folder, recursive = TRUE)


# Loop through the files, crop and save them
tictoc::tic()
for (file in file_list) {
  # Load the file as a SpatRaster
  raster <- terra::rast(file)
  
  # Crop the raster
  cropped <- crop(x = raster, y = shapefile_reprojected, mask = FALSE)
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0("bearlake_", file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(cropped, new_file,overwrite = TRUE)
}




################################################################################
## STEP 4: ACCESS-10  hist VIC
################################################################################

# ============================================================================#
# get file path for netCDF files 
# ============================================================================#

# Define the path
path <- "D:/data-raw/NCAR/vic/ACCESS1-0/historical/"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = "\\.nc$",
  full.names = TRUE
)


# Load and project the shapefile (assuming 'shapefile' is loaded)
shapefile_reprojected <- project(shapefile, crs(rast(file_list[1])))
#shapefile_reprojected <- terra::shift(shapefile_reprojected, dx = 360)

# Directory to save the cropped files
new_folder <- gsub("data-raw", "data-raw/bear_lake", path)
dir.create(new_folder, recursive = TRUE)


# Loop through the files, crop and save them
tictoc::tic()
for (file in file_list) {
  # Load the file as a SpatRaster
  raster <- terra::rast(file)
  
  # Crop the raster
  cropped <- crop(x = raster, y = shapefile_reprojected, mask = FALSE)
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0("bearlake_", file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(cropped, new_file,overwrite = TRUE)
}



################################################################################
## STEP 5: ACCESS-10  hist r45
################################################################################

# ============================================================================#
# get file path for netCDF files 
# ============================================================================#

# Define the path
path <- "D:/data-raw/NCAR/vic/ACCESS1-0/rcp45/"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = "\\.nc$",
  full.names = TRUE
)


# Load and project the shapefile (assuming 'shapefile' is loaded)
shapefile_reprojected <- project(shapefile, crs(rast(file_list[1])))
#shapefile_reprojected <- terra::shift(shapefile_reprojected, dx = 360)

# Directory to save the cropped files
new_folder <- gsub("data-raw", "data-raw/bear_lake", path)
dir.create(new_folder, recursive = TRUE)


# Loop through the files, crop and save them
tictoc::tic()
for (file in file_list) {
  # Load the file as a SpatRaster
  raster <- terra::rast(file)
  
  # Crop the raster
  cropped <- crop(x = raster, y = shapefile_reprojected, mask = FALSE)
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0("bearlake_", file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(cropped, new_file,overwrite = TRUE)
}





################################################################################
## STEP 6: ACCESS-10  hist r85
################################################################################

# ============================================================================#
# get file path for netCDF files 
# ============================================================================#

# Define the path
path <- "D:/data-raw/NCAR/vic/ACCESS1-0/rcp85/"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = "\\.nc$",
  full.names = TRUE
)


# Load and project the shapefile (assuming 'shapefile' is loaded)
shapefile_reprojected <- project(shapefile, crs(rast(file_list[1])))
#shapefile_reprojected <- terra::shift(shapefile_reprojected, dx = 360)

# Directory to save the cropped files
new_folder <- gsub("data-raw", "data-raw/bear_lake", path)
dir.create(new_folder, recursive = TRUE)


# Loop through the files, crop and save them
tictoc::tic()
for (file in file_list) {
  # Load the file as a SpatRaster
  raster <- terra::rast(file)
  
  # Crop the raster
  cropped <- crop(x = raster, y = shapefile_reprojected, mask = FALSE)
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0("bearlake_", file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(cropped, new_file,overwrite = TRUE)
}





