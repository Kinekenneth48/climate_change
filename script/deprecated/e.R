library(terra)



# Specify the directory containing the .nc files, replace "./" with your directory path if needed
path <- "C:/Users/KENNETH/OneDrive - USU/Desktop/met/ACCESS1-0/historical"

# List all files that start with 'pr' and end with '.nc'
files <- list.files(path = path, pattern = "\\.nc$",
                         full.names = TRUE)


# Directory to save the cropped files
new_folder <- gsub("met", "met/tif", path)
dir.create(new_folder, recursive = TRUE)



tictoc::tic()
for (file in files) {
  # Load the file as a SpatRaster
  tif_file <- terra::rast(file)
  
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0(file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(tif_file, new_file,overwrite = TRUE)
}


#######################################################################



# Specify the directory containing the .nc files, replace "./" with your directory path if needed
path <- "C:/Users/KENNETH/OneDrive - USU/Desktop/met/ACCESS1-0/rcp45"

# List all files that start with 'pr' and end with '.nc'
files <- list.files(path = path, pattern = "\\.nc$",
                    full.names = TRUE)


# Directory to save the cropped files
new_folder <- gsub("met", "met/tif", path)
dir.create(new_folder, recursive = TRUE)




for (file in files) {
  # Load the file as a SpatRaster
  tif_file <- terra::rast(file)
  
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0(file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(tif_file, new_file,overwrite = TRUE)
}


#######################################################################


# Specify the directory containing the .nc files, replace "./" with your directory path if needed
path <- "C:/Users/KENNETH/OneDrive - USU/Desktop/met/ACCESS1-0/rcp85"

# List all files that start with 'pr' and end with '.nc'
files <- list.files(path = path, pattern = "\\.nc$",
                    full.names = TRUE)


# Directory to save the cropped files
new_folder <- gsub("met", "met/tif", path)
dir.create(new_folder, recursive = TRUE)



tictoc::tic()
for (file in files) {
  # Load the file as a SpatRaster
  tif_file <- terra::rast(file)
  
  
  # Create a new file name
  # Modify the file name and change extension to .tif
  file_base <- basename(file)
  file_base <- sub("\\.nc$", "", file_base)  # Remove the .nc extension
  new_file <- file.path(new_folder, paste0(file_base, ".tif"))
  
  
  # Save the cropped file
  terra::writeRaster(tif_file, new_file,overwrite = TRUE)
}
