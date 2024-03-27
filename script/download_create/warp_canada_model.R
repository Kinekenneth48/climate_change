################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(sf)
library(fs) # For file and directory operations
library(tools)


################################################################################
## STEP 1: warp canada model
################################################################################

# Function to reproject .nc files in a single subdirectory
reproject_nc_files_in_subdir <- function(source_subdir, dest_subdir) {
  # Ensure the destination subdirectory exists
  dir_create(dest_subdir)

  # List all .nc files in the source subdirectory
  nc_files <- dir_ls(source_subdir, glob = "*.nc")

  # Loop over the .nc files and reproject them
  for (nc_file in nc_files) {
    dest_file_path <- file.path(
      dest_subdir,
      paste0(tools::file_path_sans_ext(basename(nc_file)), "_warped.nc")
    )
    # Reproject the .nc file
    sf::gdal_utils(
      util = "warp",
      source = nc_file,
      destination = dest_file_path,
      options = c("-t_srs", "EPSG:4326")
    )

    cat("Reprojected ", nc_file, " to ", dest_file_path, "\n")
  }
}

# ============================================================================#
#  R1
# ============================================================================#
# Base directories
base_source_dir <- "D:/data-raw/canada_model/snw/NAM-44_CCCma-CanESM2_historical-r1"
base_dest_dir <- "D:/data-raw/canada_model/snw/warp/NAM-44_CCCma-CanESM2_historical-r1"

# Create the base destination directory if it doesn't exist
dir_create(base_dest_dir)

# List direct subdirectories of the base source directory
subdirs_r1 <- list.dirs(base_source_dir, full.names = TRUE, recursive = FALSE)


# Loop over each subdirectory, reproject its .nc files, and save them in the corresponding new subdirectory
for (subdir in subdirs_r1) {
  subdir_name <- basename(subdir)
  new_dest_subdir <- file.path(base_dest_dir, subdir_name)

  reproject_nc_files_in_subdir(subdir, new_dest_subdir)
}

# ============================================================================#
#  R2
# ============================================================================#
# Base directories
base_source_dir <- "D:/data-raw/canada_model/snw/NAM-44_CCCma-CanESM2_historical-r2"
base_dest_dir <- "D:/data-raw/canada_model/snw/warp/NAM-44_CCCma-CanESM2_historical-r2"

# Create the base destination directory if it doesn't exist
dir_create(base_dest_dir)

# List direct subdirectories of the base source directory
subdirs_r2 <- list.dirs(base_source_dir, full.names = TRUE, recursive = FALSE)


# Loop over each subdirectory, reproject its .nc files, and save them in the corresponding new subdirectory
for (subdir in subdirs_r2) {
  subdir_name <- basename(subdir)
  new_dest_subdir <- file.path(base_dest_dir, subdir_name)
  
  reproject_nc_files_in_subdir(subdir, new_dest_subdir)
}



# ============================================================================#
#  R3
# ============================================================================#
# Base directories
base_source_dir <- "D:/data-raw/canada_model/snw/NAM-44_CCCma-CanESM2_historical-r3"
base_dest_dir <- "D:/data-raw/canada_model/snw/warp/NAM-44_CCCma-CanESM2_historical-r3"

# Create the base destination directory if it doesn't exist
dir_create(base_dest_dir)

# List direct subdirectories of the base source directory
subdirs_r3 <- list.dirs(base_source_dir, full.names = TRUE, recursive = FALSE)


# Loop over each subdirectory, reproject its .nc files, and save them in the corresponding new subdirectory
for (subdir in subdirs_r3) {
  subdir_name <- basename(subdir)
  new_dest_subdir <- file.path(base_dest_dir, subdir_name)
  
  reproject_nc_files_in_subdir(subdir, new_dest_subdir)
}



# ============================================================================#
#  R4
# ============================================================================#
# Base directories
base_source_dir <- "D:/data-raw/canada_model/snw/NAM-44_CCCma-CanESM2_historical-r4"
base_dest_dir <- "D:/data-raw/canada_model/snw/warp/NAM-44_CCCma-CanESM2_historical-r4"

# Create the base destination directory if it doesn't exist
dir_create(base_dest_dir)

# List direct subdirectories of the base source directory
subdirs_r4 <- list.dirs(base_source_dir, full.names = TRUE, recursive = FALSE)


# Loop over each subdirectory, reproject its .nc files, and save them in the corresponding new subdirectory
for (subdir in subdirs_r4) {
  subdir_name <- basename(subdir)
  new_dest_subdir <- file.path(base_dest_dir, subdir_name)
  
  reproject_nc_files_in_subdir(subdir, new_dest_subdir)
}



# ============================================================================#
#  R5
# ============================================================================#
# Base directories
base_source_dir <- "D:/data-raw/canada_model/snw/NAM-44_CCCma-CanESM2_historical-r5"
base_dest_dir <- "D:/data-raw/canada_model/snw/warp/NAM-44_CCCma-CanESM2_historical-r5"

# Create the base destination directory if it doesn't exist
dir_create(base_dest_dir)

# List direct subdirectories of the base source directory
subdirs_r5 <- list.dirs(base_source_dir, full.names = TRUE, recursive = FALSE)


# Loop over each subdirectory, reproject its .nc files, and save them in the corresponding new subdirectory
for (subdir in subdirs_r5) {
  subdir_name <- basename(subdir)
  new_dest_subdir <- file.path(base_dest_dir, subdir_name)
  
  reproject_nc_files_in_subdir(subdir, new_dest_subdir)
}
