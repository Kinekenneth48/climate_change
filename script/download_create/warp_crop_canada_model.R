################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(sf)
library(fs) # For file and directory operations
library(tools)
library(terra)


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

prism_mask_canada <- rast("data-raw/mask/prism_mask_canada.tif")
prism <- terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]

# ============================================================================#
# create a mask
# ============================================================================#
data = rast("D:/data-raw/canada_model/snw/warp/NAM-44_CCCma-CanESM2_historical-r1/r1i1p1/snw_NAM-44_CCCma-CanESM2_historical-r1_r1i1p1_CCCma-CanRCM4_r2_mon_195001-195012_warped.nc")
prism <- terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]

ext <- c(-126, -66, 23.375, 54)
data_crop = crop(data, ext)

prism_mask_canada <- terra::project(prism, data_crop[[1]])

# plot(prism_mask_canada)
# plot(conus, add=TRUE)
# 
# rf_pred_raster <- terra::mask(data_crop, prism_mask_canada)

writeRaster(prism_mask_canada,
            "data-raw/mask/prism_mask_canada.tif",
            overwrite = TRUE
)


################################################################################
## STEP 1: crop warped canada model
################################################################################
# Function to reproject .nc files in a single subdirectory
mask_nc_files_in_subdir <- function(source_subdir, dest_subdir) {
  # Ensure the destination subdirectory exists
  dir_create(dest_subdir)

  # List all .nc files in the source subdirectory
  nc_files <- dir_ls(source_subdir, glob = "*.nc")

  # Loop over the .nc files and reproject them
  for (nc_file in nc_files) {
    dest_file_path <- file.path(
      dest_subdir,
      paste0(tools::file_path_sans_ext(basename(nc_file)), "_mask.tif")
    )

    raster <- rast(nc_file)
    time_file_path = gsub("_warped.nc", ".nc", gsub("/warp/", "/", nc_file))
    time_raster = rast(time_file_path)
    
    crs(raster) <- "EPSG:4326"
    #raster_crop <- crop(raster, ext(prism[[1]]))
    raster_crop <- crop(raster, ext)
    time(raster_crop) = time(time_raster)
    
    raster_mask <- terra::mask(raster_crop, prism_mask_canada)



    writeRaster(raster_mask,
      dest_file_path,
      overwrite = TRUE
    )

  }
}

# ============================================================================#
#  R1
# ============================================================================#
# Base directories
base_source_dir <- "D:/data-raw/canada_model/snw/warp/NAM-44_CCCma-CanESM2_historical-r1"
base_dest_dir <- "D:/data-raw/canada_model/snw/warp_mask/NAM-44_CCCma-CanESM2_historical-r1"

# Create the base destination directory if it doesn't exist
dir_create(base_dest_dir)

# List direct subdirectories of the base source directory
subdirs_r1 <- list.dirs(base_source_dir, full.names = TRUE, recursive = FALSE)


# Loop over each subdirectory, reproject its .nc files, and save them in the corresponding new subdirectory
for (subdir in subdirs_r1) {
  subdir_name <- basename(subdir)
  
  new_dest_subdir <- file.path(base_dest_dir, subdir_name)

  mask_nc_files_in_subdir(subdir, new_dest_subdir)
}

#r =rast("D:/data-raw/canada_model/snw/warp_mask/NAM-44_CCCma-CanESM2_historical-r1/r10i2p1/snw_NAM-44_CCCma-CanESM2_historical-r1_r10i2p1_CCCma-CanRCM4_r2_mon_195001-195012_warped_mask.tif")
#rr =rast("D:/data-raw/canada_model/snw/NAM-44_CCCma-CanESM2_historical-r1/r10i2p1/snw_NAM-44_CCCma-CanESM2_historical-r1_r10i2p1_CCCma-CanRCM4_r2_mon_195001-195012.nc")

# ============================================================================#
#  R2
# ============================================================================#
# Base directories
base_source_dir <- "D:/data-raw/canada_model/snw/warp/NAM-44_CCCma-CanESM2_historical-r2"
base_dest_dir <- "D:/data-raw/canada_model/snw/warp_mask/NAM-44_CCCma-CanESM2_historical-r2"

# Create the base destination directory if it doesn't exist
dir_create(base_dest_dir)

# List direct subdirectories of the base source directory
subdirs_r2 <- list.dirs(base_source_dir, full.names = TRUE, recursive = FALSE)


# Loop over each subdirectory, reproject its .nc files, and save them in the corresponding new subdirectory
for (subdir in subdirs_r2) {
  subdir_name <- basename(subdir)
  new_dest_subdir <- file.path(base_dest_dir, subdir_name)

  mask_nc_files_in_subdir(subdir, new_dest_subdir)
}



# ============================================================================#
#  R3
# ============================================================================#
# Base directories
base_source_dir <- "D:/data-raw/canada_model/snw/warp/NAM-44_CCCma-CanESM2_historical-r3"
base_dest_dir <- "D:/data-raw/canada_model/snw/warp_mask/NAM-44_CCCma-CanESM2_historical-r3"

# Create the base destination directory if it doesn't exist
dir_create(base_dest_dir)

# List direct subdirectories of the base source directory
subdirs_r3 <- list.dirs(base_source_dir, full.names = TRUE, recursive = FALSE)


# Loop over each subdirectory, reproject its .nc files, and save them in the corresponding new subdirectory
for (subdir in subdirs_r3) {
  subdir_name <- basename(subdir)
  new_dest_subdir <- file.path(base_dest_dir, subdir_name)

  mask_nc_files_in_subdir(subdir, new_dest_subdir)
}


# ============================================================================#
#  R4
# ============================================================================#
# Base directories
base_source_dir <- "D:/data-raw/canada_model/snw/warp/NAM-44_CCCma-CanESM2_historical-r4"
base_dest_dir <- "D:/data-raw/canada_model/snw/warp_mask/NAM-44_CCCma-CanESM2_historical-r4"

# Create the base destination directory if it doesn't exist
dir_create(base_dest_dir)

# List direct subdirectories of the base source directory
subdirs_r4 <- list.dirs(base_source_dir, full.names = TRUE, recursive = FALSE)


# Loop over each subdirectory, reproject its .nc files, and save them in the corresponding new subdirectory
for (subdir in subdirs_r4) {
  subdir_name <- basename(subdir)
  new_dest_subdir <- file.path(base_dest_dir, subdir_name)

  mask_nc_files_in_subdir(subdir, new_dest_subdir)
}



# ============================================================================#
#  R5
# ============================================================================#
# Base directories
base_source_dir <- "D:/data-raw/canada_model/snw/warp/NAM-44_CCCma-CanESM2_historical-r5"
base_dest_dir <- "D:/data-raw/canada_model/snw/warp_mask/NAM-44_CCCma-CanESM2_historical-r5"

# Create the base destination directory if it doesn't exist
dir_create(base_dest_dir)

# List direct subdirectories of the base source directory
subdirs_r5 <- list.dirs(base_source_dir, full.names = TRUE, recursive = FALSE)


# Loop over each subdirectory, reproject its .nc files, and save them in the corresponding new subdirectory
for (subdir in subdirs_r5) {
  subdir_name <- basename(subdir)
  new_dest_subdir <- file.path(base_dest_dir, subdir_name)

  mask_nc_files_in_subdir(subdir, new_dest_subdir)
}
