################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages
# ============================================================================#
library(terra)


################################################################################
## STEP 1: Create a single raster for TMEAN
################################################################################
# Set your base directory
base_dir <- "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw/prism/tmean"

# List all .bil files in the directory and its subdirectories
all_files <- list.files(path = base_dir, pattern = "\\.bil$", full.names = TRUE, recursive = TRUE)

# Sort the file paths based on the filenames
sorted_files <- all_files[order(basename(all_files))]

# Initialize a variable to store the combined SpatRaster object
prism_monthly_tmean_raster <- NULL

# Loop over the sorted files and read each one
for (file in sorted_files) {
  # Read the .bil file
  monthly_raster <- rast(file)

  # If prism_monthly_tmean_raster is NULL, assign monthly_raster to it, else add monthly_raster as a new layer
  if (is.null(prism_monthly_tmean_raster)) {
    prism_monthly_tmean_raster <- monthly_raster
  } else {
    prism_monthly_tmean_raster <- c(prism_monthly_tmean_raster, monthly_raster)
  }
}


# Get the sources of the SpatRaster object
source_list <- terra::sources(prism_monthly_tmean_raster)

# Print the source list
print(source_list[1220:1236])


# Save the SpatRaster object as a raster dataset
terra::writeRaster(prism_monthly_tmean_raster,
  filename = "data-raw/prism/raster/prism_monthly_tmean_raster.tif",
  overwrite = TRUE
)

################################################################################
## STEP 2: Create a single raster for PPT
################################################################################

# Set your base directory
base_dir <- "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw/prism/ppt"

# List all .bil files in the directory and its subdirectories
all_files <- list.files(path = base_dir, pattern = "\\.bil$", full.names = TRUE, recursive = TRUE)

# Sort the file paths based on the filenames
sorted_files <- all_files[order(basename(all_files))]

# Initialize a variable to store the combined SpatRaster object
prism_monthly_ppt_raster <- NULL

# Loop over the sorted files and read each one
for (file in sorted_files) {
  # Read the .bil file
  monthly_raster <- rast(file)

  # If prism_monthly_ppt_raster is NULL, assign monthly_raster to it, else add monthly_raster as a new layer
  if (is.null(prism_monthly_ppt_raster)) {
    prism_monthly_ppt_raster <- monthly_raster
  } else {
    prism_monthly_ppt_raster <- c(prism_monthly_ppt_raster, monthly_raster)
  }
}


# Get the sources of the SpatRaster object
source_list <- terra::sources(prism_monthly_ppt_raster)

# Print the source list
print(source_list[1220:1236])


# Save the SpatRaster object as a raster dataset
terra::writeRaster(prism_monthly_ppt_raster,
  filename = "data-raw/prism/raster/prism_monthly_ppt_raster.tif",
  overwrite = TRUE
)
