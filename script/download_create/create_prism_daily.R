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
#base_dir <- "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw/prism/tmean"
base_dir <- "D:/data-raw/prism/day/tmean"

# List all .bil files in the directory and its subdirectories
all_files <- list.files(path = base_dir, pattern = "\\.bil$", full.names = TRUE, recursive = TRUE)

# Sort the file paths based on the filenames
sorted_files <- all_files[order(basename(all_files))]

# Initialize a variable to store the combined SpatRaster object
prism_day_tmean_raster <- NULL

# Loop over the sorted files and read each one
for (file in sorted_files) {
  # Read the .bil file
  day_raster <- terra::rast(file)
  
  # If prism_day_tmean_raster is NULL, assign day_raster to it, else add day_raster as a new layer
  if (is.null(prism_day_tmean_raster)) {
    prism_day_tmean_raster <- day_raster
  } else {
    prism_day_tmean_raster <- c(prism_day_tmean_raster, day_raster)
  }
}


# Extract date information from the filename
date_match <- regmatches(sorted_files[1], regexpr("\\d{8}", sorted_files[1]))
start_date <- as.Date("1981-01-01")

# Create a sequence of dates based on the number of layers
time_values <- seq(start_date, by = 1, length.out = nlyr(prism_day_tmean_raster))

#set time
terra::time(prism_day_tmean_raster) = time_values


# Save the SpatRaster object as a raster dataset
terra::writeRaster(prism_day_tmean_raster,
                   filename = "data-raw/prism/raster/prism_day_tmean_raster.tif",
                   overwrite = TRUE
)

################################################################################
## STEP 2: Create a single raster for PPT
################################################################################

# Set your base directory
base_dir <- "D:/data-raw/prism/day/ppt"


# List all .bil files in the directory and its subdirectories
all_files <- list.files(path = base_dir, pattern = "\\.bil$", full.names = TRUE, recursive = TRUE)

# Sort the file paths based on the filenames
sorted_files <- all_files[order(basename(all_files))]

# Initialize a variable to store the combined SpatRaster object
prism_day_ppt_raster <- NULL

# Loop over the sorted files and read each one
for (file in sorted_files) {
  # Read the .bil file
  day_raster <- rast(file)
  
  # If prism_day_ppt_raster is NULL, assign day_raster to it, else add day_raster as a new layer
  if (is.null(prism_day_ppt_raster)) {
    prism_day_ppt_raster <- day_raster
  } else {
    prism_day_ppt_raster <- c(prism_day_ppt_raster, day_raster)
  }
}




# Extract date information from the filename
date_match <- regmatches(sorted_files[1], regexpr("\\d{8}", sorted_files[1]))
start_date <- as.Date("1981-01-01")

# Create a sequence of dates based on the number of layers
time_values <- seq(start_date, by = 1, length.out = nlyr(prism_day_ppt_raster))

#set time
terra::time(prism_day_ppt_raster) = time_values



# Save the SpatRaster object as a raster dataset
terra::writeRaster(prism_day_ppt_raster,
                   filename = "data-raw/prism/raster/prism_day_ppt_raster.tif",
                   overwrite = TRUE
)
