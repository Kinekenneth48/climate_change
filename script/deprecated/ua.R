###############################################################################
### STEP 0 : INITIAL SET UP
#################################################################################

# ============================================================================#
# load the required R packages
# ============================================================================#
library(tidyverse)
library(sp)
library(future.apply)
library(fitdistrplus)
library(sf)
library(raster)
library(stars)
library(GoFKernel)
library(snow) # parallel compute for raster

# ============================================================================#
# get file path for netCDF files:UA
# ============================================================================#

path <- "data-raw/nc/"

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
# load UA data and create max grid maps
# ============================================================================#


for (j in 1:length(file_paths)) {
  # load nc file using terra
  ua_raster <- terra::rast(x = file_paths[j])
  ua_raster <- ua_raster[[1:365]] # get only SWE
  
  ua_max_raster <- ua_raster[[1]][[1]] # get first layer of SWE
  
  for (i in 2:(terra::nlyr(ua_raster))) {
    # retrieving the max values
    ua_max_raster <- max(ua_max_raster, ua_raster[[i]][[1]])
  }
  
  
  # Extract the year after "WY"
  year <- base::gsub("^.*WY(\\d{4}).*$", "\\1", file_paths[j])
  
  
  # Set the file name
  file_name <- paste0("ua_max_gridded_", year, ".tif")
  
  # assign name to results
  #  assign(paste0("ua_max_gridded_", year), ua_max_raster)
  
  
  # Set the output directory
  output_dir <- "data-raw/RObject"
  
  
  # Save the raster as a GeoTIFF file
  terra::writeRaster(ua_max_raster,
                     filename = file.path(output_dir, file_name),
                     overwrite = TRUE
  )
}



# ============================================================================#
# grab all values for  ecdf (UA data) and create brick
# ============================================================================#

# create a list to hold raster cell values
ua_overlap <- c()

# Create an empty list to store the raster layers
r_ua_layers <- list()

# create a vector of years from 2004 to 2021
years <- 2004:2021



for (year in years) {
  # construct the file path for the data for the current year
  file_path <- paste0("data-raw/RObject/ua_max_gridded_", year, ".tif")
  
  # load the data for the current year
  # load(file_path)
  
  # Load the raster object
  ua_max_raster <- raster(file_path)
  
  
  # # assign file name a generic name called data
  # file_name <- basename(file_path)
  # file_name <- gsub("\\.RData$", "", file_name)
  # assign("ua_raster", get(file_name))
  #
  
  # get cell values from raster
  ua_raster_values <- raster::getValues(ua_max_raster)
  
  # Filter out the zero values and NAN
  ua_raster_values <- subset(ua_raster_values, ua_raster_values > 2.54)
  
  # append values
  ua_overlap <- c(ua_overlap, ua_raster_values)
  
  # Append the raster layer to the list
  r_ua_layers[[which(years == year)]] <- ua_max_raster
}

# Create a raster brick from the list of raster layers
r_ua_brick <- raster::brick(r_ua_layers)


# Save the raster as a GeoTIFF file
raster::writeRaster(r_ua_brick,
                    filename = "data-raw/RObject/r_ua_brick.tif",
                    overwrite = TRUE
)