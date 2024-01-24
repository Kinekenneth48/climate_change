###############################################################################
### STEP 0 : INITIAL SET UP
#################################################################################

# ============================================================================#
# load the required R packages
# ============================================================================#
library(terra)

terraOptions(memfrac = 0.80, verbose = TRUE)


###############################################################################
### STEP 1 : Create annual max for SWE historical
#################################################################################
# ============================================================================#
# get file path for netCDF files:UA
# ============================================================================#

# Define the path
path <- "D:/data-raw/NCAR/vic/ACCESS1-0/historical"

# Define the pattern to match filenames
pattern <- "SWE.*\\.nc"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = pattern,
  full.names = TRUE
)


# Initialize a list to hold the MAX raster layers
max_swe_raster <- list()


# ============================================================================#
# create the max SWE maps into a single multilayer file
# ============================================================================#
tictoc::tic() # 417.23 sec elapsed
# Loop through each file to extract daily raster layers
for (j in 1:(length(file_list) - 1)) {
  # Load nc file using terra
  swe_raster1 <- terra::rast(x = file_list[j])
  swe_raster2 <- terra::rast(x = file_list[j + 1])

  # subset raster based on water year
  swe_raster1 <- swe_raster1[[300:nlyr(swe_raster1)]]
  swe_raster2 <- swe_raster2[[1:299]]

  swe_raster <- c(swe_raster1, swe_raster2)

  max_swe_raster[[j]] <- max(swe_raster, na.rm = TRUE)
}
tictoc::toc()



# Generate a sequence of years from 1950 to 2005
years <- seq(1951, 2005, by = 1)

# combine max SWE maps into a single multilayer file
max_swe_vic_hist_access10 <- terra::rast(max_swe_raster)
time(max_swe_vic_hist_access10) <- years

# save data
terra::writeRaster(max_swe_vic_hist_access10,
  filename = "E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif",
  overwrite = TRUE
)



###############################################################################
### STEP 2 : Create annual max for SWE future (rcp45)
#################################################################################
# ============================================================================#
# get file path for netCDF files:UA
# ============================================================================#

# Define the path
path <- "D:/data-raw/NCAR/vic/ACCESS1-0/rcp45"

# Define the pattern to match filenames
pattern <- "SWE.*\\.nc"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = pattern,
  full.names = TRUE
)


# Initialize a list to hold the MAX raster layers
max_swe_raster <- list()


# ============================================================================#
# create the max SWE maps into a single multilayer file
# ============================================================================#
tictoc::tic() # 652.92 sec elapsed
# Loop through each file to extract daily raster layers
for (j in 1:(length(file_list) - 1)) {
  # Load nc file using terra
  swe_raster1 <- terra::rast(x = file_list[j])
  swe_raster2 <- terra::rast(x = file_list[j + 1])

  # subset raster based on water year
  swe_raster1 <- swe_raster1[[300:nlyr(swe_raster1)]]
  swe_raster2 <- swe_raster2[[1:299]]

  swe_raster <- c(swe_raster1, swe_raster2)

  max_swe_raster[[j]] <- max(swe_raster, na.rm = TRUE)
}
tictoc::toc()



# Generate a sequence of years from 2006 to 2100
years <- seq(2007, 2100, by = 1)

# combine max SWE maps into a single multilayer file
max_swe_vic_rcp45_access10 <- terra::rast(max_swe_raster)
time(max_swe_vic_rcp45_access10) <- years

# save data
terra::writeRaster(max_swe_vic_rcp45_access10,
  filename = "E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp45_access10.tif",
  overwrite = TRUE
)




###############################################################################
### STEP 3 : Create annual max for SWE future (rcp85)
#################################################################################
# ============================================================================#
# get file path for netCDF files:UA
# ============================================================================#

# Define the path
path <- "D:/data-raw/NCAR/vic/ACCESS1-0/rcp85"

# Define the pattern to match filenames
pattern <- "SWE.*\\.nc"

# Find all netCDF files that match the pattern
file_list <- list.files(
  path = path,
  pattern = pattern,
  full.names = TRUE
)


# Initialize a list to hold the MAX raster layers
max_swe_raster <- list()


# ============================================================================#
# create the max SWE maps into a single multilayer file
# ============================================================================#
tictoc::tic() # 652.92 sec elapsed
# Loop through each file to extract daily raster layers
for (j in 1:(length(file_list) - 1)) {
  # Load nc file using terra
  swe_raster1 <- terra::rast(x = file_list[j])
  swe_raster2 <- terra::rast(x = file_list[j + 1])

  # subset raster based on water year
  swe_raster1 <- swe_raster1[[300:nlyr(swe_raster1)]]
  swe_raster2 <- swe_raster2[[1:299]]

  swe_raster <- c(swe_raster1, swe_raster2)

  max_swe_raster[[j]] <- max(swe_raster, na.rm = TRUE)
}
tictoc::toc()



# Generate a sequence of years from 2006 to 2100
years <- seq(2007, 2100, by = 1)

# combine max SWE maps into a single multilayer file
max_swe_vic_rcp85_access10 <- terra::rast(max_swe_raster)
time(max_swe_vic_rcp85_access10) <- years

# save data
terra::writeRaster(max_swe_vic_rcp85_access10,
  filename = "E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp85_access10.tif",
  overwrite = TRUE
)
