###############################################################################
### STEP 0 : INITIAL SET UP
#################################################################################

# ============================================================================#
# load the required R packages
# ============================================================================#
library(terra)
library(lubridate)
terraOptions(memfrac = 0.80, verbose = TRUE)


###############################################################################
### STEP 1 : Create annual max for SWE historical
#################################################################################
# ============================================================================#
# get file path for netCDF files:UA
# ============================================================================#

# Define the path
path <- "D:/data-raw/NCAR/vic/CCSM4/historical"

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
# Loop through each pair of files in the file_list except the last one
for (j in 1:(length(file_list) - 1)) {
  # Open NetCDF files for reading using ncdf4 package
  # nc_details_1 = ncdf4::nc_open(file_list[[j]])
  # nc_details_2 = ncdf4::nc_open(file_list[[j+1]])
  
  # Load the raster data from NetCDF files using the terra package
  swe_raster1 <- terra::rast(x = file_list[j])
  swe_raster2 <- terra::rast(x = file_list[j + 1])
  
  # Determine the number of layers in each raster
  len1 <- length(names(swe_raster1))
  len2 <- length(names(swe_raster2))
  
  # Calculate the start and end dates for each raster based on their layer names
  # Assumes layer names include numeric dates in a format that needs cleansing
  first_date1 <- ymd("1800-01-01") + as.numeric(gsub("[^0-9]", "", names(swe_raster1)[1]))
  end_date1 <- ymd("1800-01-01") + as.numeric(gsub("[^0-9]", "", names(swe_raster1)[len1]))
  first_date2 <- ymd("1800-01-01") + as.numeric(gsub("[^0-9]", "", names(swe_raster2)[1]))
  end_date2 <- ymd("1800-01-01") + as.numeric(gsub("[^0-9]", "", names(swe_raster2)[len2]))
  
  # Generate sequences of dates for each raster based on the calculated start and end dates
  date_sequence1 <- seq(from = first_date1, to = end_date1, by = "day")
  date_sequence2 <- seq(from = first_date2, to = end_date2, by = "day")
  
  # Assign the date sequences to the rasters, effectively setting their time dimension
  time(swe_raster1) <- date_sequence1
  time(swe_raster2) <- date_sequence2
  
  # Combine the two sequential rasters into a single raster stack for further processing
  swe_raster <- c(swe_raster1, swe_raster2)
  
  # Extract the years and months from the combined raster time dimension
  years <- as.numeric(format(time(swe_raster), "%Y"))
  months <- as.numeric(format(time(swe_raster), "%m"))
  
  # Identify the unique years within the raster data
  unique_years <- unique(years)
  
  # Create a mask to identify raster layers that fall within a water year
  # A water year is defined from October of the previous year to September of the current year
  water_year_mask <- (years == unique_years[1] & months >= 10) |
    (years == unique_years[2] & months <= 9)
  
  # Subset the raster stack to only include layers that fall within the defined water year
  swe_snow_year_subset <- swe_raster[[which(water_year_mask)]]
  
  # Calculate and store the maximum SWE value from the subsetted raster layers
  # Presumably, max_swe_raster is initialized earlier in the script
  swe_max <- max(swe_raster, na.rm = TRUE)
  time(swe_max) <- unique_years[2]
  max_swe_raster[[j]] <- swe_max
}



# combine max SWE maps into a single multilayer file
max_swe_vic_hist_CCSM4 <- terra::rast(max_swe_raster)


# save data
terra::writeRaster(max_swe_vic_hist_CCSM4,
                   filename = "E:/data-raw/NCAR/CCSM4/historical/max_swe_vic_hist_CCSM4.tif",
                   overwrite = TRUE
)



###############################################################################
### STEP 2 : Create annual max for SWE future (rcp45)
#################################################################################
# ============================================================================#
# get file path for netCDF files:UA
# ============================================================================#

# Define the path
path <- "D:/data-raw/NCAR/vic/CCSM4/rcp45"

# Define the pattern to match filenames
pattern <- "SWE.*\\.nc"

# Find all netCDF files that match the pattern
file_listr45 <- list.files(
  path = path,
  pattern = pattern,
  full.names = TRUE
)

file_listr45 = c("D:/data-raw/NCAR/vic/CCSM4/historical/SWE.2005.v0.nc",
                 file_listr45)

# Initialize a list to hold the MAX raster layers
max_swe_raster_r45 <- list()


# ============================================================================#
# create the max SWE maps into a single multilayer file
# ============================================================================#

# Loop through each file to extract daily raster layers
for (j in 1:(length(file_listr45) - 1)) {
  # Open NetCDF files for reading using ncdf4 package
  # nc_details_1 = ncdf4::nc_open(file_listr45[[j]])
  # nc_details_2 = ncdf4::nc_open(file_listr45[[j+1]])
  
  # Load the raster data from NetCDF files using the terra package
  swe_raster1 <- terra::rast(x = file_listr45[j])
  swe_raster2 <- terra::rast(x = file_listr45[j + 1])
  
  # Determine the number of layers in each raster
  len1 <- length(names(swe_raster1))
  len2 <- length(names(swe_raster2))
  
  # Calculate the start and end dates for each raster based on their layer names
  # Assumes layer names include numeric dates in a format that needs cleansing
  first_date1 <- ymd("1800-01-01") + as.numeric(gsub("[^0-9]", "", names(swe_raster1)[1]))
  end_date1 <- ymd("1800-01-01") + as.numeric(gsub("[^0-9]", "", names(swe_raster1)[len1]))
  first_date2 <- ymd("1800-01-01") + as.numeric(gsub("[^0-9]", "", names(swe_raster2)[1]))
  end_date2 <- ymd("1800-01-01") + as.numeric(gsub("[^0-9]", "", names(swe_raster2)[len2]))
  
  # Generate sequences of dates for each raster based on the calculated start and end dates
  date_sequence1 <- seq(from = first_date1, to = end_date1, by = "day")
  date_sequence2 <- seq(from = first_date2, to = end_date2, by = "day")
  
  # Assign the date sequences to the rasters, effectively setting their time dimension
  time(swe_raster1) <- date_sequence1
  time(swe_raster2) <- date_sequence2
  
  # Combine the two sequential rasters into a single raster stack for further processing
  swe_raster <- c(swe_raster1, swe_raster2)
  
  # Extract the years and months from the combined raster time dimension
  years <- as.numeric(format(time(swe_raster), "%Y"))
  months <- as.numeric(format(time(swe_raster), "%m"))
  
  # Identify the unique years within the raster data
  unique_years <- unique(years)
  
  # Create a mask to identify raster layers that fall within a water year
  # A water year is defined from October of the previous year to September of the current year
  water_year_mask <- (years == unique_years[1] & months >= 10) |
    (years == unique_years[2] & months <= 9)
  
  # Subset the raster stack to only include layers that fall within the defined water year
  swe_snow_year_subset <- swe_raster[[which(water_year_mask)]]
  
  # Calculate and store the maximum SWE value from the subsetted raster layers
  # Presumably, max_swe_raster is initialized earlier in the script
  swe_max <- max(swe_raster, na.rm = TRUE)
  time(swe_max) <- unique_years[2]
  max_swe_raster_r45[[j]] <- swe_max
}





# combine max SWE maps into a single multilayer file
max_swe_vic_rcp45_CCSM4 <- terra::rast(max_swe_raster_r45)


# save data
terra::writeRaster(max_swe_vic_rcp45_CCSM4,
                   filename = "E:/data-raw/NCAR/CCSM4/future/max_swe_vic_rcp45_CCSM4.tif",
                   overwrite = TRUE
)




###############################################################################
### STEP 3 : Create annual max for SWE future (rcp85)
#################################################################################
# ============================================================================#
# get file path for netCDF files:UA
# ============================================================================#

# Define the path
path <- "D:/data-raw/NCAR/vic/CCSM4/rcp85"

# Define the pattern to match filenames
pattern <- "SWE.*\\.nc"

# Find all netCDF files that match the pattern
file_listr85 <- list.files(
  path = path,
  pattern = pattern,
  full.names = TRUE
)

file_listr85 = c("D:/data-raw/NCAR/vic/CCSM4/historical/SWE.2005.v0.nc",
                 file_listr85)

# Initialize a list to hold the MAX raster layers
max_swe_raster_r85 <- list()


# ============================================================================#
# create the max SWE maps into a single multilayer file
# ============================================================================#

# Loop through each file to extract daily raster layers
for (j in 1:(length(file_listr85) - 1)) {
  # Open NetCDF files for reading using ncdf4 package
  # nc_details_1 = ncdf4::nc_open(file_listr85[[j]])
  # nc_details_2 = ncdf4::nc_open(file_listr85[[j+1]])
  
  # Load the raster data from NetCDF files using the terra package
  swe_raster1 <- terra::rast(x = file_listr85[j])
  swe_raster2 <- terra::rast(x = file_listr85[j + 1])
  
  # Determine the number of layers in each raster
  len1 <- length(names(swe_raster1))
  len2 <- length(names(swe_raster2))
  
  # Calculate the start and end dates for each raster based on their layer names
  # Assumes layer names include numeric dates in a format that needs cleansing
  first_date1 <- ymd("1800-01-01") + as.numeric(gsub("[^0-9]", "", names(swe_raster1)[1]))
  end_date1 <- ymd("1800-01-01") + as.numeric(gsub("[^0-9]", "", names(swe_raster1)[len1]))
  first_date2 <- ymd("1800-01-01") + as.numeric(gsub("[^0-9]", "", names(swe_raster2)[1]))
  end_date2 <- ymd("1800-01-01") + as.numeric(gsub("[^0-9]", "", names(swe_raster2)[len2]))
  
  # Generate sequences of dates for each raster based on the calculated start and end dates
  date_sequence1 <- seq(from = first_date1, to = end_date1, by = "day")
  date_sequence2 <- seq(from = first_date2, to = end_date2, by = "day")
  
  # Assign the date sequences to the rasters, effectively setting their time dimension
  time(swe_raster1) <- date_sequence1
  time(swe_raster2) <- date_sequence2
  
  # Combine the two sequential rasters into a single raster stack for further processing
  swe_raster <- c(swe_raster1, swe_raster2)
  
  # Extract the years and months from the combined raster time dimension
  years <- as.numeric(format(time(swe_raster), "%Y"))
  months <- as.numeric(format(time(swe_raster), "%m"))
  
  # Identify the unique years within the raster data
  unique_years <- unique(years)
  
  # Create a mask to identify raster layers that fall within a water year
  # A water year is defined from October of the previous year to September of the current year
  water_year_mask <- (years == unique_years[1] & months >= 10) |
    (years == unique_years[2] & months <= 9)
  
  # Subset the raster stack to only include layers that fall within the defined water year
  swe_snow_year_subset <- swe_raster[[which(water_year_mask)]]
  
  # Calculate and store the maximum SWE value from the subsetted raster layers
  # Presumably, max_swe_raster is initialized earlier in the script
  swe_max <- max(swe_raster, na.rm = TRUE)
  time(swe_max) <- unique_years[2]
  max_swe_raster_r85[[j]] <- swe_max
}



# combine max SWE maps into a single multilayer file
max_swe_vic_rcp85_CCSM4 <- terra::rast(max_swe_raster_r85)


# save data
terra::writeRaster(max_swe_vic_rcp85_CCSM4,
                   filename = "E:/data-raw/NCAR/CCSM4/future/max_swe_vic_rcp85_CCSM4.tif",
                   overwrite = TRUE
)

