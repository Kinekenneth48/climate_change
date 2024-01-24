###############################################################################
### STEP 0 : INITIAL SET UP
#################################################################################

# ============================================================================#
# load the required R packages
# ============================================================================#
library(terra)

terraOptions(memfrac = 0.80, verbose = TRUE)

# define the extent of usa
usa_extent <- terra::ext(235, 300, 24, 50)



################################################################################
## STEP 1: Create single raster files for spear
#################################################################################
# ============================================================================#
# 1  historical - 19210101-19301231
# ============================================================================#

# Pre-allocate the list
raster_list <- vector("list", 30)

# Initialize an index for raster_list
index <- 0

# Loop through each type of file index
for (ensemble in 1:30) {
  # Generate the file path based on the current temperature type and index
  file_path <- sprintf(
    "D:/data-raw/spear/tas/historical/day/tas_day_GFDL-SPEAR-MED_historical_r%di1p1f1_gr3_19210101-19301231.nc",
    ensemble
  )
  index <- index + 1
  # Read, crop and append the raster in one go
  raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
}


# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(x = raster_list)

tictoc::tic()
# Compute the mean raster layer across all SpatRasters in the dataset
spear_hist_day_tas_19210101_19301231 <- terra::app(x = raster_data, fun = mean)
tictoc::toc()



# Save the computed mean raster layer to disk
terra::writeRaster(spear_hist_day_tas_19210101_19301231,
                   filename = "D:/data-raw/spear/raster/spear_hist_day_tas_19210101_19301231.tif",
                   overwrite = TRUE
)

remove(raster_list, raster_data, spear_hist_day_tas_19210101_19301231)


# ============================================================================#
#  2 historical - 19310101-19401231
# ============================================================================#

# Pre-allocate the list
raster_list <- vector("list", 30)

# Initialize an index for raster_list
index <- 0

# Loop through each type of file index
for (ensemble in 1:30) {
  # Generate the file path based on the current temperature type and index
  file_path <- sprintf(
    "D:/data-raw/spear/tas/historical/day/tas_day_GFDL-SPEAR-MED_historical_r%di1p1f1_gr3_19310101-19401231.nc",
    ensemble
  )
  index <- index + 1
  # Read, crop and append the raster in one go
  raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
}


# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(x = raster_list)

remove(raster_list)

tictoc::tic()
# Compute the mean raster layer across all SpatRasters in the dataset
spear_hist_day_tas_19310101_19401231 <- terra::app(x = raster_data, fun = mean)
tictoc::toc()



# Save the computed mean raster layer to disk
terra::writeRaster(spear_hist_day_tas_19310101_19401231,
                   filename = "D:/data-raw/spear/raster/spear_hist_day_tas_19310101_19401231.tif",
                   overwrite = TRUE
)

remove(raster_list, raster_data, spear_hist_day_tas_19310101_19401231)



# ============================================================================#
# 3 historical - 19410101-19501231
# ============================================================================#

# Pre-allocate the list
raster_list <- vector("list", 30)

# Initialize an index for raster_list
index <- 0

# Loop through each type of file index
for (ensemble in 1:30) {
  # Generate the file path based on the current temperature type and index
  file_path <- sprintf(
    "D:/data-raw/spear/tas/historical/day/tas_day_GFDL-SPEAR-MED_historical_r%di1p1f1_gr3_19410101-19501231.nc",
    ensemble
  )
  index <- index + 1
  # Read, crop and append the raster in one go
  raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
}


# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(x = raster_list)

remove(raster_list)

tictoc::tic()
# Compute the mean raster layer across all SpatRasters in the dataset
spear_hist_day_tas_19410101_19501231 <- terra::app(x = raster_data, fun = mean)
tictoc::toc()



# Save the computed mean raster layer to disk
terra::writeRaster(spear_hist_day_tas_19410101_19501231,
                   filename = "D:/data-raw/spear/raster/spear_hist_day_tas_19410101_19501231.tif",
                   overwrite = TRUE
)

remove( raster_data, spear_hist_day_tas_19410101_19501231)




# ============================================================================#
#  4 historical - 19510101-19601231
# ============================================================================#

# Pre-allocate the list
raster_list <- vector("list", 30)

# Initialize an index for raster_list
index <- 0

# Loop through each type of file index
for (ensemble in 1:30) {
  # Generate the file path based on the current temperature type and index
  file_path <- sprintf(
    "D:/data-raw/spear/tas/historical/day/tas_day_GFDL-SPEAR-MED_historical_r%di1p1f1_gr3_19510101-19601231.nc",
    ensemble
  )
  index <- index + 1
  # Read, crop and append the raster in one go
  raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
}


# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(x = raster_list)

remove(raster_list)

tictoc::tic()
# Compute the mean raster layer across all SpatRasters in the dataset
spear_hist_day_tas_19510101_19601231 <- terra::app(x = raster_data, fun = mean)
tictoc::toc()



# Save the computed mean raster layer to disk
terra::writeRaster(spear_hist_day_tas_19510101_19601231,
                   filename = "D:/data-raw/spear/raster/spear_hist_day_tas_19510101_19601231.tif",
                   overwrite = TRUE
)

remove( raster_data, spear_hist_day_tas_19510101_19601231)




# ============================================================================#
# 5  historical - 19610101-19701231
# ============================================================================#

# Pre-allocate the list
raster_list <- vector("list", 30)

# Initialize an index for raster_list
index <- 0

# Loop through each type of file index
for (ensemble in 1:30) {
  # Generate the file path based on the current temperature type and index
  file_path <- sprintf(
    "D:/data-raw/spear/tas/historical/day/tas_day_GFDL-SPEAR-MED_historical_r%di1p1f1_gr3_19610101-19701231.nc",
    ensemble
  )
  index <- index + 1
  # Read, crop and append the raster in one go
  raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
}


# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(x = raster_list)

remove(raster_list)

tictoc::tic()
# Compute the mean raster layer across all SpatRasters in the dataset
spear_hist_day_tas_19610101_19701231 <- terra::app(x = raster_data, fun = mean)
tictoc::toc()



# Save the computed mean raster layer to disk
terra::writeRaster(spear_hist_day_tas_19610101_19701231,
                   filename = "D:/data-raw/spear/raster/spear_hist_day_tas_19610101_19701231.tif",
                   overwrite = TRUE
)

remove( raster_data, spear_hist_day_tas_19610101_19701231)


# ============================================================================#
#  6 historical - 19710101-19801231
# ============================================================================#

# Pre-allocate the list
raster_list <- vector("list", 30)

# Initialize an index for raster_list
index <- 0

# Loop through each type of file index
for (ensemble in 1:30) {
  # Generate the file path based on the current temperature type and index
  file_path <- sprintf(
    "D:/data-raw/spear/tas/historical/day/tas_day_GFDL-SPEAR-MED_historical_r%di1p1f1_gr3_19710101-19801231.nc",
    ensemble
  )
  index <- index + 1
  # Read, crop and append the raster in one go
  raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
}


# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(x = raster_list)

remove(raster_list)


# Compute the mean raster layer across all SpatRasters in the dataset
spear_hist_day_tas_19710101_19801231 <- terra::app(x = raster_data, fun = mean)




# Save the computed mean raster layer to disk
terra::writeRaster(spear_hist_day_tas_19710101_19801231,
                   filename = "D:/data-raw/spear/raster/spear_hist_day_tas_19710101_19801231.tif",
                   overwrite = TRUE
)

remove( raster_data, spear_hist_day_tas_19710101_19801231)



# ============================================================================#
#  7 historical - 19810101-19901231
# ============================================================================#

# Pre-allocate the list
raster_list <- vector("list", 30)

# Initialize an index for raster_list
index <- 0

# Loop through each type of file index
for (ensemble in 1:30) {
  # Generate the file path based on the current temperature type and index
  file_path <- sprintf(
    "D:/data-raw/spear/tas/historical/day/tas_day_GFDL-SPEAR-MED_historical_r%di1p1f1_gr3_19810101-19901231.nc",
    ensemble
  )
  index <- index + 1
  # Read, crop and append the raster in one go
  raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
}


# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(x = raster_list)

remove(raster_list)


tictoc::tic()
# Compute the mean raster layer across all SpatRasters in the dataset
spear_hist_day_tas_19810101_19901231 <- terra::app(x = raster_data, fun = mean)
tictoc::toc()



# Save the computed mean raster layer to disk
terra::writeRaster(spear_hist_day_tas_19810101_19901231,
                   filename = "D:/data-raw/spear/raster/spear_hist_day_tas_19810101_19901231.tif",
                   overwrite = TRUE
)

remove( raster_data, spear_hist_day_tas_19810101_19901231)





# ============================================================================#
# 8  historical - 19910101-20001231
# ============================================================================#

# Pre-allocate the list
raster_list <- vector("list", 30)

# Initialize an index for raster_list
index <- 0

# Loop through each type of file index
for (ensemble in 1:30) {
  # Generate the file path based on the current temperature type and index
  file_path <- sprintf(
    "D:/data-raw/spear/tas/historical/day/tas_day_GFDL-SPEAR-MED_historical_r%di1p1f1_gr3_19910101-20001231.nc",
    ensemble
  )
  index <- index + 1
  # Read, crop and append the raster in one go
  raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
}


# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(x = raster_list)

remove(raster_list)

tictoc::tic()
# Compute the mean raster layer across all SpatRasters in the dataset
spear_hist_day_tas_19910101_20001231 <- terra::app(x = raster_data, fun = mean)
tictoc::toc()



# Save the computed mean raster layer to disk
terra::writeRaster(spear_hist_day_tas_19910101_20001231,
                   filename = "D:/data-raw/spear/raster/spear_hist_day_tas_19910101_20001231.tif",
                   overwrite = TRUE
)

remove( raster_data, spear_hist_day_tas_19910101_20001231)





# ============================================================================#
#  9 historical - 20010101-20101231
# ============================================================================#

# Pre-allocate the list
raster_list <- vector("list", 30)

# Initialize an index for raster_list
index <- 0

# Loop through each type of file index
for (ensemble in 1:30) {
  # Generate the file path based on the current temperature type and index
  file_path <- sprintf(
    "D:/data-raw/spear/tas/historical/day/tas_day_GFDL-SPEAR-MED_historical_r%di1p1f1_gr3_20010101-20101231.nc",
    ensemble
  )
  index <- index + 1
  # Read, crop and append the raster in one go
  raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
}


# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(x = raster_list)

remove(raster_list)

tictoc::tic()
# Compute the mean raster layer across all SpatRasters in the dataset
spear_hist_day_tas_20010101_20101231 <- terra::app(x = raster_data, fun = mean)
tictoc::toc()



# Save the computed mean raster layer to disk
terra::writeRaster(spear_hist_day_tas_20010101_20101231,
                   filename = "D:/data-raw/spear/raster/spear_hist_day_tas_20010101_20101231.tif",
                   overwrite = TRUE
)

remove( raster_data, spear_hist_day_tas_20010101_20101231)




# ============================================================================#
# 10  historical - 20110101-20141231
# ============================================================================#

# Pre-allocate the list
raster_list <- vector("list", 30)

# Initialize an index for raster_list
index <- 0

# Loop through each type of file index
for (ensemble in 1:30) {
  # Generate the file path based on the current temperature type and index
  file_path <- sprintf(
    "D:/data-raw/spear/tas/historical/day/tas_day_GFDL-SPEAR-MED_historical_r%di1p1f1_gr3_20110101-20141231.nc",
    ensemble
  )
  index <- index + 1
  # Read, crop and append the raster in one go
  raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
}


# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(x = raster_list)

remove(raster_list)

tictoc::tic()
# Compute the mean raster layer across all SpatRasters in the dataset
spear_hist_day_tas_20110101_20141231 <- terra::app(x = raster_data, fun = mean)
tictoc::toc()



# Save the computed mean raster layer to disk
terra::writeRaster(spear_hist_day_tas_20110101_20141231,
                   filename = "D:/data-raw/spear/raster/spear_hist_day_tas_20110101_20141231.tif",
                   overwrite = TRUE
)

remove( raster_data, spear_hist_day_tas_20110101_20141231)


# ============================================================================#
# combine into a single mutli-layer raster
# ============================================================================#
r1 = rast("D:/data-raw/spear/raster/spear_hist_day_tas_20110101_20141231.tif")
r2 = rast("D:/data-raw/spear/raster/spear_hist_day_tas_20010101_20101231.tif")
r3 = rast("D:/data-raw/spear/raster/spear_hist_day_tas_19910101_20001231.tif")
r4 = rast("D:/data-raw/spear/raster/spear_hist_day_tas_19810101_19901231.tif")
r5 = rast("D:/data-raw/spear/raster/spear_hist_day_tas_19710101_19801231.tif")
r6 = rast("D:/data-raw/spear/raster/spear_hist_day_tas_19610101_19701231.tif")
r7 = rast("D:/data-raw/spear/raster/spear_hist_day_tas_19510101_19601231.tif")
r8 = rast("D:/data-raw/spear/raster/spear_hist_day_tas_19410101_19501231.tif")
r9 = rast("D:/data-raw/spear/raster/spear_hist_day_tas_19310101_19401231.tif")
r10 = rast("D:/data-raw/spear/raster/spear_hist_day_tas_19210101_19301231.tif")

spear_hist_day_tas_19210101_20141231 = c(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10)


terra::writeRaster(spear_hist_day_tas_19210101_20141231,
                   filename = "D:/data-raw/spear/raster/spear_hist_day_tas_19210101_20141231.tif",
                   overwrite = TRUE
)
