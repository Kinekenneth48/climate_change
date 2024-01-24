################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(stars)
library(raster)
library(terra)
library(tictoc)

# define the extent of usa
usa_extent <- terra::ext(235, 300, 24, 50)
# usa_extent <- terra::ext(-125,  -65,  25,  50)


################################################################################
## STEP 1: Create single raster files for SPEAR
################################################################################
# ============================================================================#
#  monthly historical
# ============================================================================#

# Pre-allocate the list
raster_list <- vector("list", 30)

# Initialize an index for raster_list
index <- 0

# Loop through 30 file paths to read each SpatRaster file
for (ensemble in 1:30) {
  file_path <- sprintf(
    "data-raw/spear/pr/historical/month/pr_Amon_GFDL-SPEAR-MED_historical_r%di1p1f1_gr3_192101-201412.nc",
    ensemble
  )
  index <- index + 1
  raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
}


# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(x = raster_list)


# Compute the mean raster layer across all SpatRasters in the dataset
spear_hist_r_comb_pr <- terra::app(x = raster_data, fun = mean)


# Save the computed mean raster layer to disk
terra::writeRaster(spear_hist_r_comb_pr,
  filename = "data-raw/raster/spear_hist_r_comb_pr.tif",
  overwrite = TRUE
)

remove(raster_list, raster_data, spear_hist_r_comb_pr)



s # ============================================================================#
#  monthly future
# ============================================================================#
# Pre-allocate the list
raster_list <- vector("list", 30)

# Initialize an index for raster_list
index <- 0

# Loop through 30 files to read and process each SpatRaster
for (ensemble in 1:30) {
  file_path <- sprintf(
    "data-raw/spear/pr/future/month/pr_Amon_GFDL-SPEAR-MED_scenarioSSP5-85_r%di1p1f1_gr3_201501-210012.nc",
    ensemble
  )
  index <- index + 1
  raster_list[[index]] <- terra::crop(rast(file_path), usa_extent)
}



# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(x = raster_list)

# Compute the mean raster layer across all SpatRasters in the dataset
spear_future_r_comb_pr <- terra::app(x = raster_data, fun = mean)


# Save the computed mean raster layer to disk
terra::writeRaster(spear_future_r_comb_pr,
  filename = "data-raw/raster/spear_future_r_comb_pr.tif",
  overwrite = TRUE
)

remove(raster_list, raster_data, spear_future_r_comb_pr)









################################################################################
## STEP 2: Create single raster files for LOCA
#################################################################################
# ============================================================================#
#  monthly historical
# ============================================================================#

# Pre-allocate the list
raster_list <- vector("list", 3)

# Initialize an index for raster_list
index <- 0


# Loop through each type of file index
for (ensemble in 1:3) {
  # Generate the file path based on the current temperature type and index
  file_path <- sprintf(
    "data-raw/loca/ACCESS-CM2/pr/historical/month/pr.ACCESS-CM2.historical.r%di1p1f1.1950-2014.LOCA_16thdeg_v20220519.monthly.nc",
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
loca_hist_r_comb_pr <- terra::app(x = raster_data, fun = mean)
tictoc::toc()



# Save the computed mean raster layer to disk
terra::writeRaster(loca_hist_r_comb_pr,
  filename = "data-raw/raster/loca_hist_r_comb_pr.tif",
  overwrite = TRUE
)

remove(raster_list, raster_data, loca_hist_r_comb_pr)



# ============================================================================#
#  monthly future - 2015-2044
# ============================================================================#


# Pre-allocate the list
raster_list <- vector("list", 9)

# Initialize an index for raster_list
index <- 0


# Loop through each type of file index
for (model in c(245, 370, 585)) {
  for (ensemble in 1:3) {
    # Generate the file path based on the current temperature type and index
    file_path <- sprintf(
      "data-raw/loca/ACCESS-CM2/pr/future/month/pr.ACCESS-CM2.ssp%d.r%di1p1f1.2015-2044.LOCA_16thdeg_v20220519.monthly.nc",
      model, ensemble
    )

    index <- index + 1

    # Read, crop and append the raster in one go
    raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
  }
}


gc()

# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(raster_list)

tictoc::tic()
# Compute the mean raster layer across all SpatRasters in the dataset
loca_future_r_comb_pr_2044 <- terra::app(raster_data, mean)
tictoc::toc()



# Save the computed mean raster layer to disk
terra::writeRaster(loca_future_r_comb_pr_2044,
  filename = "data-raw/raster/loca_future_r_comb_pr_2044.tif",
  overwrite = TRUE
)

remove(raster_list, raster_data, loca_future_r_comb_pr_2044)



# ============================================================================#
#  monthly future - 2045-2074
# ============================================================================#


# Pre-allocate the list
raster_list <- vector("list", 9)

# Initialize an index for raster_list
index <- 0

# Loop through each type of  file index
for (model in c(245, 370, 585)) {
  for (ensemble in 1:3) {
    # Generate the file path based on the current temperature type and index
    file_path <- sprintf(
      "data-raw/loca/ACCESS-CM2/pr/future/month/pr.ACCESS-CM2.ssp%d.r%di1p1f1.2045-2074.LOCA_16thdeg_v20220519.monthly.nc",
      model, ensemble
    )

    index <- index + 1

    # Read, crop and append the raster in one go
    raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
  }
}



# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(raster_list)

gc()
gc()

tictoc::tic()
# Compute the mean raster layer across all SpatRasters in the dataset
loca_future_r_comb_pr_2074 <- terra::app(raster_data, mean)
tictoc::toc()



# Save the computed mean raster layer to disk
terra::writeRaster(loca_future_r_comb_pr_2074,
  filename = "data-raw/raster/loca_future_r_comb_pr_2074.tif",
  overwrite = TRUE
)

remove(raster_list, raster_data, loca_future_r_comb_pr_2074)




# ============================================================================#
#  monthly future - 2075-2100
# ============================================================================#

# Pre-allocate the list
raster_list <- vector("list", 9)

# Initialize an index for raster_list
index <- 0


# Loop through each type of temperature and file index
for (model in c(245, 370, 585)) {
  for (ensemble in 1:3) {
    # Generate the file path based on the current temperature type and index
    file_path <- sprintf(
      "data-raw/loca/ACCESS-CM2/pr/future/month/pr.ACCESS-CM2.ssp%d.r%di1p1f1.2075-2100.LOCA_16thdeg_v20220519.monthly.nc",
      model, ensemble
    )

    index <- index + 1

    # Read, crop and append the raster in one go
    raster_list[[index]] <- terra::crop(terra::rast(file_path), usa_extent)
  }
}


gc()

# Combine all individual SpatRasters into a single SpatRasterDataset
raster_data <- terra::sds(x = raster_list)

tictoc::tic()
# Compute the mean raster layer across all SpatRasters in the dataset
loca_future_r_comb_pr_2100 <- terra::app(x = raster_data, fun = mean)
tictoc::toc()



# Save the computed mean raster layer to disk
terra::writeRaster(loca_future_r_comb_pr_2100,
  filename = "data-raw/raster/loca_future_r_comb_pr_2100.tif",
  overwrite = TRUE
)

remove(raster_list, raster_data, loca_future_r_comb_pr_2100)
