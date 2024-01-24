library(terra)



# Define the extent, rows, and columns for the raster
ext <- ext(0, 10, 0, 10)
rows <- 10
cols <- 10

# Create some example daily snow and temperature raster layers
set.seed(123)
snow_rasters <- lapply(1:365, function(i) {
  rast(
    extent = ext,
    nrow = rows, ncol = cols,
    vals = runif(rows * cols, 0, 100)
  )
})
temp_rasters <- lapply(1:365, function(i) {
  rast(
    extent = ext,
    nrow = rows, ncol = cols,
    vals = runif(rows * cols, -30, 30)
  )
})

# Convert the lists of raster layers to RasterStack objects
snow_rasters <- rast(snow_rasters)
temp_rasters <- rast(temp_rasters)




# Initialize the maximum snow and corresponding temperature rasters
max_snow <- snow_rasters[[1]]
max_temp <- temp_rasters[[1]]

# Loop through each layer and update the max snow and corresponding temperature layers
nlayers <- nlyr(snow_rasters)
for (i in 2:nlayers) {
  # Get the current snow and temperature layers
  current_snow <- snow_rasters[[i]]
  current_temp <- temp_rasters[[i]]
  
  # Update the max snow and corresponding temperature layers
  condition <- current_snow > max_snow
  max_snow <- max(max_snow, current_snow)
  max_temp <- ifelse(condition, current_temp, max_temp)
}

# Save the resulting raster layers
writeRaster(max_snow, "path/to/output/max_snow.tif", overwrite = TRUE)
writeRaster(max_temp, "path/to/output/max_temp.tif", overwrite = TRUE)



