library(fitdistrplus)

norm_data = rnorm(n=100, mean=4, sd=2)

# Exponentiate to get log-normal data
log_norm_data = exp(norm_data)

fit_normal(norm_data)
fit =fit_log_normal(log_norm_data)
fit_gev(log_norm_data)

# ============================================================================#
# create raster data
# ============================================================================#
# Define the dimensions and number of layers for the raster
nrows <- 10    # number of rows
ncols <- 10    # number of columns
nlayers <- 50  # number of layers (e.g., time points)

# Create an empty raster with the specified dimensions
raster_data <- rast(nrows=nrows, ncols=ncols, nlyr=nlayers,
                    xmin=0, xmax=10, 
                    ymin=0, ymax=10)

# Populate each layer with random data
# For example, using a normal distribution with mean=4 and sd=2
for (i in 1:nlayers) {
  # Generate random data for each cell
  layer_data <- rnorm(nrows*ncols, mean=4, sd=2)
  # Assign the data to the i-th layer of the raster
  raster_data[[i]] <- rast(matrix(layer_data, nrow=nrows, ncol=ncols))
}


# ============================================================================#
# app function to raster
# ============================================================================#
# Apply the function
distribution_params_raster_normal <- terra::app(raster_data, fit_normal)
distribution_params_raster_lnorm <- terra::app(raster_data, fit_log_normal)
distribution_params_raster_gev <- terra::app(raster_data, fit_gev)


names(distribution_params_raster) = c("mean", "sd")

# The result is a raster where each cell contains the mean and sd of the fitted normal distribution
