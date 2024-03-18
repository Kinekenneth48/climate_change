# fit_gev_shape <- function(x) {
#   # Remove NA and non-positive values
#   x <- x[x > 1]
#   x <- na.omit(x)
#   
#   # Check if the length of the dataset is less than 30
#   if (length(x) < 20) {
#     return(as.vector(c(shape = NA)))
#   }
#   
#   # Fit a GEV distribution
#   fit <- tryCatch(
#     {
#       extRemes::fevd(x, type = "GEV", method = c("Lmoments"))
#     },
#     error = function(e) NULL
#   )
#   
#   # Extract and return the location, scale, and shape parameters if fitting is successful
#   if (!is.null(fit)) {
#     results <- c(shape = fit[["results"]][["shape"]]
#     )
#     return(as.vector(results))
#   } else {
#     # Return NA values if fitting fails
#     return(as.vector(c(shape = NA)))
#   }
# }


fit_gev_shape <- function(x, enforce_constraint = FALSE) {
  # Remove NA and non-positive values
  x <- x[x > 1]
  x <- na.omit(x)
  
  # Check if the length of the dataset is less than 30
  if (length(x) < 30) {
    return(as.vector(c(shape = NA)))
  }
  
  # Fit a GEV distribution
  fit <- tryCatch(
    {
      extRemes::fevd(x, type = "GEV", method = c("Lmoments"))
    },
    error = function(e) NULL
  )
  
  # Extract and return the shape parameters if fitting is successful
  if (!is.null(fit)) {
    # Extract shape parameter
    gev_shape <- as.vector(fit[["results"]][["shape"]])
    
    # Enforce constraint on the shape parameter if specified by the user
    if (enforce_constraint) {
      gev_shape <- pmax(0, pmin(0.25, gev_shape))
    }
    
    results <- c(shape = gev_shape)
    return(as.vector(results))
  } else {
    # Return NA values if fitting fails
    return(as.vector(c(shape = NA)))
  }
}
