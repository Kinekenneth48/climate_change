fit_gev_shape <- function(x) {
  # Remove NA and non-positive values
  x <- x[x > 1]
  x <- na.omit(x)
  
  # Check if the length of the dataset is less than 30
  if (length(x) < 20) {
    return(as.vector(c(shape = NA)))
  }
  
  # Fit a GEV distribution
  fit <- tryCatch(
    {
      extRemes::fevd(x, type = "GEV", method = c("Lmoments"))
    },
    error = function(e) NULL
  )
  
  # Extract and return the location, scale, and shape parameters if fitting is successful
  if (!is.null(fit)) {
    results <- c(shape = fit[["results"]][["shape"]]
    )
    return(as.vector(results))
  } else {
    # Return NA values if fitting fails
    return(as.vector(c(shape = NA)))
  }
}
