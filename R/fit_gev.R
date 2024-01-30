fit_gev <- function(x) {
  # Remove NA and non-positive values
  x <- x[x > 0]
  x <- na.omit(x)

  # Check if the length of the dataset is less than 30
  if (length(x) < 30) {
    return(as.vector(c(location = NA, scale = NA, shape = NA)))
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
    results <- c(
      location = fit[["results"]][["location"]],
      scale = fit[["results"]][["scale"]],
      shape = fit[["results"]][["shape"]]
    )
    return(as.vector(results))
  } else {
    # Return NA values if fitting fails
    return(as.vector(c(location = NA, scale = NA, shape = NA)))
  }
}
