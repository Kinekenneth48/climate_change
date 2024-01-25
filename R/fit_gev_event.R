fit_gev_event <- function(x) {
  # Remove NA and non-positive values
  x <- x[x > 0]
  x <- na.omit(x)
  
  # Check if the length of the dataset is less than 30
  if (length(x) < 30) {
    return(NA)
  }
  
  # Fit a GEV distribution
  fit <- tryCatch(
    {
      extRemes::fevd(x, type = "GEV", method = c("MLE"))
    },
    error = function(e) NULL
  )
  
  # Extract and return the location, scale, and shape parameters if fitting is successful
  if (!is.null(fit) & fit[["results"]][["convergence"]] == 0) {
    event <- extRemes::return.level(fit, return.period = c(50))[["50"]]
    return(event)
  } else {
    # Return NA values if fitting fails
    return(NA)
  }
}
