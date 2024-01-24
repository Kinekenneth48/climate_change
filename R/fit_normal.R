
fit_normal <- function(x) {
  # Remove NA values
  x <- na.omit(x)
  
  fit <- tryCatch({
    fitdist(x, "norm")
  }, error = function(e) NULL)
  
  if (!is.null(fit)) {
    results <- c(mean = fit$estimate["mean"], sd = fit$estimate["sd"])
    return(as.vector(results))
  } else {
    return(as.vector(c(mean = NA, sd = NA)))
  }
}