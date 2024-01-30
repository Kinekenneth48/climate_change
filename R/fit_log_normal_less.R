fit_log_normal_less <- function(x) {
  # Remove NA and non-positive values
  x <- x[x > 0]
  x <- na.omit(x)
  
  # Check if the length of the dataset is less than 30
  if (length(x) < 20) {
    return(as.vector(c(meanlog = NA, sdlog = NA)))
  }
  
  # Fit a log-normal distribution
  fit <- tryCatch(
    {
      fitdistrplus::fitdist(
        data = x, distr = "lnorm",
        start = list(
          meanlog = mean(log(x)), sdlog = stats::sd(log(x))
        ),
        method = "mle"
      )
    },
    error = function(e) NULL
  )
  
  # Extract and return the meanlog and sdlog if fitting is successful
  if (!is.null(fit) & fit[["convergence"]] == 0) {
    results <- c(meanlog = fit$estimate["meanlog"], sdlog = fit$estimate["sdlog"])
    return(as.vector(results))
  } else {
    # Return NA values if fitting fails
    return(as.vector(c(meanlog = NA, sdlog = NA)))
  }
}
