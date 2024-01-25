fit_log_normal_event <- function(x) {
  # Remove NA and non-positive values
  x <- x[x > 0]
  x <- na.omit(x)

  # Check if the length of the dataset is less than 30
  if (length(x) < 30) {
    return(NA)
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
    event <- quantile(fit, probs = c(0.98))[["quantiles"]][["p=0.98"]]
    return(event)
  } else {
    # Return NA values if fitting fails
    return(NA)
  }
}
