# Define a function to fit a GEV model with potentially time-varying parameters
gev_fit_time_varying <- function(x) {
  n <- length(x)

  # Remove NA and non-positive values
  x <- x[x > 0]
  x <- na.omit(x)

  # Check if the length of the dataset is less than 30
  if (length(x) < 30) {
    return(rep(NA, n))
  }

  n_new <- length(x)

  # Create a base matrix for ydat (time)
  ydat_base <- matrix(rep(1:n_new, each = 1), nrow = n_new, ncol = 1)


  # Fit three GEV models with varying complexity:
  # fit1: model with time-varying mu
  # fit2: model with time-varying mu and scale
  # fit3: model with time-varying mu, scale and shape
  fit1 <- ismev::gev.fit(xdat = x, ydat = ydat_base, mul = 1, show = FALSE,method = "Nelder-Mead")
  fit2 <- ismev::gev.fit(xdat = x, ydat = ydat_base, mul = 1, sigl = 1, show = FALSE,method = "Nelder-Mead")
  fit3 <- ismev::gev.fit(
    xdat = x, ydat = ydat_base, mul = 1, sigl = 1,
    shl = 1, show = FALSE,method = "Nelder-Mead"
  )


  # Calculate the deviance statistics fit2 and fit3 relative to fit1
  D2 <- 2 * (-fit2$nllh + fit1$nllh)
  D3 <- 2 * (-fit3$nllh + fit1$nllh)

  # Choose the best fitting model based on the deviance statistics(chi square at 5%)
  fit <- if (D2 > 3.841) fit2 else if (D3 > 3.841) fit3 else fit1

  # Initialize and calculate the location parameter (mu) for each time point
  t_values <- 1:n_new
  mu <- fit[["mle"]][1] + fit[["mle"]][2] * t_values

  # Initialize the scale (sigma) and shape (xi) parameters
  sigma <- rep(fit[["mle"]][3], n_new)
  xi <- rep(fit[["mle"]][4], n_new)

  # Update sigma and xi if the model includes time-varying scale and/or shape parameters
  if (is.numeric(fit[["model"]][[2]]) && is.null(fit[["model"]][[3]])) {
    sigma <- sigma + fit[["mle"]][4] * t_values
    xi <- rep(fit[["mle"]][5], n_new)
  } else if (is.numeric(fit[["model"]][[2]]) &&
    is.numeric(fit[["model"]][[3]])) {
    sigma <- sigma + fit[["mle"]][4] * t_values
    xi <- fit[["mle"]][5] + fit[["mle"]][6] * t_values
  }


  # Define a nested function to estimate quantiles based on the GEV parameters
  estimate_quantile <- function(mu, sigma, xi, p) {
    zp <- ifelse(xi != 0,
      mu - sigma / xi * (1 - (-log(1 - p))^(-xi)),
      mu - sigma * log(-log(1 - p))
    )
    return(zp)
  }

  # Calculate return levels based on the estimated GEV parameters
  rl <- estimate_quantile(mu, sigma, xi, p = 0.02)

  if (length(rl) != n) {
    rl <- c(rl, rep(NA, (n - length(rl))))
  }


  if (fit[["conv"]] == 0) {
    return(rl)
  } else {
    return(rep(NA, n))
  }
}
