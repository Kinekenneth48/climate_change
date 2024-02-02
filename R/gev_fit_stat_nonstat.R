# Define a function to fit a GEV model with potentially time-varying parameters
gev_fit_stat_nonstat <- function(x) {
  n <- length(x)

  # Remove NA and non-positive values
  x <- x[x > 1]
  x <- na.omit(x)

  # Check if the length of the dataset is less than 30
  if (length(x) < 30) {
    return(rep(NA, n))
  }

  n_new <- length(x)

  test <- mk.test(x)
  p_value <- test[["p.value"]]

  if (p_value > 0.05) {
    # stationary
    stat_model <- extRemes::fevd(x, type = c("GEV"), method = c("Lmoments"))
    rl <- as.vector(return.level(stat_model, return.period = c(50)))
    return(rep(rl, n))
  } else {
    shape_para <- extRemes::fevd(x, type = c("GEV"), method = c("Lmoments"))[["results"]][["shape"]]
    # shape is fixed
    stat_model <- tryCatch(
      {
        evd::fgev(x, nsloc = 1:n_new, shape = shape_para)
      },
      error = function(e) NULL
    )

    if (is.null(fit)) {
      return(rep(NA, n))
    }

  }

  t_values <- 1:n_new
  mu <- stat_model[["estimate"]][["loc"]] + stat_model[["estimate"]][["loctrend"]] * t_values
  sigma <- rep(stat_model[["estimate"]][["scale"]], n_new)
  xi <- rep(stat_model[["fixed"]][["shape"]], n_new)


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

  return(rl)

  # if ((stat_model[["convergence"]] == "successful")) {
  #   return(rl)
  # } else {
  #   return(rep(NA, n))
  # }
}
