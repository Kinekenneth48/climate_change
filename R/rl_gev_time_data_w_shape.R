# Define a function to fit a GEV model with potentially time-varying parameters
rl_gev_time_data_w_shape <- function(x) {
  
  #get length of original data
  n <- length(x)-1
  
#get shape parameter attached to data
  shape_para = x[length(x)]
  
  #remove shape parameter from data
 x = x[-length(x)]
  

  # Remove NA and non-positive values
  x <- x[x > 1]
  x <- na.omit(x)
  
  # Check if the length of the dataset is less than 30
  if (length(x) < 30) {
    return(rep(NA, n))
  }
  
  #get length of  data after removing NA and non-positive values
  n_new <- length(x)
  
                          
  # shape is fixed
  stat_model <- tryCatch(
    {
      evd::fgev(x, nsloc = 1:n_new, shape = shape_para)
    },
    error = function(e) NULL
  )
  
  if (is.null(stat_model)) {
    return(rep(NA, n))
  }
  
  
  t_values <- 1:n_new
  mu <- stat_model[["estimate"]][["loc"]] + stat_model[["estimate"]][["loctrend"]] * t_values
  sigma <- rep(stat_model[["estimate"]][["scale"]], n_new)
  xi <- rep(shape_para, n_new)
  
  
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
  
}
