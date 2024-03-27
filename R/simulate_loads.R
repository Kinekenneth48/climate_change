simulate_loads <- function(x, n = 100, t = 50, gr = NULL) {
  # Ensure helper functions are available to workers when cores is used
  source("R/gev_fit_para_mix_stat.R")
  source("R/get_rtl.R")
  source("R/model_gr.R")
  
  Rcpp::sourceCpp("src/simulate_snow.cpp")
  Rcpp::sourceCpp("src/gr_functions.cpp")
  
  
  loc <- x[1]
  loctrend <- x[2]
  scl <- x[3]
  shp <- x[4]
  pr_zero <- x[5]

  # Simulate the ground snow load assuming a proper location is provided
  if (is.finite(loc)) {
    ground_snow <- simulate_snow(loc, loctrend, scl, shp, pr_zero, n, t)

    # High pr_zero can induce nan values, set all to zero
    ground_snow[!is.finite(ground_snow)] <- 0
    ground_snow <- as.numeric(ground_snow)
  } else {
    return(as.numeric(rep(NA, 4)))
  }

  # convert values from mm to kpa units
  ground_snow <- ground_snow * 0.00980665

  # Add in ground to roof conversion if necessary
  if (!is.null(gr)) {
    ground_snow <- as.numeric(ground_snow * gr(ground_snow))
  }


  rtl_loads <- get_rtl(
    tL = ground_snow,
    index = c(2.5, 3.0, 3.25, 3.5),
    # Dead Load statistical Parameters (Normal)
    nDL = 15 * 0.04788, # psf to kPa
    biasDL = 1.05,
    covDL = 0.1,
    # Resistance Statistical Parameters (Normal)
    # Assuming Flexural Beam Bartlette Parameters
    biasR = 1.049,
    covR = 0.09,
    # Roof adjustment from the code.
    roof_adjust = 0.7,
    # Safety Factor
    gl_safety = 1.0,
    # Don't use the minimum load requirements.
    # (Assume all roofs subject to 0.7 multiplier.)
    minLoad = 0,
    # Steel phi Factor
    phi = 0.9,
    # Number of years represented in each simulation
    years = 50
  )

  return(rtl_loads)
}
