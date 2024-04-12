# Define a function to fit a GEV model with potentially time-varying parameters
gev_fit_para_mix_stat <- function(x) {
  #percentage of zeros
  perzero <- sum(x == 0)/length(x)
  
  # Preprocess the data: remove NA and non-positive values
  x <- na.omit(x[x > 1])
  


  # Check if the length of the dataset is less than 30
  if (length(x) < 30) {
    return(as.numeric(c(loc = NA, loctrend = NA, scale = NA, shape = NA,
                        perzero = 0)))
  }

  # Perform Mann-Kendall trend test to check for data trend
  test <- trend::mk.test(x)
  p_value <- test[["p.value"]]

  # Fit GEV model based on the presence of a trend
  if (p_value > 0.05) {
    # Data is considered stationary: fit a stationary GEV model
    stat_model <- extRemes::fevd(x, type = c("GEV"), method = c("Lmoments"))
  } else {
    # Data exhibits a trend: attempt non-stationary GEV model fitting
    # First, estimate the shape parameter from a stationary model
    shape_para <- extRemes::fevd(x,
      type = c("GEV"),
      method = c("Lmoments")
    )[["results"]][["shape"]]

    # Attempt to fit a non-stationary model with fixed shape parameter
    stat_model <- tryCatch(
      {
        evd::fgev(x, nsloc = seq_along(x), shape = shape_para)
      },
      error = function(e) NULL
    )
  }

  if (is.null(stat_model)) {
    return(as.numeric(c(loc = NA, loctrend = NA, scale = NA, shape = NA,
                        perzero = 0)))
  } else if (inherits(stat_model, "fevd")) {
    names(stat_model[["results"]]) = c("loc" , "scale" , "shape")
    return(as.numeric(c(stat_model[["results"]][["loc"]] ,loctrend = 0,
                        stat_model[["results"]][["scale"]], 
                        stat_model[["results"]][["shape"]],
                         perzero)))
  } else if (inherits(stat_model, "evd")) {
    return(as.numeric(c(stat_model[["estimate"]], shape = shape_para, perzero)))
  } 
    
}  
