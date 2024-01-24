library(terra)
# Set seed for reproducible random numbers
set.seed(42)

# Create the original raster 'y' with random values
y <- rast(ncol = 5, nrow = 5, nlyr = 100)
y <- setValues(y, runif(ncell(y) * nlyr(y)))

# Create 'x1' raster with random values
x1 <- rast(ncol = 5, nrow = 5, nlyr = 100)
x1 <- setValues(x1, runif(ncell(x1) * nlyr(x1)))

# Create 'x2' raster with random values
x2 <- rast(ncol = 5, nrow = 5, nlyr = 100)
x2 <- setValues(x2, runif(ncell(x2) * nlyr(x2)))

# Create 'x3' raster with random values
x3 <- rast(ncol = 5, nrow = 5, nlyr = 100)
x3 <- setValues(x3, runif(ncell(x3) * nlyr(x3)))
rr <- c(y, x1, x2, x3)


rr[1:5] <- NA

ff <- function(v, nlayers = 100, variables = 3) {
  X <- cbind(1, matrix(v[-c(1:nlayers)], ncol = variables))
  solve(t(X) %*% X) %*% t(X) %*% v[1:nlayers]
}

b <- app(rr, ff)



f <- function(x, variables = 3) {
  if (any(is.na(x))) {
    return(rep(NA, (variables + 1)))
  }

  # Creating dynamic column names based on `param`
  col_names <- c("y", paste0("x", 1:variables))

  # Reshaping data and fitting the model
  d <- data.frame(matrix(x, ncol = (variables + 1), byrow = FALSE))
  names(d) <- col_names

  # Constructing a formula dynamically
  formula_str <- paste("y ~", paste(col_names[-1], collapse = " + "))
  coefficients(lm(formula_str, data = d))
}



bb <- app(rr, f)

index <- seq(i, nlyr(predictor_rast), by = variables)


predictor_rast_vars <- predictor_rast[[index]]
raster=c(lm_model_layer, predictor_rast_vars)

predict_cellwise <- function(raster, variables= 7) {
  
  

    # Multiply each predictor by its corresponding coefficient and sum them
    prediction <- raster[[1]] # starting with the intercept
    for (i in 2:(variables + 1)) {
      pred_layer[i] <- prediction + raster[[i]] * predictor_rast_vars[[i + variables]]
  
  }
  return(pred_layer)
}
