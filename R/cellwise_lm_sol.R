
# cellwise_lm_sol <- function(x, variables) {
#   if (any(is.na(x))) {
#     return(rep(NA, (variables + 1))) 
#   }
#   
#   # Creating dynamic column names based on `param`
#   col_names <- c("y", paste0("x", 1:variables))
#   
#   # Reshaping data and fitting the model
#   d <- data.frame(matrix(x, ncol = (variables + 1), byrow = FALSE))
#   names(d) <- col_names
#   
#   # Constructing a formula dynamically
#   formula_str <- paste("y ~", paste(col_names[-1], collapse = " + "))
#   coefficients(lm(formula_str, data = d))
# }



cellwise_lm_sol <- function(x, variables, include_intercept = TRUE) {
  if (any(is.na(x))) {
    return(rep(NA, variables + ifelse(include_intercept, 1, 0))) 
  }
  
  # Determine the number of observations based on the number of variables
  num_obs <- length(x) / (variables + 1)
  
  # Creating dynamic column names based on `variables`
  col_names <- c("y", paste0("x", 1:variables))
  
  # Reshaping data and fitting the model
  d <- data.frame(matrix(x, ncol = (variables + 1), byrow = TRUE))
  names(d) <- col_names
  
  # Constructing a formula dynamically
  if (include_intercept) {
    formula_str <- paste("y ~", paste(col_names[-1], collapse = " + "))
  } else {
    formula_str <- paste("y ~", paste(col_names[-1], collapse = " + "), " - 1")
  }
  
  coef <- coefficients(lm(formula_str, data = d))
  
  # Replace NaN coefficients with 0
  coef[is.na(coef)] <- 0
  
  return(coef)
}
