mk <- function(x) {
  
  # Remove NA and non-positive values
  x <- x[x > 1]
  x <- na.omit(x)
  
  # Check if the length of the dataset is less than 30
  if (length(x) < 30) {
    return(NA)
  }
  
  
  test <- mk.test(x)
  p_value <- test[["p.value"]]

  if (p_value > 0.05) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
