zero_position_before_max <- function(cell_stack) {
  # Convert stack to vector
  cell_vector <- as.vector(cell_stack)

  # Find max value position
  highest_value_position <- which.max(cell_vector)


  # Check for no valid max position
  if (length(highest_value_position) == 0 || is.na(highest_value_position)) {
    return(NA_real_)
  }


  # Find zero positions before max value
  zeros_before_highest <- which(cell_vector[1:highest_value_position] == 0)

  # Return last zero position before max, or 1 if none
  if (length(zeros_before_highest) == 0) {
    return(NA_real_)
  } else {
    return(max(zeros_before_highest))
  }
}
