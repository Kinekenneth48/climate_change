cal_max_cons_days_above_thrd <- function(x, threshold_temp){
  # Convert to binary: 1 for days above the threshold, 0 for below.
  binary_temp <- (x >= threshold_temp) * 1
  
  # Apply run length encoding to find consecutive days.
  encoded_temp <- rle(binary_temp)
  
  # Extract the lengths of streaks where the value is 1 (above the threshold).
  streak_lengths <- encoded_temp$lengths[encoded_temp$values == 1]
  
  # Check if there are any streaks, if not return 0, otherwise return the max streak length.
  if(length(streak_lengths) == 0){
    return(0)
  } else {
    return(max(streak_lengths))
  }
}
