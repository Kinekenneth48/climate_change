find_top_storm_sums_w_index <- function(data, num_zeros) {
  # Initialize vectors to store storm information
  storm_sums <- numeric()
  start_indices <- numeric()
  end_indices <- numeric()

  # Start index of the first storm
  start_index <- 1

  # Iterate over the data to extract storms
  for (i in 1:(length(data) - 1)) {
    if (i < (length(data) - num_zeros + 1) && all(data[i:(i + num_zeros - 1)] == 0,
      na.rm = TRUE
    )) {
      end_index <- i - 1
      if (start_index <= end_index) {
        storm_sum <- sum(data[start_index:end_index], na.rm = TRUE)
        storm_sums <- c(storm_sums, storm_sum)
        start_indices <- c(start_indices, start_index)
        end_indices <- c(end_indices, end_index)
      }
      start_index <- i + num_zeros
    }
  }

  if (start_index <= length(data)) {
    storm_sum <- sum(data[start_index:length(data)], na.rm = TRUE)
    storm_sums <- c(storm_sums, storm_sum)
    start_indices <- c(start_indices, start_index)
    end_indices <- c(end_indices, length(data))
  }

  # Ensure there are at least 3 storms by adding placeholder values
  while (length(storm_sums) < 3) {
    storm_sums <- c(storm_sums, 0)
    start_indices <- c(start_indices, 1)
    end_indices <- c(end_indices, length(data))
  }

  # Sorting and selecting top 3 storms
  top_storms_indices <- order(storm_sums, decreasing = TRUE)[1:3]
  top_storms <- cbind(
    storm_sums[top_storms_indices],
    start_indices[top_storms_indices],
    end_indices[top_storms_indices]
  )

  # Flatten and return the vector
  return(as.vector(t(top_storms)))
}


# # # Test the function
# storm_data <- c(1, 2, 3,NA,  4, 5, 0, 0, 6, 7, 8, 9,  10, 11)
# print(find_top_storm_sums_w_index(storm_data, 1)) # Separation by 1 zero
# print(find_top_storm_sums_w_index(storm_data, 2)) # Separation by 2 zeros
