# Full and subset data
full_data <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
subset_data <- c(30, 50, 70)
subset_indices_in_full <- c(3, 5, 7)

# Best analog in subset_data
best_analog_index_in_subset <- 2  # Index of 50 in subset_data

# Adjust index to refer to full_data
best_analog_index_in_full <- subset_indices_in_full[best_analog_index_in_subset]

# Output
print(paste("Best analog index in full data is:", best_analog_index_in_full))
