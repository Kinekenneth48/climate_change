analog_day_index <- function(future_layer, hist_layers, n = 5, common_dates, global = TRUE) {
  
  source("R/dates_for_analog_filter.R")
  
  # Extract the time associated with the future prediction layer
  reference_date <- terra::time(future_layer)
  
  # Determine the start and end years in the historical data
  hist_start_year <- lubridate::year(min(time(hist_layers)))
  hist_end_year <- lubridate::year(max(time(hist_layers)))
  
  # Use a custom function to find potential analog dates within a specified range
  potential_dates_for_analog_search <- dates_for_analog_filter(
    reference_date = reference_date, 
    hist_start_year = hist_start_year,
    hist_end_year = hist_end_year, 
    analog_range_to_search = 45
  )
  
  # Identify indices of historical layers that correspond to the potential analog dates
  analog_index_range <- which(common_dates %in% potential_dates_for_analog_search)
  
  # Select the historical layers that match the identified indices
  analog_hist_layers <- hist_layers[[analog_index_range]]
  
  # If global approach is selected:
  if (global == TRUE) {
    # Compute the error between each analog historical layer and the future layer
    error_layers <- (analog_hist_layers - future_layer)
    
    # Calculate the global root mean square error for each layer
    spatial_rmse <- terra::global(x = error_layers, fun = "rms", na.rm = TRUE)$rms
    
    # Order the layers by RMSE and select the top 'n'
    best_global_analog_index_vector <- sort(order(spatial_rmse)[1:n])
    best_analog_hist_layers <- analog_hist_layers[[best_global_analog_index_vector]]
    
    # Retrieve the dates corresponding to the best layers
    best_analog_dates <- terra::time(best_analog_hist_layers)
    
    # Find and return the original indices of the best analog dates in the common dates
    best_original_analog_index_vector <- which(common_dates %in% best_analog_dates)
    return(best_original_analog_index_vector)
    
    # If cell-by-cell approach is selected:
  } else {
    # Compute the absolute error for each cell between each analog layer and the future layer
    abs_error_layers <- abs(analog_hist_layers - future_layer)
    
    # Apply a function to find the best analogs for each cell
    best_cell_analog_index_layers <- app(abs_error_layers, function(x) order(x)[1:n])
    
    # Adjust the indices to match the original historical dataset
    best_original_analog_index_layers <- best_cell_analog_index_layers
    best_original_analog_index_layers[] <- analog_index_range[as.vector(best_cell_analog_index_layers)]
    
    # Return the adjusted indices
    return(best_original_analog_index_layers)
  }
}
