dates_for_analog_filter <- function(reference_date = as.Date("2023-07-15"),
                                    hist_start_year = 2010,
                                    hist_end_year = 2020, analog_range_to_search = 5) {
  # get month and day from reference_date
  month <- format(reference_date, "%m")
  day <- format(reference_date, "%d")
  
  # Generate a list of years between hist_start_year and hist_end_year
  years_list <- seq(hist_start_year, hist_end_year, by = 1)
  
  # Initialize an empty list to store the dates for each year
  dates_for_analog_filter <- list()
  
  for (i in 1:length(years_list)) {
    hist_pivot_date <- as.Date(paste(years_list[i], month, day, sep = "-"),
                               format = "%Y-%m-%d"
    )
    
    # Calculate the date for each day within the range
    days_before <- seq(hist_pivot_date - analog_range_to_search, hist_pivot_date - 1, by = "days")
    days_after <- seq(hist_pivot_date, hist_pivot_date + analog_range_to_search, by = "days")
    
    date_range <- c(days_before, days_after)
    
    # Add the date range for this year to the list
    dates_for_analog_filter[[as.character(years_list[i])]] <- date_range
  }
  
  dates_for_analog_filter <- do.call(c, dates_for_analog_filter)
  names(dates_for_analog_filter) <- NULL
  
  return(dates_for_analog_filter)
}
