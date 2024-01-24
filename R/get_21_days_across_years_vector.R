# Function to generate a vector of 21-day periods for a target date across multiple snow years
get_21_days_across_years_vector <- function(target_date, start_year, end_year) {
  target_day_month <- format(as.Date(target_date), "-%m-%d")
  target_month <- format(as.Date(target_date), "%m")

  # Initialize an empty vector to store the dates
  all_dates <- c()

  for (year in start_year:end_year) {
    snow_year_start <- as.Date(paste0(year, "-10-01"))
    # snow_year_end <- as.Date(paste0(year + 1, "-05-31"))
    snow_year_end <- as.Date(paste0(year + 1, "-09-30"))

    if (target_month %in% c(10, 11, 12)) {
      current_year_target_date <- as.Date(paste0(year, target_day_month))
    } else {
      current_year_target_date <- as.Date(paste0(year + 1, target_day_month))
    }


    if (current_year_target_date < snow_year_start || current_year_target_date > snow_year_end) {
      next
    }

    start_date <- max(current_year_target_date - 10, snow_year_start)
    end_date <- min(current_year_target_date + 10, snow_year_end)

    dates <- seq(from = start_date, to = end_date, by = "day")
    all_dates <- c(all_dates, dates) # Concatenate the dates
  }


  # Convert numerical dates to Date format
  all_dates <- as.Date(all_dates, origin = "1970-01-01")
  return(all_dates)
}
