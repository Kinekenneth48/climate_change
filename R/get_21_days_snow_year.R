# Function to generate 21 days around a target date, considering the snow year
get_21_days_snow_year <- function(target_date) {
  target_date <- as.Date(target_date)
  
  # Extract year as a numeric value
  year <- as.numeric(format(target_date, "%Y"))
  
  # Define snow year start and end dates based on the target year
  snow_year_start <- as.Date(paste0(year, "-10-01"))
 # snow_year_end <- as.Date(paste0(year + 1, "-05-31"))
  snow_year_end <- as.Date(paste0(year + 1, "-09-30"))
  
  # Adjust if target date is before October
  if (target_date < snow_year_start) {
    snow_year_start <- as.Date(paste0(year - 1, "-10-01"))
   # snow_year_end <- as.Date(paste0(year, "-05-31"))
    snow_year_end <- as.Date(paste0(year, "-09-30"))
  }
  
  # Calculate dates 10 days before and after the target date
  start_date <- max(target_date - 10, snow_year_start)
  end_date <- min(target_date + 10, snow_year_end)
  
  dates <- seq(from = start_date, to = end_date, by = "day")
  return(dates)
}
