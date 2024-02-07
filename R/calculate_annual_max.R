
calculate_annual_max <- function(r, start_year) {
  # Define the start and end dates for the snow year
  start_date <- as.Date(paste(start_year, "10-01", sep = "-")) # October 1 of the starting year
  end_date <- as.Date(paste(start_year + 1, "09-30", sep = "-")) # September 30 of the following year
  
  # Extract the subset of the raster for the snow year
  r_subset <- subset(r, time(r) >= start_date & time(r) <= end_date)
  
  # Calculate the maximum snow depth for the subset
  annual_max <- max(r_subset, na.rm = TRUE)
  time(annual_max) <- start_year + 1
  
  # Return the annual max raster
  return(annual_max)
}