###############################################################################
### STEP 0: INITIAL SETUP
###############################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(rsnodas)

options(timeout = max(6000, getOption("timeout"))) # increase timeout to 10mins

# ============================================================================#
# download daily snodas data
# ============================================================================#
# Define the start and end dates
start_date <- as.Date("2003-10-01")
end_date <- as.Date("2023-07-31")

# Generate a sequence of all days between start_date and end_date
all_days <- seq(from = start_date, to = end_date, by = "days")


# Initialize an error log vector
error_log <- character()

# Loop through each date in all_days
for (i in 1:length(all_days)) {
  # Use tryCatch to handle errors
  tryCatch({
    download_snodas(
      dates = all_days[i],
      masked = TRUE,
      overwrite = TRUE,
      remove_zip = TRUE,
      data_saved = c("swe"),
      out_dir = paste0("D:/data-raw/snodas"),
      GTiff = TRUE
    )
  }, error = function(e) {
    # If an error occurs, append it to the error_log
    error_log <- c(error_log, paste("Date:", all_days[i], "Error:", e$message))
  })
}

# If there are any errors, print them or save them to a file
if (length(error_log) > 0) {
  writeLines(error_log, con = "error_log.txt") # Save to a file
  print(error_log) # Print to the console
}
