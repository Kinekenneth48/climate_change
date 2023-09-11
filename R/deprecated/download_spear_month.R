download_spear_month <- function(directory = ".", var = "tas") {
  
  #increase timeout to 5mins
  options(timeout = max(300, getOption("timeout"))) 
  
  # Validate the variable
  available_vars <- c(
    "tas", "ta", "pr", "hus", "psl", "zg", "sfcWind",
    "uas", "vas", "ua", "va", "rlut", "rsut", "rsdt"
  )
  if (!var %in% available_vars) {
    stop("The 'var' parameter is not an available variable to download.")
  }
  
  # Create download directory
  download_dir <- ifelse(directory == ".", getwd(), directory)
  download_dir <- paste0(path.expand(gsub(download_dir,
                                          pattern = "/$",
                                          replacement = ""
  )), "/spear_month_data")
  
  # Clear directory
  unlink(download_dir, recursive = TRUE)
  dir.create(download_dir, showWarnings = FALSE)
  
  # Initialize list to store failed downloads
  failed_downloads <- vector("character", 0)
  
  # Base URLs
  base_url <- list(
    hist = "https://noaa-gfdl-spear-large-ensembles-pds.s3.amazonaws.com/SPEAR/GFDL-LARGE-ENSEMBLES/CMIP/NOAA-GFDL/GFDL-SPEAR-MED/historical/",
    future = "https://noaa-gfdl-spear-large-ensembles-pds.s3.amazonaws.com/SPEAR/GFDL-LARGE-ENSEMBLES/CMIP/NOAA-GFDL/GFDL-SPEAR-MED/scenarioSSP5-85/"
  )
  
  # Time periods
  time_periods <- list(
    hist = c("192101-201412"),
    future = c("201501-210012")
  )
  
  # Download function with error handling
  download_files <- function(base, periods, phase) {
    for (r in 1:30) {
      for (period in periods) {
        url <- paste0(
          base, "r", r, "i1p1f1/Amon/", var, "/gr3/v20210201/", var,
          "_Amon_GFDL-SPEAR-MED_", phase, "_r",
          r, "i1p1f1_gr3_", period, ".nc"
        )
        dest <- paste0(download_dir, "/", basename(url))
        tryCatch({
          utils::download.file(url = url, destfile = dest)
          message(paste("Successfully downloaded: ", url))
        }, error = function(e) {
          message(paste("Failed to download: ", url))
          failed_downloads <<- c(failed_downloads, url)
        })
      }
    }
  }
  
  # Download historical and future files
  download_files(base_url$hist, time_periods$hist, "historical")
  download_files(base_url$future, time_periods$future, "scenarioSSP5-85")
  
  #message("All monthly files are downloaded.")
  
  # Print the list of failed downloads
  if (length(failed_downloads) > 0) {
    message("The following files failed to download:")
    for (url in failed_downloads) {
      message(url)
    }
  }
}
