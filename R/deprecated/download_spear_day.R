download_spear_day <- function(directory = ".", var = "tas") {
  
  #increase timeout to 10mins
  options(timeout = max(600, getOption("timeout"))) 
  
  # Validate the variable
  available_vars <- c("tas", "tasmin", "tasmax", "pr", "psl", "uas", "vas")
  if (!var %in% available_vars) {
    stop("The 'var' parameter is not an available variable to download.")
  }
  
  # Create download directory
  download_dir <- ifelse(directory == ".", getwd(), directory)
  download_dir <- paste0(path.expand(gsub(download_dir, pattern = "/$",
                                          replacement = "")), "/spear_day_data")
  
  # Clear and create directory
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
    hist = c("19210101-19301231", "19310101-19401231", "19410101-19501231",
             "19510101-19601231", "19610101-19701231", "19710101-19801231", 
             "19810101-19901231", "19910101-20001231", "20010101-20101231", 
             "20110101-20141231"),
    future = c("20150101-20201231", "20210101-20301231", "20310101-20401231",
               "20410101-20501231", "20510101-20601231", "20610101-20701231", 
               "20710101-20801231", "20810101-20901231", "20910101-21001231")
  )
  
  # Download function
  download_files <- function(base, periods, phase) {
    for (r in 1:30) {
      for (period in periods) {
        url <- paste0(base, "r", r, "i1p1f1/day/", var, "/gr3/v20210201/", 
                      var, "_day_GFDL-SPEAR-MED_", phase, "_r", r,
                      "i1p1f1_gr3_", period, ".nc")
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
  
  message("All daily files are downloaded.")
  
  # Print the list of failed downloads
  if (length(failed_downloads) > 0) {
    message("The following files failed to download:")
    for (url in failed_downloads) {
      message(url)
    }
  }
}
