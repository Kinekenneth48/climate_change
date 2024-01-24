download_spear_month <- function(directory = ".", var = "tas",
                                 scenario = c("historical", "future")) {
  # Increase timeout to 10 mins
  options(timeout = max(600, getOption("timeout")))

  # Validate the variable
  available_vars <- c(
    "tas", "ta", "pr", "hus", "psl", "zg", "sfcWind",
    "uas", "vas", "ua", "va", "rlut", "rsut", "rsdt"
  )
  if (!var %in% available_vars) {
    stop("The 'var' parameter is not an available variable to download.")
  }

  # Create root download directory
  download_dir <- ifelse(directory == ".", getwd(), directory)
  download_dir <- paste0(path.expand(gsub(download_dir,
    pattern = "/$",
    replacement = ""
  )), "/spear")

  # Initialize list to store failed downloads
  failed_downloads <- vector("character", 0)

  # Base URLs for each scenario
  base_url <- list(
    historical = "https://noaa-gfdl-spear-large-ensembles-pds.s3.amazonaws.com/SPEAR/GFDL-LARGE-ENSEMBLES/CMIP/NOAA-GFDL/GFDL-SPEAR-MED/historical/",
    future = "https://noaa-gfdl-spear-large-ensembles-pds.s3.amazonaws.com/SPEAR/GFDL-LARGE-ENSEMBLES/CMIP/NOAA-GFDL/GFDL-SPEAR-MED/scenarioSSP5-85/"
  )

  # Time periods for each scenario
  time_periods <- list(
    historical = c("192101-201412"),
    future = c("201501-210012")
  )

  # Download function
  download_files <- function(base, periods, phase) {
    if (phase == "scenarioSSP5-85") {
      scenario_dir <- paste0(download_dir, "/", var, "/future/month")
      if (!dir.exists(scenario_dir)) {
        dir.create(scenario_dir, recursive = TRUE)
      }
    } else {
      scenario_dir <- paste0(download_dir, "/", var, "/", phase, "/month")
      if (!dir.exists(scenario_dir)) {
        dir.create(scenario_dir, recursive = TRUE)
      }
    }

    for (r in 1:30) {
      for (period in periods) {
        url <- paste0(
          base, "r", r, "i1p1f1/Amon/", var, "/gr3/v20210201/", var,
          "_Amon_GFDL-SPEAR-MED_", phase, "_r",
          r, "i1p1f1_gr3_", period, ".nc"
        )
        dest <- paste0(scenario_dir, "/", basename(url))
        tryCatch(
          {
            utils::download.file(url = url, destfile = dest, mode="wb")
            message(paste("Successfully downloaded: ", url))
          },
          error = function(e) {
            message(paste("Failed to download: ", url))
            failed_downloads <<- c(failed_downloads, url)
          }
        )
      }
    }
  }

  # Loop over specified scenarios
  for (s in scenario) {
    if (s == "future") {
      download_files(base_url[["future"]], time_periods[[s]], "scenarioSSP5-85")
    } else {
      download_files(base_url[[s]], time_periods[[s]], s)
    }
  }

  message("All monthly files are downloaded.")

  # Print the list of failed downloads
  if (length(failed_downloads) > 0) {
    message("The following files failed to download:")
    for (url in failed_downloads) {
      message(url)
    }
  }
}

# Example usage
# download_spear_month(directory = "./data", var = "tas", scenario = "historical")
# download_spear_month(directory = "./data", var = "tas", scenario = "future")
# download_spear_month(directory = "./data", var = "tas", scenario = c("historical", "future"))
