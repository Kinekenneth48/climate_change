download_spear_day <- function(directory = ".", var = "tas",
                               scenario = c("historical", "future")) {
 pacman::p_load(curl)
  
  # Validate the variable
  available_vars <- c("tas", "tasmin", "tasmax", "pr", "psl", "uas", "vas")
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
    historical = c(
      "19210101-19301231", "19310101-19401231",
      "19410101-19501231", "19510101-19601231",
      "19610101-19701231", "19710101-19801231",
      "19810101-19901231", "19910101-20001231",
      "20010101-20101231", "20110101-20141231"
    ),
    future = c(
      "20150101-20201231", "20210101-20301231",
      "20310101-20401231", "20410101-20501231",
      "20510101-20601231", "20610101-20701231",
      "20710101-20801231", "20810101-20901231",
      "20910101-21001231"
    )
  )
  
  # Download function
  download_files <- function(base, periods, phase) {
    if (phase == "scenarioSSP5-85") {
      scenario_dir <- paste0(download_dir, "/", var, "/future/day")
      if (!dir.exists(scenario_dir)) {
        dir.create(scenario_dir, recursive = TRUE)
      }
    } else {
      scenario_dir <- paste0(download_dir, "/", var, "/", phase, "/day")
      if (!dir.exists(scenario_dir)) {
        dir.create(scenario_dir, recursive = TRUE)
      }
    }
    
    url =vector()
    dest = vector()
    for (r in 1:30) {
      for (period in periods) {
        i = 1+ length(url)
        url[i] <- paste0(
          base, "r", r, "i1p1f1/day/", var, "/gr3/v20210201/",
          var, "_day_GFDL-SPEAR-MED_", phase, "_r", r,
          "i1p1f1_gr3_", period, ".nc"
        )
        dest[i] <- paste0(scenario_dir, "/", basename(url[i]))
      
      }
    }
    
   multi_download(urls = url, destfiles =dest, resume = TRUE)
  }
  
  # Loop over specified scenarios
  for (s in scenario) {
    if (s == "future") {
      download_files(base_url[["future"]], time_periods[[s]], "scenarioSSP5-85")
    } else {
      download_files(base_url[[s]], time_periods[[s]], s)
    }
  }
}

