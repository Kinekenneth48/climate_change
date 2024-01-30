
download_loca_nc_data <- function(nc_files_urls, variable_name = "pr",
                                  historical = TRUE, monthly = TRUE,
                                  download_dir = ".",
                                  model_names = NULL) {
  
  #increase timeout 
  options(timeout = max(60000, getOption("timeout"))) 
  
  # Validate the variable name
  valid_variable_names <- c("pr", "tasmax", "tasmin")
  if (!(variable_name %in% valid_variable_names)) {
    stop(paste("Invalid variable name: ", variable_name))
  }

  # Define a list of known climate models
  known_models <- c(
    "ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR",
    "CESM2-LENS", "CNRM-CM6-1-HR", "CNRM-CM6-1", "CNRM-ESM2-1", "CanESM5",
    "EC-Earth3-Veg", "EC-Earth3", "FGOALS-g3", "GFDL-CM4", "GFDL-ESM4",
    "HadGEM3-GC31-LL", "HadGEM3-GC31-MM", "INM-CM4-8", "INM-CM5-0",
    "IPSL-CM6A-LR", "KACE-1-0-G", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR",
    "MRI-ESM2-0", "NorESM2-LM", "NorESM2-MM", "TaiESM1"
  )

  # Validate user-supplied model_names
  if (is.null(model_names)) {
    model_names <- known_models
  } else {
    invalid_models <- setdiff(model_names, known_models)
    if (length(invalid_models) > 0) {
      stop(paste("Invalid model names: ", paste(invalid_models, collapse = ", ")))
    }
  }




  for (model_name in model_names) {
    # Construct the regular expression pattern based on the conditions
    pattern <- paste0(".*", model_name, ".*", variable_name, ".*")

    if (historical) {
      pattern <- paste0(pattern, "historical")
    } else {
      pattern <- paste0(pattern, "ssp")
    }

    if (monthly) {
      pattern <- paste0(pattern, ".*", "monthly")
    }

    # Create an empty list to store matching URLs
    matching_urls <- character(0)

    for (url in nc_files_urls) {
      # Check if the pattern is found in the URL
      if (grepl(pattern, url)) {
        matching_urls <- c(matching_urls, url)
      }
    }

    if (monthly == FALSE) {
      # Filter URLs that don't have "monthly" in their names
      matching_urls <- grep("monthly", matching_urls,
        value = TRUE,
        invert = TRUE
      )
    }

    # Create a directory structure reflecting model name, variable name,
    # and historical/future
    time_folder <- ifelse(historical, "historical", "future")
    freq_folder <- ifelse(monthly, "month", "day")
    specific_directory <- file.path(
      download_dir, "loca", model_name,
      variable_name, time_folder, freq_folder
    )
    if (!dir.exists(specific_directory)) {
      dir.create(specific_directory, recursive = TRUE)
    }

    # Download the files from the matching URLs
    for (url in matching_urls) {
      file_name <- basename(url)
      destination <- file.path(specific_directory, file_name)
      
      # Check if the file already exists
      if (!file.exists(destination)) {
        message(paste("Downloading", url, "to", destination))
        download.file(url, destination, mode = "wb")
      } else {
        message(paste("File already exists:", destination))
      }
    }
  }
}
