download_cmip6 <- function(model, variable , frequency, period, directory = ".") {
  
  # Increase timeout 
  options(timeout = max(6000000, getOption("timeout")))
  
  if (!requireNamespace("epwshiftr", quietly = TRUE)) {
    install.packages("epwshiftr")
  }
  
  valid_variables <- c("pr", "tas", "snw")
  valid_frequencies <- c("day", "mon", "snw")
  valid_periods <- c("historical", "future")
  
  # Check if 'period' is valid
  if (!(period %in% valid_periods)) {
    stop("Invalid 'period' parameter. It must be 'historical' or 'future'.")
  }
  
  # Check if 'variable' is valid
  if (!(variable %in% valid_variables)) {
    stop("Invalid 'variable' parameter. 'variable' must be one of 'pr' or 'tas'.")
  }
  
  # Check if 'frequency' is valid
  if (!(frequency %in% valid_frequencies)) {
    stop("Invalid 'frequency' parameter. It must be 'day' or 'mon'.")
  }
  
  download_dir <- ifelse(directory == ".", getwd(), directory)
  scenario_dir <- file.path(download_dir, model, variable, period, frequency)
  
  if (!dir.exists(scenario_dir)) {
    dir.create(scenario_dir, recursive = TRUE)
  }
  
  if (period == "historical") {
    idx <- epwshiftr::init_cmip6_index(
      activity = "CMIP",
      variable = variable,
      frequency = frequency,
      experiment = "historical",
      source = NULL,
      variant = NULL,
      replica = FALSE,
      resolution = NULL,
      latest = TRUE,
      data_node = NULL
    )
  } else if (period == "future") {
    idx <- epwshiftr::init_cmip6_index(
      activity = "ScenarioMIP",
      variable = variable,
      frequency = frequency,
      experiment = c("ssp126", "ssp245", "ssp370", "ssp585"),
      source = NULL,
      variant = NULL,
      replica = FALSE,
      resolution = NULL,
      latest = TRUE,
      data_node = NULL
    )
  }
  
  if (is.null(idx)) {
    stop("Error initializing CMIP6 index. Check your input parameters.")
  }
  
  idx <- idx[idx$source_id == model,]
  
  if (nrow(idx) == 0) {
    message("No matching data found for the specified model.")
    return(NULL)
  }
  
  #get url of specified model
  url <- idx$file_url
  
  for (i in 1:length(url)) {
    dest <- file.path(scenario_dir, basename(url[i]))
    tryCatch({
      download.file(url = url[i], destfile = dest, mode = "wb")
      message(paste("Downloaded:", dest))
    }, error = function(e) {
      warning(paste("Error downloading:", dest))
    })
  }
}
