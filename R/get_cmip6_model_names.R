get_cmip6_model_names <- function(variable = 'pr', frequency = "day",
    period = "historical"
) {
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
    
    #return model names
    return(unique(idx$source_id))
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
    
    #return model names
    return(unique(idx$source_id))
  }
  

}
