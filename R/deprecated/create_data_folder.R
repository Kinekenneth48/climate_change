create_data_folder <- function(directory = ".") {
  # Check if 'directory' is a character string
  if (!is.character(directory) || length(directory) != 1) {
    stop("The 'directory' parameter should be a single string.")
  }


  # If directory is specified, validate its existence and permissions
  if (directory != ".") {
    if (!dir.exists(directory)) {
      stop("Specified directory does not exist.")
    }

    # Check if the directory is writable
    if (!file.access(directory, 2) == 0) {
      stop("Specified directory is not writable.")
    }

    # Use the specified directory
    download_dir <- directory
  } else {
    # If directory is not specified, use the current working directory
    download_dir <- getwd()
  }


  # Expand tilde and environment variables in the directory path
  download_dir <- path.expand(download_dir)

  # Remove any trailing slash from the directory path
  download_dir <- gsub(download_dir, pattern = "/$", replacement = "")

  # Append '/spear_data' to the directory path for the new folder
  download_dir <- paste0(download_dir, "/spear_data")

  # Delete the existing folder and its contents
  if (dir.exists(download_dir)) {
    unlink(download_dir, recursive = TRUE)
  }

  # Create the new folder
  success <- dir.create(download_dir)

  # Check if folder creation was successful
  if (success) {
    return(paste("Successfully created folder at:", download_dir))
  } else {
    stop("Failed to create folder.")
  }
}
