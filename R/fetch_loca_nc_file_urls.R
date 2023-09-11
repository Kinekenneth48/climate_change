fetch_loca_nc_file_urls <- function(base_url = "https://cirrus.ucsd.edu/~pierce/LOCA2/NAmer/") {
  # Create an empty list to store the full URLs of .nc files
  nc_files_found <- character(0)

  # Initialize the directory stack with the base URL and the base path (same as URL)
  directory_stack <- list(list(url = base_url))

  # While there are directories in the stack, continue searching
  while (length(directory_stack) > 0) {
    # Get the next directory to process along with its URL
    current_entry <- directory_stack[[length(directory_stack)]]
    current_url <- current_entry$url
    directory_stack <- directory_stack[-length(directory_stack)]

    # Skip entries that represent links to parent directories
    if (grepl("Parent Directory$", current_url)) {
      next
    }

    # Send a GET request to the current directory URL
    html_content <- RCurl::getURL(current_url,
      followlocation = TRUE,
      ssl.verifypeer = FALSE
    )
    page <- XML::htmlParse(html_content, asText = TRUE)

    # Extract the .nc file URLs in the current directory with their full URLs
    nc_file_elements <- XML::xpathSApply(
      page, "//a[contains(@href,'.nc')]",
      xmlAttrs
    )

    # Attach the .nc file names to their  current directory paths and URLs
    nc_files <- list()
    for (nc_file_attrs in nc_file_elements) {
      nc_url <- nc_file_attrs
      full_nc_url <- paste0(current_url, nc_url)
      nc_files <- c(nc_files, full_nc_url)
    }

    # Append the current directory's URL and path and attached .nc file names 
    # to the list of found URLs
    if (length(nc_files) > 0) {
      nc_files_found <- c(nc_files_found, nc_files)
    }

    # Extract sub directory links
    subdirectory_links <- XML::xpathSApply(
      page, "//a[contains(@href,'/')]",
      xmlValue
    )

    # Add sub directories to the stack for further processing with their URLs
    for (subdir_link in subdirectory_links) {
      subdir_url <- paste0(current_url, subdir_link)
      directory_stack <- c(directory_stack, list(list(url = subdir_url)))
    }
  }

  return(nc_files_found)
}
