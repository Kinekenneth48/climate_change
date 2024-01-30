download_nc_files <- function(base_url, target_directory) {
  # Load necessary libraries
  if (!requireNamespace("RCurl", quietly = TRUE)) {
    install.packages("RCurl")
    library(RCurl)
  }
  if (!requireNamespace("XML", quietly = TRUE)) {
    install.packages("XML")
    library(XML)
  }
  
  # Function to list all directories and files in a URL
  list_url <- function(url) {
    doc <- RCurl::getURL(url, .opts = list(ssl.verifypeer = FALSE))
    links <- strsplit(doc, "\n")[[1]]
    dirs <- grep("href=\"[^\"]*/\"", links, value = TRUE)
    dirs <- sub(".*href=\"", "", dirs)
    dirs <- sub("/\">.*", "/", dirs)
    return(dirs)
  }
  
  list_url_nc <- function(url) {
    # Get the HTML content from the URL
    doc <- RCurl::getURL(url, .opts = list(ssl.verifypeer = FALSE))
    # Parse HTML content
    parsed_html <- XML::htmlParse(doc)
    
    # Extract the links (href attributes) from the anchor tags
    links <- XML::xpathSApply(parsed_html, "//a", XML::xmlGetAttr, "href")
    
    # Filter out links that do not end in .nc (NetCDF files)
    nc_files <- links[grepl("\\.nc$", links)]
    
    return(nc_files)
  }
  
  
  #increase timeout 
  options(timeout = max(60000, getOption("timeout"))) 
  
  
  # Ensure target directory exists or create it
  if (!dir.exists(target_directory)) {
    dir.create(target_directory, recursive = TRUE)
  }
  
  # Get list of main directories
  main_dirs <- list_url(base_url)
  main_dirs <- main_dirs[-1] # Skip the first non-relevant element
  
  for (main_dir in main_dirs) {
    main_dir_url <- paste0(base_url, main_dir)
    specific_path_url <- paste0(main_dir_url, "day/atmos/snd/")
    
    # List second-level subdirectories (r1i1p1, r2i1p1, etc.)
    second_level_dirs <- list_url(specific_path_url)
    second_level_dirs <- second_level_dirs[-1] # Skip the first non-relevant element
    
    for (second_level_dir in second_level_dirs) {
      second_level_dir_url <- paste0(specific_path_url, second_level_dir)
      
      # Create a directory in the target directory
      second_level_subdir <- paste0(target_directory, "/", main_dir, second_level_dir)
      dir.create(second_level_subdir, recursive = TRUE)
      
      # List .nc files in the second-level sub-directory
      nc_files <- list_url_nc(second_level_dir_url)
      nc_files <- nc_files[grepl("\\.nc$", nc_files)]
      
      for (file in nc_files) {
        file_url <- paste0(second_level_dir_url, file)
        
        message(paste("Downloading", file_url))
        
        # Download file to the respective subdirectory
        download.file(file_url, destfile = paste0(second_level_subdir, file), mode = "wb")
      }
    }
  }
}
