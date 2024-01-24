fetch_loca_nc_file_urls <- function(base_url = "https://cirrus.ucsd.edu/~pierce/LOCA2/NAmer/",
                                    resolution = "all") {
  nc_files_found <- character(0)
  directory_stack <- list(list(url = base_url))
  
  message("fetching requested .nc file urls from base url... ")
  
  while (length(directory_stack) > 0) {
    current_entry <- directory_stack[[length(directory_stack)]]
    current_url <- current_entry$url
    directory_stack <- directory_stack[-length(directory_stack)]
    
    if (grepl("Parent Directory$", current_url)) {
      next
    }
    
    html_content <- RCurl::getURL(current_url,
                                  followlocation = TRUE,
                                  ssl.verifypeer = FALSE
    )
    page <- XML::htmlParse(html_content, asText = TRUE)
    
    xpath_query <- switch(
      resolution,
      "6km" = "//a[substring(@href, string-length(@href) - string-length('.nc') +1) = '.nc' and not(contains(@href, '0p5x0p5'))]",
      "50km" = "//a[contains(@href, '0p5x0p5.nc')]",
      "all" = "//a[contains(@href,'.nc')]",
      stop("Invalid resolution")
    )
    
    nc_file_elements <- XML::xpathSApply(
      page, xpath_query,
      xmlAttrs
    )
    
    nc_files <- list()
    for (nc_file_attrs in nc_file_elements) {
      nc_url <- nc_file_attrs
      full_nc_url <- paste0(current_url, nc_url)
      nc_files <- c(nc_files, full_nc_url)
    }
    
    if (length(nc_files) > 0) {
      nc_files_found <- c(nc_files_found, nc_files)
    }
    
    subdirectory_links <- XML::xpathSApply(
      page, "//a[contains(@href,'/')]",
      xmlValue
    )
    
    for (subdir_link in subdirectory_links) {
      subdir_url <- paste0(current_url, subdir_link)
      directory_stack <- c(directory_stack, list(list(url = subdir_url)))
    }
  }
  
  return(nc_files_found)
}
