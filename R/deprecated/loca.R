# Load the necessary libraries
library(RCurl)
library(XML)
library(tictoc)


tic()
# Define the base URL for the models
base_url <- "https://cirrus.ucsd.edu/~pierce/LOCA2/NAmer/"

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

  # Send a GET request to the current directory URL, disabling SSL certificate verification
  html_content <- RCurl::getURL(current_url, followlocation = TRUE, ssl.verifypeer = FALSE)
  page <- XML::htmlParse(html_content, asText = TRUE)

  # Extract the .nc file URLs in the current directory with their full URLs
  nc_file_elements <- XML::xpathSApply(page, "//a[contains(@href,'.nc')]", xmlAttrs)

  # Attach the .nc file names to their corresponding current directory paths and URLs
  nc_files <- list()
  for (nc_file_attrs in nc_file_elements) {
    nc_url <- nc_file_attrs
    full_nc_url <- paste0(current_url, nc_url)
    nc_files <- c(nc_files, full_nc_url)
  }

  # Append the current directory's URL and path and attached .nc file names to the list of found URLs
  if (length(nc_files) > 0) {
    nc_files_found <- c(nc_files_found, nc_files)
  }

  # Extract subdirectory links
  subdirectory_links <- XML::xpathSApply(page, "//a[contains(@href,'/')]", xmlValue)

  # Add subdirectories to the stack for further processing with their URLs
  for (subdir_link in subdirectory_links) {
    subdir_url <- paste0(current_url, subdir_link)
    directory_stack <- c(directory_stack, list(list(url = subdir_url)))
  }
}
toc()

# Print the full URLs of the .nc files
if (length(nc_files_found) > 0) {
  cat("Found .nc files with their URLs:\n")
  print(nc_files_found)
} else {
  cat("No .nc files found.\n")
}

save(nc_files_found, file = "data-raw/RObjects/nc_files_found.RData")

load("data-raw/RObjects/nc_files_found.RData"
     )
# =============================================================================

models <- c(
  "ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR",
  "CESM2-LENS", "CNRM-CM6-1-HR", "CNRM-CM6-1", "CNRM-ESM2-1", "CanESM5",
  "EC-Earth3-Veg", "EC-Earth3", "FGOALS-g3", "GFDL-CM4", "GFDL-ESM4",
  "HadGEM3-GC31-LL", "HadGEM3-GC31-MM", "INM-CM4-8", "INM-CM5-0",
  "IPSL-CM6A-LR", "KACE-1-0-G", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR",
  "MRI-ESM2-0", "NorESM2-LM", "NorESM2-MM", "TaiESM1"
)



# =============================================================================

find_matching_urls <- function(model_name, variable_name, historical = TRUE, monthly = TRUE) {
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

  for (url in nc_files_found) {
    # Check if the pattern is found in the URL
    if (grepl(pattern, url)) {
      matching_urls <- c(matching_urls, url)
    }
  }

  if (monthly == FALSE) {
    # Filter URLs that don't have "monthly" in their names
    matching_urls <- grep("monthly", matching_urls, value = TRUE, invert = TRUE)
  }

  # Return the list of matching URLs
  return(matching_urls)
}



matching_urls <- find_matching_urls(
  model_name = "MPI-ESM1-2-LR",
  variable_name = "tasmin",
  historical = TRUE, monthly = FALSE
)

# Print the matching URLs
print(matching_urls)

# ==============================================================================
