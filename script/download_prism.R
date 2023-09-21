library(prism) 

# ============================================================================#
# Download tmean from prism
# ============================================================================#

prism_set_dl_dir("C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw/prism/tmean")

# Download monthly average temperature data for years 1920-2022
get_prism_monthlys(type = "tmean", years = 1920:2022, mon = 1:12, keepZip = FALSE)


# ============================================================================#
# Check if all folders are downloaded
# ============================================================================#

# Set your base directory
base_dir <- "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw/prism/tmean"

years <- 1920:2022
months <- 1:12

missing_folders <- list()

# Loop through each year and month, and construct the expected folder name
for (year in years) {
  for (month in months) {
    # Construct the folder name based on the given pattern
    folder_name <- sprintf("PRISM_tmean_stable_4kmM3_%d%02d_bil", year, month)
    
    full_path <- file.path(base_dir, folder_name)
    
    # Check if the folder exists
    if (!dir.exists(full_path)) {
      missing_folders <- append(missing_folders, full_path)
    }
  }
}

# Print the paths of missing folders
if (length(missing_folders) > 0) {
  cat("Missing folders:\n")
  print(missing_folders)
} else {
  cat("All expected folders are present.")
}



# ============================================================================#
# Download ppt from prism
# ============================================================================#

prism_set_dl_dir("C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw/prism/ppt")


options(timeout = max(60000, getOption("timeout")))

# Download monthly ppt data for years 1920-2022
get_prism_monthlys(type = "ppt", years = 1920:2022, mon = 1:12, keepZip = FALSE)


# ============================================================================#
# Check if all folders are downloaded
# ============================================================================#

# Set your base directory
base_dir <- "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw/prism/ppt"

years <- 1920:1980
months <- 1:12

missing_folders <- list()

# Loop through each year and month, and construct the expected folder name
for (year in years) {
  for (month in months) {
    # Construct the folder name based on the given pattern
    folder_name <- sprintf("PRISM_ppt_stable_4kmM2_%d%02d_bil", year, month)
    
    full_path <- file.path(base_dir, folder_name)
    
    # Check if the folder exists
    if (!dir.exists(full_path)) {
      missing_folders <- append(missing_folders, full_path)
    }
  }
}

# Print the paths of missing folders
if (length(missing_folders) > 0) {
  cat("Missing folders:\n")
  print(missing_folders)
} else {
  cat("All expected folders are present.")
}

################################################################################

years <- 1981:2022
months <- 1:12

missing_folders <- list()

# Loop through each year and month, and construct the expected folder name
for (year in years) {
  for (month in months) {
    # Construct the folder name based on the given pattern
    folder_name <- sprintf("PRISM_ppt_stable_4kmM3_%d%02d_bil", year, month)
    
    full_path <- file.path(base_dir, folder_name)
    
    # Check if the folder exists
    if (!dir.exists(full_path)) {
      missing_folders <- append(missing_folders, full_path)
    }
  }
}

# Print the paths of missing folders
if (length(missing_folders) > 0) {
  cat("Missing folders:\n")
  print(missing_folders)
} else {
  cat("All expected folders are present.")
}

