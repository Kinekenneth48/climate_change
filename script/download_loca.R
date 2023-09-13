###############################################################################
### STEP 0: INITIAL SETUP
###############################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(RCurl)
library(XML)
library(tictoc)

source(file = "R/fetch_loca_nc_file_urls.R")
source(file = "R/download_loca_nc_data.R")


options(timeout = max(600, getOption("timeout"))) # increase timeout to 10mins


# ============================================================================#
# fetch all nc file urls in LOCA directory
# ============================================================================#
# 13 mins -- 3.8MB
tictoc::tic()
urls_nc_files_found <- fetch_loca_nc_file_urls()
tictoc::toc()

save(urls_nc_files_found, file = "data-raw/RObjects/urls_nc_files_found.RData")






###############################################################################
### STEP 1: DOWNLOAD NC FILES
###############################################################################

# load("data-raw/RObjects/urls_nc_files_found.RData")
# ============================================================================#
# download  nc files from LOCA directory - MONTHLY
# ============================================================================#
# 11 mins - ACCESS-CM2 - 1 hour
tictoc::tic()
# download hist data
download_loca_nc_data(
  nc_files_urls = urls_nc_files_found, historical = TRUE,
  monthly = TRUE, variable_name = "pr",
  download_dir = "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw",
  model_names = "ACCESS-CM2"
)

# download future data
download_loca_nc_data(
  nc_files_urls = urls_nc_files_found, historical = FALSE,
  monthly = TRUE, variable_name = "pr",
  download_dir = "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw",
  model_names = "ACCESS-CM2"
)
tictoc::toc()

# ============================================================================#
# download  nc files from LOCA directory - DAY
# ============================================================================#
# ACCESS-CM2 6 hours
tictoc::tic()
# download hist data
download_loca_nc_data(
  nc_files_urls = urls_nc_files_found, historical = TRUE,
  monthly = FALSE, variable_name = "pr",
  download_dir = "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw",
  model_names = "ACCESS-CM2"
)

# download future data
download_loca_nc_data(
  nc_files_urls = urls_nc_files_found, historical = FALSE, 
  monthly = FALSE, variable_name = "pr",
  download_dir = "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw",
  model_names = "ACCESS-CM2"
)
tictoc::toc()
