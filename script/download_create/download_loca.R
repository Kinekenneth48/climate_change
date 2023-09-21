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
# 13 mins -- 1.7MB - 6,168 elements
tictoc::tic()
urls_nc_files_found_6km <- fetch_loca_nc_file_urls(resolution = "6km")
tictoc::toc()

save(urls_nc_files_found_6km, file = "data-raw/RObjects/urls_nc_files_found_6km.RData")

# 13 mins -- 3.8MB - 13,794 elements
tictoc::tic()
urls_nc_files_found_all <- fetch_loca_nc_file_urls(resolution = "all")
tictoc::toc()

save(urls_nc_files_found_all, file = "data-raw/RObjects/urls_nc_files_found_all.RData")


load("data-raw/RObjects/urls_nc_files_found_6km.RData")


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
  nc_files_urls = urls_nc_files_found_6km, historical = TRUE,
  monthly = TRUE, variable_name = "tasmin",
  download_dir = "C:/Users/Ken/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw",
  model_names = "ACCESS-CM2"
)

# download future data
download_loca_nc_data(
  nc_files_urls = urls_nc_files_found_6km, historical = FALSE,
  monthly = TRUE, variable_name = "tasmin",
  download_dir = "C:/Users/Ken/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw",
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
  nc_files_urls = urls_nc_files_found_all, historical = TRUE,
  monthly = FALSE, variable_name = "pr",
  download_dir = "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw",
  model_names = "ACCESS-CM2"
)

# download future data
download_loca_nc_data(
  nc_files_urls = urls_nc_files_found_all, historical = FALSE, 
  monthly = FALSE, variable_name = "pr",
  download_dir = "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw",
  model_names = "ACCESS-CM2"
)
tictoc::toc()


r =terra::rast("data-raw/loca/ACCESS-CM2/pr/historical/month/pr.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA_16thdeg_v20220519.monthly.nc")

t = terra::rast("data-raw\\spear\\pr\\historical\\month\\pr_Amon_GFDL-SPEAR-MED_historical_r3i1p1f1_gr3_192101-201412.nc")
