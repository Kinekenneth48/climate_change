# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(RCurl)
library(XML)
library(tictoc)

source(file = "R/fetch_loca_nc_file_urls.R")
source(file = "R/download_loca_nc_data.R")


options(timeout = max(600, getOption("timeout"))) #increase timeout to 10mins


# ============================================================================#
# fetch all nc file urls in LOCA directory
# ============================================================================#
# 13 mins -- 3.8MB
tictoc::tic()
urls_nc_files_found = fetch_loca_nc_file_urls()
tictoc::toc()

save(urls_nc_files_found, file = "data-raw/RObjects/urls_nc_files_found.RData")

#load("data-raw/RObjects/urls_nc_files_found.RData")


# ============================================================================#
# download  nc files from LOCA directory
# ============================================================================#
# 11 mins
tictoc::tic() 
download_loca_nc_data(nc_files_urls = urls_nc_files_found,
                      download_dir = "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw",
                      model_names = "ACCESS-CM2")
tictoc::toc()


