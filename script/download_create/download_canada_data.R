
library(RCurl)
library(ncdf4)
source("R/download_nc_files_canada.R")
source("R/download_snw_canada.R")

# Example usage
download_nc_files_canada(
  base_url="https://crd-data-donnees-rdc.ec.gc.ca/CCCMA/products/CanSISE/output/CCCma/CanRCM4/",
  target_directory=    "D:/data-raw/canada_model/snd")

# Example usage
download_snw_canada(
  base_url="https://crd-data-donnees-rdc.ec.gc.ca/CCCMA/products/CanSISE/output/CCCma/CanRCM4/",
  target_directory=    "D:/data-raw/canada_model/snw")