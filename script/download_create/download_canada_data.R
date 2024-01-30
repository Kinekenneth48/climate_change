
library(RCurl)
library(ncdf4)


# Example usage
download_nc_files_canada(
  base_url="https://crd-data-donnees-rdc.ec.gc.ca/CCCMA/products/CanSISE/output/CCCma/CanRCM4/",
  target_directory=    "D:/data-raw/canada_model")
