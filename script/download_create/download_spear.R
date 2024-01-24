# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(stars)
library(raster)
library(terra)
library(tictoc)

source(file = "R/download_spear_day.R")
source(file = "R/download_spear_month.R")

options(timeout = max(300, getOption("timeout"))) #increase timeout to 5mins

# ============================================================================#
# download spear data
# ============================================================================#
#download daily data
tictoc::tic()
download_spear_day(
  #directory = "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw",
  directory = "D:/data-raw/spear/pr/future/day",
  var = "pr", 
  scenario = c( "future")
 # scenario = c("historical", "future")
)
tictoc::toc()

#download monthly data (2223.61 secs - 37 mins)
tictoc::tic()
download_spear_month(
  directory = "C:/Users/KENNETH/OneDrive - USU/RESEARCH CODE/project/climate_change/data-raw",
  var = "tas", 
  scenario = c("historical", "future")
)
tictoc::toc()

# ============================================================================#
# load spear data 
# ============================================================================#
#download monthly data
spear_month_hist <- terra::rast(
  x = "data-raw/spear_month_data/tas_Amon_GFDL-SPEAR-MED_historical_r1i1p1f1_gr3_192101-201412.nc")



# ============================================================================#
# validate spear data (world)
# ============================================================================#
# Subset to get the first layer
single_layer <- spear_month_hist[[1]]

#  plot the single layer to visualize it
plot(single_layer)




# ============================================================================#
# validate spear data (USA)
# ============================================================================#

# Creating a bounding box for USA mainland
# Format: xmin, xmax, ymin, ymax
#usa_extent <- c(-125, -66, 24, 50)

usa_extent_adjusted <- terra::ext(235, 294, 24, 50)  # 360 - 125 = 235, 360 - 66 = 294
cropped_raster <- terra::crop(spear_month_hist[[1]], usa_extent_adjusted)


# Crop the raster to USA extent
cropped_raster <- crop(spear_month_hist[[1]], usa_extent)

# Plot the cropped raster
plot(cropped_raster)






