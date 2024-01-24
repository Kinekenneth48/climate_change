
##############################################################################
## STEP 0: INITIAL SETUP
##############################################################################
# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(stars)
library(raster)
library(terra)
library(tictoc)
library(digest)
library(sf)
library(USAboundaries)



# ============================================================================#
# load LOCA data 
# ============================================================================#
#download monthly data
fr_pr_month_50km <- terra::rast(
  x = "data-raw/loca/ACCESS-CM2/pr/future/month/pr.ACCESS-CM2.ssp245.r1i1p1f1.2015-2044.LOCA_16thdeg_v20220519.monthly.0p5x0p5.nc")
fr_pr_month_50km

fr_pr_month_6km <- terra::rast(
  x = "data-raw/loca/ACCESS-CM2/pr/future/month/pr.ACCESS-CM2.ssp245.r1i1p1f1.2015-2044.LOCA_16thdeg_v20220519.monthly.nc")
fr_pr_month_6km


# md5_from_file <- readLines(
#  "data-raw/loca/ACCESS-CM2/pr/future/month/pr.ACCESS-CM2.ssp245.r1i1p1f1.2015-2044.LOCA_16thdeg_v20220519.monthly.0p5x0p5.nc.md5")
# 
# md5_computed <- digest(file="data-raw/loca/ACCESS-CM2/pr/future/month/pr.ACCESS-CM2.ssp245.r1i1p1f1.2015-2044.LOCA_16thdeg_v20220519.monthly.0p5x0p5.nc",
#                        algo = "md5", file = TRUE)

fr_pr_day_6km <- terra::rast(
  x = "data-raw/loca/ACCESS-CM2/pr/future/day/pr.ACCESS-CM2.ssp245.r1i1p1f1.2015-2044.LOCA_16thdeg_v20220519.nc")
fr_pr_day_6km


##############################################################################
## STEP 0: PLOT DATA
##############################################################################


# ============================================================================#
# prepare plotting data
# ============================================================================#

# load USA boundaries shapefile
usa_boundary <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

# Subset to get the first 5 layers
first_layer <- fr_pr_month_6km[[1]]
second_layer <- fr_pr_month_6km[[2]]
third_layer <- fr_pr_month_6km[[3]]
fourth_layer <- fr_pr_month_6km[[4]]
fifth_layer <- fr_pr_month_6km[[5]]


# Check CRS of the SpatRaster object
print(terra::crs(first_layer))

# Check CRS of the sf object
print(sf::st_crs(usa_boundary))



# Reproject to align CRS 
usa_boundary_reproj <- sf::st_transform(usa_boundary, crs(first_layer))

# shift shapefile longitude from 0-360 to -180 to 180
usa_boundary_reproj$geom <- sf::st_shift_longitude(usa_boundary_reproj$geom)


# ============================================================================#
# plot the first five layers 
# ============================================================================#
terra::plot(first_layer, main = "first layer") 
plot(st_geometry(usa_boundary_reproj), add = TRUE)

terra::plot(second_layer, main = "second layer") 
plot(st_geometry(usa_boundary_reproj), add = TRUE)

terra::plot(third_layer, main = "third layer") 
plot(st_geometry(usa_boundary_reproj), add = TRUE)

terra::plot(fourth_layer, main = "fourth layer") 
plot(st_geometry(usa_boundary_reproj), add = TRUE)

terra::plot(fifth_layer, main = "fifth layer") 
plot(st_geometry(usa_boundary_reproj), add = TRUE)



