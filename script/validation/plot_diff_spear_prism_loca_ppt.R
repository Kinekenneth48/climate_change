################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(stars)
library(raster)
library(terra)
library(tictoc)
library(RColorBrewer)

# define the extent of usa
usa_extent <- terra::ext(235, 300, 24, 50)



# ============================================================================#
# load data
# ============================================================================#
spear_hist_ppt <- terra::rast(x = "data-raw/raster/spear_hist_r_comb_pr.tif")
loca_hist_ppt <- terra::rast(x = "data-raw/raster/loca_hist_r_comb_pr.tif")
prism_hist_ppt <- terra::rast(x = "data-raw/prism/raster/prism_monthly_ppt_raster.tif")

# convert from kg m2 s to mm per month
loca_hist_ppt <- loca_hist_ppt * 2592000
spear_hist_ppt <- spear_hist_ppt * 2592000

################################################################################
## STEP 1: Prepare data
################################################################################
# ============================================================================#
#  filter PRSIM data to start from 1950 to match start date of LOCA
# ============================================================================#

# Subset the raster to keep only the filtered layers from 1950 to 2014
prism_hist_ppt_for_loca <- prism_hist_ppt[[361:1140]]

# Create a new SpatExtent object with the adjusted coordinates
ext(prism_hist_ppt_for_loca) <- terra::ext(235, 295, 24, 50)
ext(loca_hist_ppt) <- terra::ext(235, 295, 24, 50)

# ============================================================================#
#  Check CRS of both SpatRaster and project if needed:LOCA and PRISM
# ============================================================================#

# Check CRS of both SpatRasters and project if needed: LOCA and PRISM
crs_loca <- terra::crs(loca_hist_ppt)
crs_prism <- terra::crs(prism_hist_ppt_for_loca)


# Transform one raster to match the CRS of the other
prism_hist_ppt_for_loca <- terra::project(
  x = prism_hist_ppt_for_loca,
  y = loca_hist_ppt,
  method = "average", mask = TRUE,
  align = TRUE
)



# Get the extent of the rasters
ext_loca <- ext(loca_hist_ppt)
ext_prism <- ext(prism_hist_ppt_for_loca)


# Now, you can crop or resample to match the exact extent of loca_hist_ppt
prism_hist_ppt_for_loca <- terra::crop(prism_hist_ppt_for_loca, ext_loca)






# ============================================================================#
#  filter PRSIM data to start from 1950 to match start date of SPEAR
# ============================================================================#

# Subset the raster to keep only the filtered layers from 1921 to 2014
prism_hist_ppt_for_spear <- prism_hist_ppt[[13:1140]]

# Create a new SpatExtent object with the adjusted coordinates
ext(prism_hist_ppt_for_spear) <- terra::ext(235, 295, 24, 50)
ext(spear_hist_ppt) <- terra::ext(235, 295, 24, 50)


# ============================================================================#
#  Check CRS of both SpatRaster and project if needed:SPEAR and PRISM
# ============================================================================#

# Transform one raster to match the CRS of the other
prism_hist_ppt_for_spear <- terra::project(
  x = prism_hist_ppt_for_spear,
  y = spear_hist_ppt, method = "average", mask = TRUE, align = TRUE,
  threads = TRUE
)



# Get the extent of the rasters
ext_spear <- ext(spear_hist_ppt)
ext_prism <- ext(prism_hist_ppt_for_spear)


# Now, you can crop or resample to match the exact extent of spear_hist_ppt
prism_hist_ppt_for_spear <- terra::crop(prism_hist_ppt_for_spear, ext_spear)



################################################################################
## STEP 2: comp
################################################################################



# ============================================================================#
#  Take mean difference of each corresponding raster layer:PRSIM and SPEAR
# ============================================================================#
# Calculate the mean difference
multiple_per_diff_prism_spear <- (prism_hist_ppt_for_spear - spear_hist_ppt)

single_per_diff_prism_spear <- terra::app(multiple_per_diff_prism_spear,
  fun = mean,
  na.rm = TRUE
)



# ============================================================================#
#  Take mean  difference of each corresponding raster layer:PRISM and LOCA
# ============================================================================#
# Calculate the percentage difference
multiple_per_diff_prism_loca <- (prism_hist_ppt_for_loca - loca_hist_ppt)

single_per_diff_prism_loca <- terra::app(multiple_per_diff_prism_loca,
  fun = mean,
  na.rm = TRUE
)





################################################################################
## STEP 4: plot raster : spear and prism
################################################################################

# ============================================================================#
#  set up USA boundaries for plotting
# ============================================================================#

# load USA boundaries shapefile
usa_boundary <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))


# Convert the data frame to an sf object
usa_boundary <- st_as_sf(usa_boundary, coords = c("long", "lat"), crs = 4326, region = "id")


# Reproject to align CRS
usa_boundary_reproj <- sf::st_transform(
  usa_boundary,
  crs(single_per_diff_prism_spear)
)

# shift shapefile longitude from 0-360 to -180 to 180
usa_boundary_reproj$geom <- sf::st_shift_longitude(usa_boundary_reproj$geom)



# ============================================================================#
#  make plot
# ============================================================================#

# Create a sequence of breaks from -3 to 3, with 10 equally spaced breaks
my_breaks <- seq(-100, 100, by = 20)
legend_labels <- sprintf(
  "%.1f to %.1f", my_breaks[-length(my_breaks)],
  my_breaks[-1]
)



# Plot raster
plot(single_per_diff_prism_spear,
  col = colorRampPalette(brewer.pal(9, "RdYlBu"))(length(my_breaks) - 1),
  breaks = my_breaks, main = "Mean Difference of Monthly Precipitation between PRISM and SPEAR:1921 to 2014(in mm per month)",
  plg = list(legend = legend_labels)
)
plot(st_geometry(usa_boundary_reproj), add = TRUE)





################################################################################
## STEP 4: plot raster : loca and prism
################################################################################

# ============================================================================#
#  set up USA boundaries for plotting
# ============================================================================#

# load USA boundaries shapefile
usa_boundary <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))


# Convert the data frame to an sf object
usa_boundary <- st_as_sf(usa_boundary, coords = c("long", "lat"), crs = 4326, region = "id")


# Reproject to align CRS
usa_boundary_reproj <- sf::st_transform(
  usa_boundary,
  crs(single_per_diff_prism_loca)
)

# shift shapefile longitude from 0-360 to -180 to 180
usa_boundary_reproj$geom <- sf::st_shift_longitude(usa_boundary_reproj$geom)



# ============================================================================#
#  make plot
# ============================================================================#

# Create a sequence of breaks from -3 to 3, with 10 equally spaced breaks
my_breaks <- seq(-50, 50, by = 10)
legend_labels <- sprintf(
  "%.1f to %.1f", my_breaks[-length(my_breaks)],
  my_breaks[-1]
)



# Plot raster
plot(single_per_diff_prism_loca,
  col = colorRampPalette(brewer.pal(9, "RdYlBu"))(length(my_breaks) - 1),
  breaks = my_breaks, main = "Mean Difference of Monthly Precipitation between PRISM and LOCA:1950 to 2014(in mm per month)",
  plg = list(legend = legend_labels)
)
plot(st_geometry(usa_boundary_reproj), add = TRUE)
