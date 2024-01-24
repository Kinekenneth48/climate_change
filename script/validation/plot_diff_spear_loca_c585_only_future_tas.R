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
spear_future_tas <- terra::rast(x = "data-raw/raster/spear_future_r_comb_tas.tif")
loca_future_tas_2044 <- terra::rast(x = "data-raw/raster/loca_future_r_comb_tas_2044_c585.tif")
loca_future_tas_2074 <- terra::rast(x = "data-raw/raster/loca_future_r_comb_tas_2074_c585.tif")
loca_future_tas_2100 <- terra::rast(x = "data-raw/raster/loca_future_r_comb_tas_2100_c585.tif")




################################################################################
## STEP 1: Prepare data
################################################################################
# ============================================================================#
#  filter SPEAR data ro create sub-data(SPEAR) to match LOCA sub-data
# ============================================================================#
# Extract the time attribute
time_values <- terra::time(spear_future_tas)

# Filter to keep layers
filtered_indices_2044 <- which(time_values >= as.Date("2015-01-16") &
  time_values <= as.Date("2044-12-16"))
filtered_indices_2074 <- which(time_values >= as.Date("2045-01-16") &
  time_values <= as.Date("2074-12-16"))
filtered_indices_2100 <- which(time_values >= as.Date("2075-01-16") &
  time_values <= as.Date("2100-12-17"))

# Subset the raster to keep only the filtered layers
spear_future_tas_filtered_2044 <- spear_future_tas[[filtered_indices_2044]]
spear_future_tas_filtered_2074 <- spear_future_tas[[filtered_indices_2074]]
spear_future_tas_filtered_2100 <- spear_future_tas[[filtered_indices_2100]]


# ============================================================================#
#  Create a single file for LOCA (2015-01-16 to 2100-12-16)
# ============================================================================#

# Combine the two SpatRaster objects into one
loca_future_tas_filtered_2044_2100 <- c(
  loca_future_tas_2044,
  loca_future_tas_2074,
  loca_future_tas_2100
)



# ============================================================================#
#  filter SPEAR data to match LOCA's period (2015-01-16 to 2100-12-16)
# ============================================================================#
# Filter to keep layers
filtered_indices_loca <- which(time_values >= as.Date("2015-01-16") &
  time_values <= as.Date("2100-12-17"))

# Subset the raster to keep only the filtered layers
spear_future_tas_filtered_2044_2100 <- spear_future_tas[[filtered_indices_loca]]



################################################################################
## STEP 1: Difference between SPEAR and LOCA from 2015 to 2100
################################################################################

# ============================================================================#
#  Check CRS of both SpatRaster and project if needed
# ============================================================================#
# Check CRS of both SpatRaster
crs_r_fine_agg <- crs(loca_future_tas_filtered_2044_2100)
crs_spear_filtered <- crs(spear_future_tas_filtered_2044_2100)

# Compare CRS
if (crs_r_fine_agg != crs_spear_filtered) {
  warning("The CRS of the two SpatRaster do not match.")

  # Transform one raster to match the CRS of the other
  loca_future_tas_filtered_2044_2100 <- terra::project(
    x = loca_future_tas_filtered_2044_2100,
    y = crs_spear_filtered
  )
} else {
  message("The CRS of both SpatRaster match. You can proceed.")
}


# ============================================================================#
#  Align and resample finer raster to match the coarser raster
# ============================================================================#
# Align the extents by cropping or extending
loca_future_tas_crop <- terra::crop(
  loca_future_tas_filtered_2044_2100,
  terra::ext(spear_future_tas_filtered_2044_2100)
)

# Resample the finer raster to match the coarser raster
loca_future_tas_resampled <- terra::resample(loca_future_tas_crop,
  spear_future_tas_filtered_2044_2100,
  method = "average"
)


# ============================================================================#
#  Take percentage difference of each corresponding raster layer
# ============================================================================#
# Calculate the percentage difference
multiple_per_diff_future_tas_2044_2100 <-
  ((spear_future_tas_filtered_2044_2100 -
    loca_future_tas_resampled) /
    (spear_future_tas_filtered_2044_2100)) * 100

single_per_diff_future_tas_2044_2100 <- terra::app(
  multiple_per_diff_future_tas_2044_2100,
  fun = mean,
  na.rm = TRUE
)



# ============================================================================#
#  set up USA boundaries for plotting
# ============================================================================#

# load USA boundaries shapefile
usa_boundary <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

# Reproject to align CRS
usa_boundary_reproj <- sf::st_transform(
  usa_boundary,
  crs(single_per_diff_future_tas_2044_2100)
)

# shift shapefile longitude from 0-360 to -180 to 180
usa_boundary_reproj$geom <- sf::st_shift_longitude(usa_boundary_reproj$geom)



# ============================================================================#
#  make plot
# ============================================================================#

# Create a sequence of breaks from -3 to 3, with 10 equally spaced breaks
my_breaks <- seq(-2, 2, length.out = 10)
legend_labels <- sprintf(
  "%.1f to %.1f", my_breaks[-length(my_breaks)],
  my_breaks[-1]
)



# Plot raster
plot(crop(single_per_diff_future_tas_2044_2100, usa_extent),
  col = colorRampPalette(brewer.pal(9, "RdYlBu"))(length(my_breaks) - 1),
  breaks = my_breaks, main = "Mean Percentage Difference of Average Surface Temperature between future SPEAR and LOCA (C585 only)",
  plg = list(legend = legend_labels)
)
plot(st_geometry(usa_boundary_reproj), add = TRUE)









################################################################################
## STEP 2: Difference between SPEAR and LOCA from 2015 to 2044
################################################################################


# ============================================================================#
#  Check CRS of both SpatRaster and project if needed
# ============================================================================#
# Check CRS of both SpatRaster
crs_r_fine_agg <- crs(loca_future_tas_2044)
crs_spear_filtered <- crs(spear_future_tas_filtered_2044)

# Compare CRS
if (crs_r_fine_agg != crs_spear_filtered) {
  warning("The CRS of the two SpatRaster do not match.")
  
  # Transform one raster to match the CRS of the other
  loca_future_tas_2044 <- terra::project(
    x = loca_future_tas_2044,
    y = crs_spear_filtered
  )
} else {
  message("The CRS of both SpatRaster match. You can proceed.")
}


# ============================================================================#
#  Align and resample finer raster to match the coarser raster
# ============================================================================#
# Align the extents by cropping or extending
loca_future_tas_crop <- terra::crop(
  loca_future_tas_2044,
  terra::ext(spear_future_tas_filtered_2044)
)

# Resample the finer raster to match the coarser raster
loca_future_tas_resampled <- terra::resample(loca_future_tas_crop,
                                             spear_future_tas_filtered_2044,
                                             method = "average"
)


# ============================================================================#
#  Take percentage difference of each corresponding raster layer
# ============================================================================#
# Calculate the percentage difference
multiple_per_diff_future_tas_2044 <-
  ((spear_future_tas_filtered_2044 -
      loca_future_tas_resampled) /
     (spear_future_tas_filtered_2044)) * 100

single_per_diff_future_tas_2044 <- terra::app(
  multiple_per_diff_future_tas_2044,
  fun = mean,
  na.rm = TRUE
)



# ============================================================================#
#  set up USA boundaries for plotting
# ============================================================================#

# load USA boundaries shapefile
usa_boundary <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

# Reproject to align CRS
usa_boundary_reproj <- sf::st_transform(
  usa_boundary,
  crs(single_per_diff_future_tas_2044)
)

# shift shapefile longitude from 0-360 to -180 to 180
usa_boundary_reproj$geom <- sf::st_shift_longitude(usa_boundary_reproj$geom)



# ============================================================================#
#  make plot
# ============================================================================#

# Create a sequence of breaks from -3 to 3, with 10 equally spaced breaks
my_breaks <- seq(-2, 2, length.out = 10)
legend_labels <- sprintf(
  "%.1f to %.1f", my_breaks[-length(my_breaks)],
  my_breaks[-1]
)



# Plot raster
plot(crop(single_per_diff_future_tas_2044, usa_extent),
     col = colorRampPalette(brewer.pal(9, "RdYlBu"))(length(my_breaks) - 1),
     breaks = my_breaks, main = "Mean Percentage Difference of Average Surface Temperature between future SPEAR and LOCA (2014-2044)- (C585 only)",
     plg = list(legend = legend_labels)
)
plot(st_geometry(usa_boundary_reproj), add = TRUE)









################################################################################
## STEP 3: Difference between SPEAR and LOCA from 2045 to 2074
################################################################################


# ============================================================================#
#  Check CRS of both SpatRaster and project if needed
# ============================================================================#
# Check CRS of both SpatRaster
crs_r_fine_agg <- crs(loca_future_tas_2074)
crs_spear_filtered <- crs(spear_future_tas_filtered_2074)

# Compare CRS
if (crs_r_fine_agg != crs_spear_filtered) {
  warning("The CRS of the two SpatRaster do not match.")
  
  # Transform one raster to match the CRS of the other
  loca_future_tas_2074 <- terra::project(
    x = loca_future_tas_2074,
    y = crs_spear_filtered
  )
} else {
  message("The CRS of both SpatRaster match. You can proceed.")
}


# ============================================================================#
#  Align and resample finer raster to match the coarser raster
# ============================================================================#
# Align the extents by cropping or extending
loca_future_tas_crop <- terra::crop(
  loca_future_tas_2074,
  terra::ext(spear_future_tas_filtered_2074)
)

# Resample the finer raster to match the coarser raster
loca_future_tas_resampled <- terra::resample(loca_future_tas_crop,
                                             spear_future_tas_filtered_2074,
                                             method = "average"
)


# ============================================================================#
#  Take percentage difference of each corresponding raster layer
# ============================================================================#
# Calculate the percentage difference
multiple_per_diff_future_tas_2074 <-
  ((spear_future_tas_filtered_2074 -
      loca_future_tas_resampled) /
     (spear_future_tas_filtered_2074)) * 100

single_per_diff_future_tas_2074 <- terra::app(
  multiple_per_diff_future_tas_2074,
  fun = mean,
  na.rm = TRUE
)



# ============================================================================#
#  set up USA boundaries for plotting
# ============================================================================#

# load USA boundaries shapefile
usa_boundary <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

# Reproject to align CRS
usa_boundary_reproj <- sf::st_transform(
  usa_boundary,
  crs(single_per_diff_future_tas_2074)
)

# shift shapefile longitude from 0-360 to -180 to 180
usa_boundary_reproj$geom <- sf::st_shift_longitude(usa_boundary_reproj$geom)



# ============================================================================#
#  make plot
# ============================================================================#

# Create a sequence of breaks from -3 to 3, with 10 equally spaced breaks
my_breaks <- seq(-2, 2, length.out = 10)
legend_labels <- sprintf(
  "%.1f to %.1f", my_breaks[-length(my_breaks)],
  my_breaks[-1]
)



# Plot raster
plot(crop(single_per_diff_future_tas_2074, usa_extent),
     col = colorRampPalette(brewer.pal(9, "RdYlBu"))(length(my_breaks) - 1),
     breaks = my_breaks, main = "Mean Percentage Difference of Average Surface Temperature between future SPEAR and LOCA (2045-2074)- (C585 only)",
     plg = list(legend = legend_labels)
)
plot(st_geometry(usa_boundary_reproj), add = TRUE)









################################################################################
## STEP 4: Difference between SPEAR and LOCA from 2075 to 2100
################################################################################


# ============================================================================#
#  Check CRS of both SpatRaster and project if needed
# ============================================================================#
# Check CRS of both SpatRaster
crs_r_fine_agg <- crs(loca_future_tas_2100)
crs_spear_filtered <- crs(spear_future_tas_filtered_2100)

# Compare CRS
if (crs_r_fine_agg != crs_spear_filtered) {
  warning("The CRS of the two SpatRaster do not match.")
  
  # Transform one raster to match the CRS of the other
  loca_future_tas_2100 <- terra::project(
    x = loca_future_tas_2100,
    y = crs_spear_filtered
  )
} else {
  message("The CRS of both SpatRaster match. You can proceed.")
}


# ============================================================================#
#  Align and resample finer raster to match the coarser raster
# ============================================================================#
# Align the extents by cropping or extending
loca_future_tas_crop <- terra::crop(
  loca_future_tas_2100,
  terra::ext(spear_future_tas_filtered_2100)
)

# Resample the finer raster to match the coarser raster
loca_future_tas_resampled <- terra::resample(loca_future_tas_crop,
                                             spear_future_tas_filtered_2100,
                                             method = "average"
)


# ============================================================================#
#  Take percentage difference of each corresponding raster layer
# ============================================================================#
# Calculate the percentage difference
multiple_per_diff_future_tas_2100 <-
  ((spear_future_tas_filtered_2100 -
      loca_future_tas_resampled) /
     (spear_future_tas_filtered_2100)) * 100

single_per_diff_future_tas_2100 <- terra::app(
  multiple_per_diff_future_tas_2100,
  fun = mean,
  na.rm = TRUE
)



# ============================================================================#
#  set up USA boundaries for plotting
# ============================================================================#

# load USA boundaries shapefile
usa_boundary <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

# Reproject to align CRS
usa_boundary_reproj <- sf::st_transform(
  usa_boundary,
  crs(single_per_diff_future_tas_2100)
)

# shift shapefile longitude from 0-360 to -180 to 180
usa_boundary_reproj$geom <- sf::st_shift_longitude(usa_boundary_reproj$geom)



# ============================================================================#
#  make plot
# ============================================================================#

# Create a sequence of breaks from -3 to 3, with 10 equally spaced breaks
my_breaks <- seq(-2, 2, length.out = 10)
legend_labels <- sprintf(
  "%.1f to %.1f", my_breaks[-length(my_breaks)],
  my_breaks[-1]
)



# Plot raster
plot(crop(single_per_diff_future_tas_2100, usa_extent),
     col = colorRampPalette(brewer.pal(9, "RdYlBu"))(length(my_breaks) - 1),
     breaks = my_breaks, main = "Mean Difference of Average Surface Temperature between future SPEAR and LOCA (2075-2100)- (C585 only)",
     plg = list(legend = legend_labels)
)
plot(st_geometry(usa_boundary_reproj), add = TRUE)


