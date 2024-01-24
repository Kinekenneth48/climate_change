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
spear_hist_pr <- terra::rast(x = "data-raw/raster/spear_hist_r_comb_pr.tif")
loca_hist_pr <- terra::rast(x = "data-raw/raster/loca_hist_r_comb_pr.tif")



################################################################################
## STEP 1: Prepare data
################################################################################
# ============================================================================#
#  filter SPEAR data to start from 1950 to match start date of LOCA
# ============================================================================#
# Extract the time attribute
time_values <- terra::time(spear_hist_pr)

# Filter to keep layers where the time is greater than or equal to the start of 1950
filtered_indices <- which(time_values >= as.Date("1950-01-01"))

# Subset the raster to keep only the filtered layers
spear_hist_pr_filtered <- spear_hist_pr[[filtered_indices]]


# ============================================================================#
#  Check CRS of both SpatRaster and project if needed
# ============================================================================#
# Check CRS of both SpatRaster
crs_r_fine_agg <- crs(loca_hist_pr)
crs_spear_hist_pr_filtered <- crs(spear_hist_pr_filtered)

# Compare CRS
if (crs_r_fine_agg != crs_spear_hist_pr_filtered) {
  warning("The CRS of the two SpatRaster do not match.")

  # Transform one raster to match the CRS of the other
  loca_hist_pr <- terra::project(
    x = loca_hist_pr,
    y = crs_spear_hist_pr_filtered
  )
} else {
  message("The CRS of both SpatRaster match. You can proceed.")
}


# ============================================================================#
#  Align and resample finer raster to match the coarser raster
# ============================================================================#
# Align the extents by cropping or extending
extent_to_match <- terra::ext(spear_hist_pr_filtered)
loca_hist_pr_crop <- terra::crop(loca_hist_pr, extent_to_match)

# Resample the finer raster to match the coarser raster
loca_hist_pr_resampled <- resample(loca_hist_pr_crop, spear_hist_pr_filtered,
  method = "average"
)


# ============================================================================#
#  Take percentage difference of each corresponding raster layer
# ============================================================================#
# Calculate the percentage difference
multiple_per_diff_hist_pr <- (spear_hist_pr_filtered - loca_hist_pr_resampled) * 86400

single_per_diff_hist_pr <- terra::app(multiple_per_diff_hist_pr, fun = mean, 
                                       na.rm = TRUE)





################################################################################
## STEP 2: plot raster
################################################################################

# ============================================================================#
#  set up USA boundaries for plotting
# ============================================================================#

# load USA boundaries shapefile
usa_boundary <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

# Reproject to align CRS 
usa_boundary_reproj <- sf::st_transform(
  usa_boundary,
  crs(single_per_diff_hist_pr)
)

# shift shapefile longitude from 0-360 to -180 to 180
usa_boundary_reproj$geom <- sf::st_shift_longitude(usa_boundary_reproj$geom)



# ============================================================================#
#  make plot
# ============================================================================#

# Create a sequence of breaks from -3 to 3, with 10 equally spaced breaks
my_breaks <- seq(-2, 2, by = 0.5)
legend_labels <- sprintf(
  "%.1f to %.1f", my_breaks[-length(my_breaks)],
  my_breaks[-1]
)



# Plot raster
plot(crop(single_per_diff_hist_pr, usa_extent),
  col = colorRampPalette(brewer.pal(9, "RdYlBu"))(length(my_breaks) - 1),
  breaks = my_breaks, main = "Mean Difference of Monthly Precipitatin between historical SPEAR and LOCA (mm per day)",
  plg = list(legend = legend_labels)
)
plot(st_geometry(usa_boundary_reproj), add = TRUE)



# Save or analyze the resulting raster
#writeRaster(single_per_diff_hist, "single_per_diff_hist.tif", overwrite = TRUE)
