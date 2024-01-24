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
library(ncdump)


# define the extent of usa
usa_extent <- terra::ext(235, 300, 24, 50)


# ============================================================================#
# load data
# ============================================================================#
vic_swe <- terra::rast(x = "E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")
ua_swe <- terra::rast("E:/data-raw/ua/raster/annual/max_swe_ua.tif")


prism = terra::rast("E:/data-raw/prism/raster/prism_day_ppt_raster.tif")[[1]]
prism_mask = project(prism, ua_swe[[1]])


# ============================================================================#
# subset based on matching dates
# ============================================================================#
time_vic <- terra::time(vic_swe)
time_ua <- terra::time(ua_swe)

# Identify dates in time_vic that match the dates in time_ua
matching_dates <- intersect(time_vic, time_ua)


# Subset vic_swe & ua_swe using the matching dates
vic_swe <- subset(vic_swe, time_vic >= matching_dates[1] &
  time_vic <= matching_dates[length(matching_dates)])

ua_swe <- subset(ua_swe, time_ua >= matching_dates[1] &
  time_ua <= matching_dates[length(matching_dates)])


################################################################################
## STEP 1: Plot diff between UA and VIC
################################################################################

# ============================================================================#
# Take the difference
# ============================================================================#
error_layers <- ua_swe - vic_swe


# ============================================================================#
# Mask the error layers for US
# ============================================================================#
cropped_error_layers = mask(x=error_layers, mask=prism_mask)

# ============================================================================#
# Plot
# ============================================================================#

# Subset the raster based on the condition
subset_d <- clamp(cropped_error_layers[[1]], lower = -50, upper = 50)

# Plot the subset raster
plot(subset_d)

# Specify the file name for the PDF
pdf("plots.pdf", width = 8, height = 6)  # Adjust the overall PDF dimensions as needed

# Loop through your data and create and save the smaller plots
for (i in 1:24) {
  # Create a smaller plot for cropped_error_layers[[i]]
  par(mar = c(5, 4, 4, 2))  # Adjust margin as needed (top, right, bottom, left)
  plot(cropped_error_layers[[i]], breaks = c(-500,-400,-300,-200,-100, -80, -60, 
                              -40, -20, 0, 20, 40, 60, 80,100,200,300,400, 500), 
       width = 2, height = 2)  # Adjust width and height as needed
  
  # Optionally, add a title or other customizations here
  
  # Save the current plot to the PDF
}

# Close the PDF file
dev.off()



plot(cropped_error_layers[[3]], breaks=c(-500,-80,-60,-40,-20,0,20, 40,60,80,500))
# ============================================================================#
# RMSE MAE
# ============================================================================#
rmse = vector("numeric", length = 0)
mae = vector("numeric", length = 0)

for (i in 1:nlyr(cropped_error_layers)) {
  rmse[i] = sqrt(mean((values(cropped_error_layers[[i]]))^2, na.rm = TRUE))
  mae[i] = mean(abs(values(cropped_error_layers[[i]])), na.rm = TRUE)
}

mae = mean(mae)
rmse= mean(rmse)


