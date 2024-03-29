################################################################################
# STEP 1: Initial Setup ---------------------------------
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
pacman:::p_load(fitdistrplus, terra, tidyverse)

source("R/fit_log_normal_less.R")
source("R/fit_log_normal.R")
source("R/fit_gev.R")
source("R/gev_fit_time_varying.R")
source("R/gev_fit_stat_nonstat.R")
source("R/fit_gev_event.R")
source("R/mk.R")


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

# ============================================================================#
# load the data
# ============================================================================#
vic_r45 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp45_access10.tif")
vic_r85 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp85_access10.tif")
vic_hist <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")

loca1_tmean_hist <- terra::rast("E:/data-raw/loca1/access10/hist_tmean_loca1.tif")
loca1_tmean_r45 <- terra::rast("E:/data-raw/loca1/access10/rcp45_tmean_loca1.tif")
loca1_tmean_r85 <- terra::rast("E:/data-raw/loca1/access10/rcp85_tmean_loca1.tif")


diff_event_r45 <- terra::rast("E:data-raw/dist_fit_vic/diff_event_r45.tif")
diff_event_r85 <- terra::rast("E:data-raw/dist_fit_vic/diff_event_r85.tif")

mean_r45 <- mean(vic_r45) - mean(vic_hist)
mean_r85 <- mean(vic_r85) - mean(vic_hist)

plot(mean_r45, breaks = c(-100, -50, 0, 100, 200, 500, 1000), main = "R45")
plot(mean_r85, breaks = c(-100, -50, 0, 100, 200, 500, 1000), main = "R85")

# ============================================================================#
# get coordinates of areas of interest -R45
# ============================================================================#
dev.new(noRStudioGD = TRUE)
plot(diff_event_r45,
  breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
  col = c(
    "#543005", "#8c510a", "#bf812d", "#dfc27d",
    "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
  )
)

terra::click(diff_event_r45, n = 10, xy = TRUE)

lon <- c(-108.5312, -107.6562, -107.0312, -106.7812, -108.2188, -106.1562, -97.84375, -93.65625, -94.03125, -108.6562)
lat <- c(42.03125, 42.46875, 42.09375, 42.84375, 43.03125, 43.46875, 40.28125, 43.71875, 44.21875, 43.65625)

r45_points <- data.frame(lon, lat)


# ============================================================================#
# get coordinates of areas of interest -R85
# ============================================================================#
# first 4 is red
dev.new(noRStudioGD = TRUE)
plot(diff_event_r85,
  breaks = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1, 3),
  col = c(
    "#543005", "#8c510a", "#bf812d", "#dfc27d",
    "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30", "red"
  )
)

terra::click(diff_event_r45, n = 10, xy = TRUE)

lon <- c(-104.9688, -103.5312, -104.5938, -104.2812, -98.46875, -96.53125, -96.40625, -94.21875, -100.2188, -108.3438)
lat <- c(47.59375, 47.59375, 48.15625, 47.96875, 47.15625, 46.34375, 48.15625, 47.03125, 45.28125, 48.78125)

r85_points <- data.frame(lon, lat)


################################################################################
# STEP 1: Site Exploration for SWE ---------------------------------
################################################################################

future_points_r45 <- terra::extract(vic_r45, r45_points)[, -1]
hist_r45 <- terra::extract(vic_hist, r45_points)[, -1]
future_points_r85 <- terra::extract(vic_r85, r85_points)[, -1]
hist_r85 <- terra::extract(vic_hist, r85_points)[, -1]

r45_gridpoint_hist <- (as.vector(unlist(hist_r45[5, ])))
r45_gridpoint_future <- (as.vector(unlist(future_points_r45[5, ])))

summary(r45_gridpoint_hist)
summary(r45_gridpoint_future)

fit_gev_event(r45_gridpoint_hist)
fit_gev_event(r45_gridpoint_future)

plot(as.numeric(r45_gridpoint_hist))
plot(as.numeric(r45_gridpoint_future))



r85_gridpoint_hist <- (as.vector(unlist(hist_r85[5, ])))
r85_gridpoint_future <- (as.vector(unlist(future_points_r85[5, ])))

summary(r85_gridpoint_hist)
summary(r85_gridpoint_future)

fit_gev_event(r85_gridpoint_hist)
fit_gev_event(r85_gridpoint_future)

## ==========================================================================###

# Open the PDF device
pdf("line_plots_R45.pdf", width = 8, height = 6)

# Loop through your data and create and save the smaller plots
for (i in 1:10) {
  data <- c(
    (as.vector(unlist(hist_r45[i, ]))),
    (as.vector(unlist(future_points_r45[i, ])))
  )
  index <- 1:length(data)

  df <- data.frame(Index = index, SWE = data)

  # Plotting with ggplot
  p <- ggplot(df, aes(x = Index, y = SWE)) +
    geom_line() + # Add line plot
    geom_point() + # Add points
    geom_vline(xintercept = 55, color = "red", linetype = "dashed", size = 1) + # Add vertical line at index 55
    theme_minimal() + # Optional: Using a minimal theme for aesthetics
    labs(
      title = "Line Plot for ssp585 R45",
      x = "Index",
      y = "SWE"
    )

  # Print the plot
  print(p)
}

# Close the PDF device
dev.off()



## ==========================================================================###

# Open the PDF device
pdf("line_plots_R85.pdf", width = 8, height = 6) # Adjust the overall PDF dimensions as needed

# Loop through your data and create and save the smaller plots
for (i in 1:10) {
  data <- c(
    (as.vector(unlist(hist_r85[i, ]))),
    (as.vector(unlist(future_points_r85[i, ])))
  )
  index <- 1:length(data)

  df <- data.frame(Index = index, SWE = data)

  # Plotting with ggplot
  p <- ggplot(df, aes(x = Index, y = SWE)) +
    geom_line() + # Add line plot
    geom_point() + # Add points
    geom_vline(xintercept = 55, color = "red", linetype = "dashed", size = 1) + # Add vertical line at index 55
    theme_minimal() + # Optional: Using a minimal theme for aesthetics
    labs(
      title = "Line Plot for ssp585 R85",
      x = "Index",
      y = "SWE"
    )

  # Print the plot
  print(p)
}

# Close the PDF device
dev.off()



################################################################################
# STEP 2: Site Exploration for TEMP ---------------------------------
################################################################################
# Comment:  find a way to find one temp for one season

lon_r45 <- c(
  -108.5312, -107.6562, -107.0312, -106.7812, -108.2188,
  -106.1562, -97.84375, -93.65625, -94.03125, -108.6562
)

lat_r45 <- c(
  42.03125, 42.46875, 42.09375, 42.84375, 43.03125, 43.46875,
  40.28125, 43.71875, 44.21875, 43.65625
)

lon_r85 <- c(
  -104.9688, -103.5312, -104.5938, -104.2812, -98.46875,
  -96.53125, -96.40625, -94.21875, -100.2188, -108.3438
)

lat_r85 <- c(
  47.59375, 47.59375, 48.15625, 47.96875, 47.15625,
  46.34375, 48.15625, 47.03125, 45.28125, 48.78125
)

r45_points <- data.frame(lon_r45, lat_r45)
r85_points <- data.frame(lon_r85, lat_r85)

future_points_r45 <- terra::extract(loca1_tmean_r45, r45_points)[, -1]
hist_r45 <- terra::extract(loca1_tmean_hist, r45_points)[, -1]
future_points_r85 <- terra::extract(loca1_tmean_r85, r85_points)[, -1]
hist_r85 <- terra::extract(loca1_tmean_hist, r85_points)[, -1]

r45_gridpoint_hist <- (as.vector(unlist(hist_r45[5, ])))
r45_gridpoint_future <- (as.vector(unlist(future_points_r45[5, ])))

summary(r45_gridpoint_hist)
summary(r45_gridpoint_future)

fit_gev_event(r45_gridpoint_hist)
fit_gev_event(r45_gridpoint_future)

plot(as.numeric(r45_gridpoint_hist))
plot(as.numeric(r45_gridpoint_future))



r85_gridpoint_hist <- (as.vector(unlist(hist_r85[5, ])))
r85_gridpoint_future <- (as.vector(unlist(future_points_r85[5, ])))

summary(r85_gridpoint_hist)
summary(r85_gridpoint_future)

fit_gev_event(r85_gridpoint_hist)
fit_gev_event(r85_gridpoint_future)



## ==========================================================================###

# Open the PDF device
pdf("line_plots_tmean_R45.pdf", width = 8, height = 6)

# Loop through your data and create and save the smaller plots
for (i in 1:10) {
  data <- c(
    (as.vector(unlist(hist_r45[i, ]))),
    (as.vector(unlist(future_points_r45[i, ])))
  )
  index <- 1:length(data)

  df <- data.frame(Index = index, SWE = data)

  # Plotting with ggplot
  p <- ggplot(df, aes(x = Index, y = SWE)) +
    geom_line() + # Add line plot
    geom_point() + # Add points
    geom_vline(xintercept = 55, color = "red", linetype = "dashed", size = 1) + # Add vertical line at index 55
    theme_minimal() + # Optional: Using a minimal theme for aesthetics
    labs(
      title = "Line Plot for ssp585 R45",
      x = "Index",
      y = "SWE"
    )

  # Print the plot
  print(p)
}

# Close the PDF device
dev.off()



## ==========================================================================###

# Open the PDF device
pdf("line_plots_R85.pdf", width = 8, height = 6) # Adjust the overall PDF dimensions as needed

# Loop through your data and create and save the smaller plots
for (i in 1:10) {
  data <- c(
    (as.vector(unlist(hist_r85[i, ]))),
    (as.vector(unlist(future_points_r85[i, ])))
  )
  index <- 1:length(data)

  df <- data.frame(Index = index, SWE = data)

  # Plotting with ggplot
  p <- ggplot(df, aes(x = Index, y = SWE)) +
    geom_line() + # Add line plot
    geom_point() + # Add points
    geom_vline(xintercept = 55, color = "red", linetype = "dashed", size = 1) + # Add vertical line at index 55
    theme_minimal() + # Optional: Using a minimal theme for aesthetics
    labs(
      title = "Line Plot for ssp585 R85",
      x = "Index",
      y = "SWE"
    )

  # Print the plot
  print(p)
}

# Close the PDF device
dev.off()




## ==========================================================================###

library(dplyr)
library(ggplot2)

tr_h_85 = as.data.frame(t(hist_r85 )) %>%
  mutate(year = 1951:2005 )


tr_f_85 = as.data.frame(t(future_points_r85 )) %>%
  mutate(year = 2006:2100 )

data85 = rbind(tr_h_85,tr_f_85 )

# Convert to long format
df_long <- data85 %>%
  pivot_longer(cols = V1:V4, names_to = "Series", values_to = "SWE")

df_long_adjusted <- df_long %>%
  mutate(Series = case_when(
    Series == "V1" ~ "Location 1",
    Series == "V2" ~ "Location 2",
    Series == "V3" ~ "Location 3",
    Series == "V4" ~ "Location 4",
    TRUE ~ Series # Default case, if needed
  ))



# Assuming df_long is your long-format dataframe ready for plotting
# Make sure it contains 'year', 'Series', and 'Value' columns

ggplot(df_long_adjusted, aes(x = year, y = SWE, group = Series)) + 
  geom_line() + # Draw the lines for each series
  geom_point(size = 1 , colour = "black")+
  facet_wrap(~Series) + # Create separate panels for each series
  geom_vline(xintercept = 2006, color = "red", linetype = "dashed") + # Add the red line at 2006
  theme_minimal() + # Use a minimal theme for a cleaner look
  labs(title = "Multiple Time Series with Facets", x = "Year", y = "SWE") +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 30), # Increase the size of the facet labels
    legend.title = element_blank(), # Assuming you want to hide the legend title
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30),
    panel.spacing = unit(2, "lines") # Adjust space between panels
  )


## ==========================================================================###

library(dplyr)
library(ggplot2)

tr_h_45 = as.data.frame(t(hist_r45 )) %>%
  mutate(year = 1951:2005 )


tr_f_45 = as.data.frame(t(future_points_r45 )) %>%
  mutate(year = 2006:2100 )

data45 = rbind(tr_h_45,tr_f_45 )

# Convert to long format
df_long <- data45 %>%
  pivot_longer(cols = V1:V4, names_to = "Series", values_to = "SWE")

df_long_adjusted <- df_long %>%
  mutate(Series = case_when(
    Series == "V1" ~ "Location 1",
    Series == "V2" ~ "Location 2",
    Series == "V3" ~ "Location 3",
    Series == "V4" ~ "Location 4",
    TRUE ~ Series # Default case, if needed
  ))



# Assuming df_long is your long-format dataframe ready for plotting
# Make sure it contains 'year', 'Series', and 'Value' columns

ggplot(df_long_adjusted, aes(x = year, y = SWE, group = Series)) + 
  geom_line() + # Draw the lines for each series
  geom_point(size = 1 , colour = "black") +
  facet_wrap(~Series) + # Create separate panels for each series
  geom_vline(xintercept = 2006, color = "red", linetype = "dashed") + # Add the red line at 2006
  theme_minimal() + # Use a minimal theme for a cleaner look
  labs(title = "Multiple Time Series with Facets", x = "Year", y = "SWE") +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 30), # Increase the size of the facet labels
    legend.title = element_blank(), # Assuming you want to hide the legend title
    legend.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30),
    panel.spacing = unit(2, "lines") # Adjust space between panels
  )





