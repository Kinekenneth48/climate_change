################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(terra)
library(future)
library(future.apply)
library(furrr)
library(lubridate)

library(fitdistrplus)
library(terra)
library(extRemes)
library(evd)
library(ismev)
library(trend)
# 
# source("R/fit_log_normal_less.R")
# source("R/fit_log_normal.R")
# source("R/fit_gev.R")
# source("R/gev_fit_time_varying.R")
# source("R/gev_fit_stat_nonstat.R")
# source("R/fit_gev_event.R")
# source("R/mk.R")
# source("R/fit_gev_shape.R")
# source("R/rl_gev_time_data_w_shape.R")

source("R/process_directory_tif.R")
source("R/calculate_annual_max.R")

# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

################################################################################
## STEP 1: AVERAGE THE SIMULATION INTO A INDVIVIDAUL RASTER
################################################################################

# ============================================================================#
#  R1
# ============================================================================#
# Set the path to the parent directory containing the folders of NetCDF files
parent_directory_r1 <- "D:/data-raw/canada_model/snw/warp_mask/NAM-44_CCCma-CanESM2_historical-r1"

# List the subdirectories
subdirs_r1 <- list.dirs(parent_directory_r1, full.names = TRUE, recursive = FALSE)

r1_raster <- list()


for (dir in subdirs_r1) {
  r1_raster[length(r1_raster) + 1] <- process_directory_tif(dir)
}

s <- terra::sds(r1_raster)
r1_raster <- app(s, mean)

terra::writeRaster(r1_raster,
                   filename = "E:/data-raw/canada_model/raster/snw/r1/r1_raster.tif",
                   overwrite = TRUE
)


# ============================================================================#
#  R2
# ============================================================================#
parent_directory_r2 <- "D:/data-raw/canada_model/snw/warp_mask/NAM-44_CCCma-CanESM2_historical-r2"
subdirs_r2 <- list.dirs(parent_directory_r2, full.names = TRUE, recursive = FALSE)
r2_raster <- list()

for (dir in subdirs_r2) {
  r2_raster[length(r2_raster) + 1] <- process_directory_tif(dir)
}

s2 <- terra::sds(r2_raster)
r2_raster <- app(s2, mean)

terra::writeRaster(r2_raster,
                   filename = "E:/data-raw/canada_model/raster/snw/r2/r2_raster.tif",
                   overwrite = TRUE
)
# ============================================================================#
#  R3
# ============================================================================#
parent_directory_r3 <- "D:/data-raw/canada_model/snw/warp_mask/NAM-44_CCCma-CanESM2_historical-r3"
subdirs_r3 <- list.dirs(parent_directory_r3, full.names = TRUE, recursive = FALSE)
r3_raster <- list()

for (dir in subdirs_r3) {
  r3_raster[length(r3_raster) + 1] <- process_directory_tif(dir)
}

s3 <- terra::sds(r3_raster)
r3_raster <- app(s3, mean)

terra::writeRaster(r3_raster,
                   filename = "E:/data-raw/canada_model/raster/snw/r3/r3_raster.tif",
                   overwrite = TRUE
)

# ============================================================================#
#  R4
# ============================================================================#
parent_directory_r4 <- "D:/data-raw/canada_model/snw/warp_mask/NAM-44_CCCma-CanESM2_historical-r4"
subdirs_r4 <- list.dirs(parent_directory_r4, full.names = TRUE, recursive = FALSE)
r4_raster <- list()

for (dir in subdirs_r4) {
  r4_raster[length(r4_raster) + 1] <- process_directory_tif(dir)
}

s4 <- terra::sds(r4_raster)
r4_raster <- app(s4, mean)

terra::writeRaster(r4_raster,
                   filename = "E:/data-raw/canada_model/raster/snw/r4/r4_raster.tif",
                   overwrite = TRUE
)


# ============================================================================#
#  R5
# ============================================================================#
parent_directory_r5 <- "D:/data-raw/canada_model/snw/warp_mask/NAM-44_CCCma-CanESM2_historical-r5"
subdirs_r5 <- list.dirs(parent_directory_r5, full.names = TRUE, recursive = FALSE)
r5_raster <- list()

for (dir in subdirs_r5) {
  r5_raster[length(r5_raster) + 1] <- process_directory_tif(dir)
}

s5 <- terra::sds(r5_raster)
r5_raster <- app(s5, mean)

terra::writeRaster(r5_raster,
                   filename = "E:/data-raw/canada_model/raster/snw/r5/r5_raster.tif",
                   overwrite = TRUE
)



################################################################################
## STEP 2: CREATE ONE RASTER FROM INDVIVIDAUL RASTERS
################################################################################
r1_raster <- terra::rast("E:/data-raw/canada_model/raster/snw/r1/r1_raster.tif")
r2_raster <- terra::rast("E:/data-raw/canada_model/raster/snw/r2/r2_raster.tif")
r3_raster <- terra::rast("E:/data-raw/canada_model/raster/snw/r3/r3_raster.tif")
r4_raster <- terra::rast("E:/data-raw/canada_model/raster/snw/r4/r4_raster.tif")
r5_raster <- terra::rast("E:/data-raw/canada_model/raster/snw/r5/r5_raster.tif")
zero_mask = rast("E:/data-raw/canada_model/zero_mask.tif")

raster <- mean(r1_raster, r2_raster, r3_raster, r4_raster, r5_raster)
raster = crop(raster, zero_mask, mask =TRUE)

hist <- subset(raster, time(raster) <= 2005)
future <- subset(raster, time(raster) > 2005)

hist_mean <- mean(hist, na.rm = TRUE) 
future_mean <- mean(future, na.rm = TRUE)

# zero_mask  <- ifel(hist_mean == 0, NA, hist_mean)
# terra::writeRaster(zero_mask,
#                    filename = "E:/data-raw/canada_model/zero_mask.tif",
#                    overwrite = TRUE
# )
# 
# future_mean <- ifel(future_mean == 0, NA, future_mean)
# ============================================================================#
# create a mask
# ============================================================================#
usa_extent <- terra::ext(-34.1, 34.1, -20, 5)
hist_mean_r <- terra::crop(hist_mean, usa_extent)
future_mean_r <- terra::crop(future_mean, usa_extent)


diff <- future_mean_r - hist_mean_r

plot(diff,
     breaks = c( -500, -250, -100, -50, -25, 0 , 1),
     col = c("#543005","#8c510a","#bf812d","#dfc27d","#f6e8c3","#80cdc1"),
     main = "Difference in mean snow amount (Canada): Hindcast vs Future"
)

