library(terra)
library(future)
library(future.apply)
library(furrr)
library(lubridate)

source("R/process_directory_nc.R")
source("R/calculate_annual_max.R")

# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)



# ============================================================================#
#  R1
# ============================================================================#
# Set the path to the parent directory containing the folders of NetCDF files
parent_directory_r1 <- "D:/data-raw/canada_model/snd/NAM-44_CCCma-CanESM2_historical-r1"

# List the subdirectories
subdirs_r1 <- list.dirs(parent_directory_r1, full.names = TRUE, recursive = FALSE)

r1_raster <- list()


for (dir in subdirs_r1) {
  r1_raster[length(r1_raster) + 1] <- process_directory_nc(dir)
}

s <- terra::sds(r1_raster)
r1_raster <- app(s, mean)

terra::writeRaster(r1_raster,
  filename = "E:/data-raw/canada_model/raster/snd/r1/r1_raster.tif",
  overwrite = TRUE
)


# ============================================================================#
#  R2
# ============================================================================#
parent_directory_r2 <- "D:/data-raw/canada_model/snd/NAM-44_CCCma-CanESM2_historical-r2"
subdirs_r2 <- list.dirs(parent_directory_r2, full.names = TRUE, recursive = FALSE)
r2_raster <- list()

for (dir in subdirs_r2) {
  r2_raster[length(r2_raster) + 1] <- process_directory_nc(dir)
}

s2 <- terra::sds(r2_raster)
r2_raster <- app(s2, mean)

terra::writeRaster(r2_raster,
  filename = "E:/data-raw/canada_model/raster/snd/r2/r2_raster.tif",
  overwrite = TRUE
)
# ============================================================================#
#  R3
# ============================================================================#
parent_directory_r3 <- "D:/data-raw/canada_model/snd/NAM-44_CCCma-CanESM2_historical-r3"
subdirs_r3 <- list.dirs(parent_directory_r3, full.names = TRUE, recursive = FALSE)
r3_raster <- list()

for (dir in subdirs_r3) {
  r3_raster[length(r3_raster) + 1] <- process_directory_nc(dir)
}

s3 <- terra::sds(r3_raster)
r3_raster <- app(s3, mean)

terra::writeRaster(r3_raster,
  filename = "E:/data-raw/canada_model/raster/snd/r3/r3_raster.tif",
  overwrite = TRUE
)

# ============================================================================#
#  R4
# ============================================================================#
parent_directory_r4 <- "D:/data-raw/canada_model/snd/NAM-44_CCCma-CanESM2_historical-r4"
subdirs_r4 <- list.dirs(parent_directory_r4, full.names = TRUE, recursive = FALSE)
r4_raster <- list()

for (dir in subdirs_r4) {
  r4_raster[length(r4_raster) + 1] <- process_directory_nc(dir)
}

s4 <- terra::sds(r4_raster)
r4_raster <- app(s4, mean)

terra::writeRaster(r4_raster,
  filename = "E:/data-raw/canada_model/raster/snd/r4/r4_raster.tif",
  overwrite = TRUE
)


# ============================================================================#
#  R5
# ============================================================================#
parent_directory_r5 <- "D:/data-raw/canada_model/snd/NAM-44_CCCma-CanESM2_historical-r5"
subdirs_r5 <- list.dirs(parent_directory_r5, full.names = TRUE, recursive = FALSE)
r5_raster <- list()

for (dir in subdirs_r5) {
  r5_raster[length(r5_raster) + 1] <- process_directory_nc(dir)
}

s5 <- terra::sds(r5_raster)
r5_raster <- app(s5, mean)

terra::writeRaster(r5_raster,
  filename = "E:/data-raw/canada_model/raster/snd/r5/r5_raster.tif",
  overwrite = TRUE
)
