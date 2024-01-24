
# ============================================================================#
# load the required R packages
# ============================================================================#
library(stars)
library(raster)
library(terra)

# ============================================================================#
# load LOCA data
# ============================================================================#


ua_raster <- terra::rast("data-raw/nc/4km_SWE_Depth_WY2004_v01.nc")

# get one layer and change class to raster
ua_raster <- raster(ua_raster[["SWE_1"]])
class(ua_raster)


# ============================================================================#
# load snodas data and create 6km resolution that matches the LOCA resolution
# ============================================================================#

# create a vector of years from 2003 to 2022
years <- 2003:2022


for (year in years) {
  # construct the file path for the data for the current year
  file_path <- paste0("data-raw/RObject/snodas_max_", year, ".RData")
  
  # load the data for the current year
  load(file_path)
  
  # assign file name a generic name called data
  file_name <- basename(file_path)
  file_name <- gsub("\\.RData$", "", file_name)
  assign("df_stars", get(file_name))
  
  # change class to Raster from stars
  df_raster <- raster(as(df_stars, "SpatRaster"))
  
  # Aggregate to 4km resolution by taking the average of each 4x4 block of cells
  r4km_avg <- aggregate(df_raster, fact = 4, fun = base::mean)
  
  # Project the raster to the the UA data projection
  df_raster_4km <- raster::projectRaster(r4km_avg, crs = crs(ua_raster))
  
  # resample df_raster to match ua_raster dimensions and extent
  df_raster_resampled <- raster::resample(df_raster_4km, ua_raster,
                                          method = "ngb"
  )
  
  
  # assign name
  assign(paste0("snodas_max_4km_", year), df_raster_resampled)
  
  file_name_save <- paste0("snodas_max_4km_", year)
  
  # save downloaded data
  save(
    list = paste0("snodas_max_4km_", year),
    file = paste0("data-raw/RObject/", file_name_save, ".RData")
  )
}
