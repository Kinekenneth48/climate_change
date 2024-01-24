library(terra)

# Load your shapefile and raster
ecoregions <-terra::vect("data-raw/eco_regions/NA_CEC_Eco_Level3.shp")
target_raster <- terra::rast("data-raw/ua/raster/combined/ua_swe_combined_daily_1982_2014_for_loca_res.tif")[[1]]

# Align CRS of high_res_raster with ecoregions if needed
high_res_raster <- terra::project(target_raster, crs(ecoregions))

# Rasterize the ecoregions, using the 'NA_L3CODE' attribute for the raster values
#Transfer values associated with the geometries of vector data to a raster
rasterized_ecoregions_L1 <- terra::rasterize(ecoregions, high_res_raster, field="NA_L1CODE")
rasterized_ecoregions_L2 <- terra::rasterize(ecoregions, high_res_raster, field="NA_L2CODE")
rasterized_ecoregions_L3 <- terra::rasterize(ecoregions, high_res_raster, field="NA_L3CODE")

# Reproject the ecoregion raster to match the CRS of the target raster
reprojected_ecoregions_L1 <- terra::project(rasterized_ecoregions_L1, crs(target_raster))
reprojected_ecoregions_L2 <- terra::project(rasterized_ecoregions_L2, crs(target_raster))
reprojected_ecoregions_L3 <- terra::project(rasterized_ecoregions_L3, crs(target_raster))

# Resample the reprojected ecoregion raster to match the resolution of the target raster
ecoregions_L1 <- terra::resample(reprojected_ecoregions_L1, 
                                           target_raster, method="near")
ecoregions_L2 <- terra::resample(reprojected_ecoregions_L2,
                                           target_raster, method="near")
ecoregions_L3 <- terra::resample(reprojected_ecoregions_L3,
                                           target_raster, method="near")


# Save the output
terra::writeRaster(ecoregions_L1,
                   filename = "data-raw/eco_regions/ecoregions_L1.tif",
                   overwrite = TRUE
)

terra::writeRaster(ecoregions_L2,
                   filename = "data-raw/eco_regions/ecoregions_L2.tif",
                   overwrite = TRUE
)

terra::writeRaster(ecoregions_L3,
                   filename = "data-raw/eco_regions/ecoregions_L3.tif",
                   overwrite = TRUE
)
