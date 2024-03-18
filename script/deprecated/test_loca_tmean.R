library(terra)

# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

# Specify the directory containing the .nc files, replace "./" with your directory path if needed
directory_path <- "C:/Users/KENNETH/OneDrive - USU/Desktop/met/ACCESS1-0/historical/"

# List all files that start with 'pr' and end with '.nc'
files_tmin <- list.files(path = directory_path, pattern = "^tasmin.*\\.nc$", full.names = TRUE)
files_tmax <- list.files(path = directory_path, pattern = "^tasmax.*\\.nc$", full.names = TRUE)


hist_list_tmax <- list()
hist_list_tmin <- list()
hist_tmean<- list()

for (i in 1:length(files_tmin)) {
  # Load nc file using terra
  hist_tmin <- terra::rast(x = files_tmin[i])
  hist_tmax <- terra::rast(x = files_tmax[i])

  hist_tmean[[i]] = (hist_tmin+hist_tmax)/2
  gc()
  
  remove(hist_tmin,hist_tmax)
  gc()
}



hist_tmean = rast(hist_tmean)

writeRaster(hist_tmean,
            "data-raw/hist_tmean.tif",
            overwrite = TRUE
)

hist_tmean= rast("data-raw/loca_other/hist_tmean.tif")


dates <- time(hist_tmean)

# Convert dates to months
months <- as.numeric(format(dates, "%m"))

# Identify layers corresponding to winter months (December, January, February)
winter_layers <- which(months %in% c(12, 1, 2, 3))

# Subset the SpatRaster object to keep only winter month layers
loca_hist_tmean_winter <- hist_tmean[[winter_layers]]

loca_hist_tmean_winter_avg <- mean(loca_hist_tmean_winter)

writeRaster(loca_hist_tmean_winter_avg,
  "data-raw/loca_other/loca_hist_tmean_winter_avg.tif",
  overwrite = TRUE
)

#######################################################################

directory_path_r45 <- "D:/data-raw/NCAR/met/ACCESS1-0/rcp45/"


files_tmin_r45 <- list.files(path = directory_path_r45, 
                             pattern = "^tasmin.*\\.nc$", full.names = TRUE)
files_tmax_r45 <- list.files(path = directory_path_r45, 
                             pattern = "^tasmax.*\\.nc$", full.names = TRUE)


tmean_r45_list <- list()

for (i in 1:length(files_tmin_r45)) {
  # Load nc file using terra
  tmin <- terra::rast(x = files_tmin_r45[i])
  tmax <- terra::rast(x = files_tmax_r45[i])
  
  tmean_r45_list[[i]] = (tmin+tmax)/2
  gc()
  
  remove(tmin,tmax)
  gc()
}



r45_tmean = rast(tmean_r45_list)

writeRaster(r45_tmean,
            "E:/data-raw/loca_other/r45_tmean.tif",
            overwrite = TRUE, gdal= "COMPRESS=NONE"
)



dates <- time(r45_tmean)
months <- as.numeric(format(dates, "%m"))

# Identify layers corresponding to winter months (December, January, February)
winter_layers <- which(months %in% c(12, 1, 2, 3))

# Subset the SpatRaster object to keep only winter month layers
loca_r45_tmean_winter <- r45_tmean[[winter_layers]]

loca_r45_tmean_winter_avg <- mean(loca_r45_tmean_winter)


writeRaster(loca_r45_tmean_winter_avg,
  "E:/data-raw/loca_other/loca_r45_tmean_winter_avg.tif",
  overwrite = TRUE
)


#######################################################################

directory_path_r85 <- "D:/data-raw/NCAR/met/ACCESS1-0/rcp85/"


files_tmin_r85 <- list.files(path = directory_path_r85, 
                             pattern = "^tasmin.*\\.nc$", full.names = TRUE)
files_tmax_r85 <- list.files(path = directory_path_r85, 
                             pattern = "^tasmax.*\\.nc$", full.names = TRUE)


tmean_r85_list <- list()

for (i in 1:length(files_tmin_r85)) {
  # Load nc file using terra
tmin <- terra::rast(x = files_tmin_r85[i])
  tmax <- terra::rast(x = files_tmax_r85[i])
  
  tmean_r85_list[[i]] = (tmin + tmax)/2
  gc()
  
  remove(tmin,tmax)
  gc()
}



r85_tmean = rast(tmean_r85_list)

writeRaster(r85_tmean,
            "E:/data-raw/loca_other/r85_tmean.tif",
            overwrite = TRUE
)



dates <- time(r85_tmean)
months <- as.numeric(format(dates, "%m"))

# Identify layers corresponding to winter months (December, January, February)
winter_layers <- which(months %in% c(12, 1, 2, 3))

# Subset the SpatRaster object to keep only winter month layers
loca_r85_tmean_winter <- r85_tmean[[winter_layers]]

loca_r85_tmean_winter_avg <- mean(loca_r85_tmean_winter)


writeRaster(loca_r85_tmean_winter_avg,
            "E:/data-raw/loca_other/loca_r85_tmean_winter_avg.tif",
            overwrite = TRUE
)
