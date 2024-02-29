
library(terra)



# Specify the directory containing the .nc files, replace "./" with your directory path if needed
directory_path <- "D:/data-raw/NCAR/met/ACCESS1-0/historical/"

# List all files that start with 'pr' and end with '.nc'
files <- list.files(path = directory_path, pattern = "^pr.*\\.nc$", full.names = TRUE)

hist_list <- list()

for (i in 1:length(files)) {
  # Load nc file using terra
  hist_list[[i]] <- terra::rast(x = files[i])
  
}

hist =rast(hist_list)

dates <- time(hist)

# Convert dates to months
months <- as.numeric(format(dates, "%m"))

# Identify layers corresponding to winter months (December, January, February)
winter_layers <- which(months %in% c(12, 1, 2, 3))

# Subset the SpatRaster object to keep only winter month layers
loca_hist_pr_winter <- hist[[winter_layers]]

loca_hist_pr_winter_avg = mean(loca_hist_pr_winter)
loca_hist_pr_winter_avg =loca_hist_pr_winter_avg

writeRaster(loca_hist_pr_winter_avg,
            "data-raw/loca_other/loca_hist_pr_winter_avg.tif",
            overwrite = TRUE
)

#######################################################################

directory_path_r45 <- "D:/data-raw/NCAR/met/ACCESS1-0/rcp45/"

files_r45 <- list.files(path = directory_path_r45, 
                    pattern = "^pr.*\\.nc$", full.names = TRUE)


r45_list <- list()

for (i in 1:length(files_r45)) {
  # Load nc file using terra
  r45_list[[i]] <- terra::rast(x = files_r45[i])
  
}

r45_pr =rast(r45_list)


dates <- time(r45_pr)
months <- as.numeric(format(dates, "%m"))

# Identify layers corresponding to winter months (December, January, February)
winter_layers <- which(months %in% c(12, 1, 2, 3))

# Subset the SpatRaster object to keep only winter month layers
loca_r45_pr_winter <- r45_pr[[winter_layers]]

loca_r45_pr_winter_avg = mean(loca_r45_pr_winter)
loca_r45_pr_winter_avg =loca_r45_pr_winter_avg

writeRaster(loca_r45_pr_winter_avg,
            "data-raw/loca_other/loca_r45_pr_winter_avg.tif",
            overwrite = TRUE
)


#######################################################################

directory_path_r85 <- "D:/data-raw/NCAR/met/ACCESS1-0/rcp85/"

files_r85 <- list.files(path = directory_path_r85, 
                        pattern = "^pr.*\\.nc$", full.names = TRUE)


r85_list <- list()

for (i in 1:length(files_r85)) {
  # Load nc file using terra
  r85_list[[i]] <- terra::rast(x = files_r85[i])
  
}

r85_pr =rast(r85_list)


dates <- time(r85_pr)
months <- as.numeric(format(dates, "%m"))

# Identify layers corresponding to winter months (December, January, February)
winter_layers <- which(months %in% c(12, 1, 2, 3))

# Subset the SpatRaster object to keep only winter month layers
loca_r85_pr_winter <- r85_pr[[winter_layers]]

loca_r85_pr_winter_avg = mean(loca_r85_pr_winter)
loca_r85_pr_winter_avg =loca_r85_pr_winter_avg* 86400

writeRaster(loca_r85_pr_winter,
            "data-raw/loca_other/loca_r85_pr_winter.tif",
            overwrite = TRUE
)

