library(rsnodas)

download_snodas(
  dates = c("2003-09-01", "2024-31-1"),
  masked = TRUE,
  overwrite = TRUE,
  remove_zip = TRUE,
  data_saved = c("swe"),
  out_dir = paste0(getwd(), "/snodas_data"),
  GTiff = TRUE
)

library(rsnodas)
library(raster)
library(stars)

snodas_wy15 <- download_snodas(
  dates = seq(as.Date("2014-10-01"),
              as.Date("2015-06-30"),
              by = "day"
  ),
  masked = TRUE, remove_zip = TRUE,
  data_saved = c("swe"),
  out_dir = paste0(
    "/Users/kenkin/Library/CloudStorage/OneDrive-USU/RESEARCH CODE/project/UA/data-raw",
    "/snodas_data"
  ),
  GTiff = FALSE
)


# create a list of start and end dates to use in the loop
date_list <- list(
  list(start = "2003-10-01", end = "2004-12-10"),
  list(start = "2014-12-11", end = "2014-12-20"),
  list(start = "2014-12-21", end = "2014-12-30")
)

# create a for loop to download SNODAS data for each time period in the list
for (i in seq_along(date_list)) {
  # extract the start and end dates from the list
  start_date <- as.Date(date_list[[i]]$start)
  end_date <- as.Date(date_list[[i]]$end)
  
  # create a new sequence of dates using the start and end dates
  date_seq <- seq(start_date, end_date, by = "day")
  
  # download SNODAS data for the new date sequence
  snodas_data <- download_snodas(
    dates = date_seq,
    masked = TRUE, remove_zip = TRUE,
    data_saved = c("swe"),
    out_dir = "/Users/kenkin/Library/CloudStorage/OneDrive-USU/RESEARCH CODE/project/UA/data-raw/snodas_data",
    GTiff = FALSE
  )
  
  
  snodas_matrix <- snodas_data[[1]][[1]]
  
  for (i in 2:(length(snodas_data))) {
    # retrieving the min values between the two stars object
    snodas_matrix <- pmax(snodas_matrix, snodas_data[[i]][[1]])
  }
  
  # extract the year from the end date
  year_suffix <- format(end_date, "%Y")
  
  
  # assign name to results
  assign(paste0("snodas_max_", year_suffix), snodas_matrix)
  
  file_name <- paste0("snodas_max_", year_suffix)
  
  # save downloaded data
  save(
    list = paste0("snodas_max_", year_suffix),
    file = paste0("data-raw/RObject/", file_name, ".RData")
  )
}



snodas_wy15 <- download_snodas(
  dates = seq(as.Date("2014-12-01"),
              as.Date("2014-12-30"),
              by = "day"
  ),
  masked = TRUE, remove_zip = TRUE,
  data_saved = c("swe"),
  out_dir = paste0(
    "/Users/kenkin/Library/CloudStorage/OneDrive-USU/RESEARCH CODE/project/UA/data-raw",
    "/snodas_data"
  ),
  GTiff = FALSE
)


snodas_wy15 <- snodas_wy15[[1]][[1]]

for (i in 1:(length(snodas_wy15) - 1)) {
  # retrieving the min values between the two stars object
  empty_vector <- pmax(empty_vector, snodas_wy15[[i]][[1]])
}


# converting the resulting array 'tif_min' into a stars object
empty_vector <- st_as_stars(empty_vector)


#  retrieving the dimensions from one of the two previous
# stars object (here, tif1) and setting a name
st_dimensions(empty_vector) <- st_dimensions(snodas_wy15[[1]])
setNames(empty_vector, "swe_max")

library(raster)

library(stars)



# 1. Creating two stars objects
r1 <- raster(ncols = 3, nrows = 3)
values(r1) <- seq(length(r1))

r2 <- raster(ncols = 3, nrows = 3)
values(r2) <- rev(seq(length(r2)))

r_stack <- stack(r1, r2)
writeRaster(r_stack, "raster.tif",
            bylayer = TRUE, suffix = 1:nlayers(r_stack)
)

tif1 <- read_stars("raster_1.tif")
tif2 <- read_stars("raster_2.tif")

# Array of the first stars object
tif1[[1]]


# Array of the second stars object
tif2[[1]]



# 2. Creating a 'stars' object with the minimum values
# of the two previous stars objects

# 2.1. retrieving the min values between the two stars object
tif_min <- pmin(tif1[[1]], tif2[[1]])

# 2.2. converting the resulting array 'tif_min' into a stars object
tif_min <- st_as_stars(tif_min)

# 2.3. retrieving the dimensions from one of the two previous
# stars object (here, tif1) and setting a name
st_dimensions(tif_min) <- st_dimensions(tif1)
setNames(tif_min, "tif_min")

#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>          Min. 1st Qu. Median     Mean 3rd Qu. Max.
#> tif_min     1       2      3 2.777778       4    5
#> dimension(s):
#>   from to offset delta refsys point values x/y
#> x    1  3   -180   120 WGS 84 FALSE   NULL [x]
#> y    1  3     90   -60 WGS 84 FALSE   NULL [y]

# 2.4 a little check!
tif_min[[1]]
#>      [,1] [,2] [,3]
#> [1,]    1    4    3
#> [2,]    2    5    2
#> [3,]    3    4    1
#>These are the minimum values of the two "star" input objects
