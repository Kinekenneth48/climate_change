################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
pacman::p_load(
  fitdistrplus, terra, extRemes, evd, ismev, trend, tidyterra,
  tidyverse
)


source("R/gev_fit_para_mix_stat.R")
source("R/get_rtl.R")
source("R/model_gr.R")
source("R/simulate_loads.R")


Rcpp::sourceCpp("src/simulate_snow.cpp")
Rcpp::sourceCpp("src/gr_functions.cpp")


# manage memory usage so PC doesn't crash
terraOptions(memfrac = 0.80, verbose = TRUE)

# set seed
set.seed(121)

# ============================================================================#
# load the data
# ============================================================================#
vic_r45 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp45_access10.tif")
vic_r85 <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/future/max_swe_vic_rcp85_access10.tif")
vic_hist <- terra::rast("E:/data-raw/NCAR/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")

# 2112 grid cells
vic_r45_high_res <- terra::aggregate(vic_r45, fact = 15, method = "bilinear")

# ==============================================================================
# get USA map with state boundaries
# ==============================================================================

conus <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = T))

conus <- terra::vect(conus)


################################################################################
## STEP 1: Estimate dist parameters
################################################################################
# 5.26 sec elapsed
tictoc::tic()
vic_r45_para_ms <- terra::app(vic_r45_high_res, fun = gev_fit_para_mix_stat, cores = 12)
names(vic_r45_para_ms) <- c("loc", "loctrend", "scale", "shape", "perzero")
tictoc::toc()

################################################################################
## STEP 2: simulate LOADS
################################################################################
gr_data <- read_csv("data-raw/final_gr_models_08042020.csv", show_col_types = FALSE)

gr_model <- model_gr(
  cap = gr_data$cap[8],
  mse = gr_data$sd[8],
  intercept = gr_data$intercept[8],
  slope = gr_data$slope[8],
  lam = gr_data$transform[8],
  flat_line = gr_data$elevation[8]
)

# 30 mins
tictoc::tic()
vic_r45_ms_loads <- terra::app(vic_r45_para_ms,
  fun = simulate_loads, n = 1000000, t = 50, gr = gr_model, cores = 14
)
tictoc::toc()
names(vic_r45_ms_loads) <- c("RT_I", "RT_II", "RT_III", "RT_IV")



################################################################################
## STEP 3: RTL load: kpa convert psf
################################################################################
vic_r45_ms_loads = vic_r45_ms_loads * 20.8855

