################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
pacman::p_load(
  fitdistrplus, terra, extRemes, evd, ismev, trend, tidyterra,
  tidyverse, Rcpp, pracma
)

gr_data <- read_csv("../data-raw/final_gr_models_08042020.csv",
                    show_col_types = FALSE
)

source("../R/gev_fit_para_mix_stat.R")
source("../R/get_rtl.R")
source("../R/model_gr.R")
source("../R/simulate_loads.R")


Rcpp::sourceCpp("../src/simulate_snow.cpp")
Rcpp::sourceCpp("../src/gr_functions.cpp")


# set seed
set.seed(121)

# ============================================================================#
# load the data
# ============================================================================#
#vic_r45 <- terra::rast("../data-raw/swe/ACCESS1-0/future/max_swe_vic_rcp45_access10.tif")
vic_r85 <- terra::rast("../data-raw/swe/ACCESS1-0/future/max_swe_vic_rcp85_access10.tif")
vic_hist <- terra::rast("../data-raw/swe/ACCESS1-0/historical/max_swe_vic_hist_access10.tif")
smooth_shape <- terra::rast("../data-raw/smooth_shape/vic_smooth_shape.tif")

comb_vic_r85 = c(vic_hist, vic_r85)
#comb_vic_r85_high_res <- terra::aggregate(comb_vic_r85, fact = 5, method = "bilinear")
################################################################################
## STEP 1: Estimate dist parameters
################################################################################


vic_r85c_para_ms_6km_smooth <- terra::app(comb_vic_r85,
                                   fun = gev_fit_para_mix_stat,
                                   cores = 20
)
names(vic_r85c_para_ms_6km_smooth) <- c("loc", "loctrend", "scale", "shape", "perzero")

vic_r85c_para_ms_6km_smooth[["shape"]] = smooth_shape[["shape"]]

writeRaster(vic_r85c_para_ms_6km_smooth,
            "../data-raw/swe/ACCESS1-0/vic_r85c_para_ms_6km_smooth.tif",
            overwrite = TRUE
)

################################################################################
## STEP 2: simulate LOADS
################################################################################

gr_model <- model_gr(
  cap = gr_data$cap[8],
  mse = gr_data$sd[8],
  intercept = gr_data$intercept[8],
  slope = gr_data$slope[8],
  lam = gr_data$transform[8],
  flat_line = gr_data$elevation[8]
)


vic_r85c_ms_rtl_6km_smooth <- terra::app(vic_r85c_para_ms_6km_smooth,
                                  fun = simulate_loads, n = 1000000,
                                  t = 50, gr = gr_model, cores = 20
)
names(vic_r85c_ms_rtl_6km_smooth) <- c("RT_I", "RT_II", "RT_III", "RT_IV")



################################################################################
## STEP 3: RTL load: kpa convert psf
################################################################################
vic_r85c_ms_rtl_6km_smooth <- vic_r85c_ms_rtl_6km_smooth * 20.8855

writeRaster(vic_r85c_ms_rtl_6km_smooth,
            "../data-raw/swe/ACCESS1-0/vic_r85c_ms_rtl_6km_smooth.tif",
            overwrite = TRUE
)
