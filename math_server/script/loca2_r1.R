################################################################################
## STEP 0: INITIAL SETUP
################################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
pacman::p_load(
  fitdistrplus, terra, extRemes, evd, ismev, trend,
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
tif_hist <- rast(list.files("../data-raw/swe/loca2_swe/ssp585/hist/",
                       pattern = "\\.tif$", full.names = TRUE
))

tif_r1 <- rast(list.files("../data-raw/swe/loca2_swe/ssp585/r1/",
                     pattern = "\\.tif$", full.names = TRUE
))

comb_r1 = c(tif_hist, tif_r1)


################################################################################
## STEP 1: Estimate dist parameters
################################################################################


ssp585_r1_para_ms_6km <- terra::app(comb_r1,
                                   fun = gev_fit_para_mix_stat,
                                   cores = 25
)
names(ssp585_r1_para_ms_6km) <- c("loc", "loctrend", "scale", "shape", "perzero")

writeRaster(ssp585_r1_para_ms_6km,
            "../data-raw/swe/loca2_swe/ssp585/ssp585_r1_para_ms_6km.tif",
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


ssp585_r1c_ms_rtl_6km <- terra::app(ssp585_r1_para_ms_6km,
                                  fun = simulate_loads, n = 1000000,
                                  t = 50, gr = gr_model, cores = 25
)
names(ssp585_r1c_ms_rtl_6km) <- c("RT_I", "RT_II", "RT_III", "RT_IV")



################################################################################
## STEP 3: RTL load: kpa convert psf
################################################################################
ssp585_r1c_ms_rtl_6km <- ssp585_r1c_ms_rtl_6km * 20.8855

writeRaster(ssp585_r1c_ms_rtl_6km,
            "../data-raw/swe/loca2_swe/ssp585/ssp585_r1c_ms_rtl_6km.tif",
            overwrite = TRUE
)

