###############################################################################
### STEP 0: INITIAL SETUP
###############################################################################

# ============================================================================#
# load the required R packages and user functions
# ============================================================================#
library(epwshiftr)

source("R/get_cmip6_model_names.R")
source("R/download_cmip6.R")




###############################################################################
### STEP 1: DOWNLOAD CMIP6 data for "MPI-ESM1-2-HR"
###############################################################################
# ============================================================================#
# day -- MPI-ESM1-2-HR
# ============================================================================#
# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "snw", frequency = "day",
  period = "historical"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#
download_cmip6(
  model = "MPI-ESM1-2-HR",
  variable = "snw", frequency = "day",
  period = "historical", directory = "D:/data-raw/cmip6/models"
)



# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "snw", frequency = "day",
  period = "future"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#
download_cmip6(
  model = "MPI-ESM1-2-HR",
  variable = "snw", frequency = "day",
  period = "future",
  directory = "D:/data-raw/cmip6/models"
)





###############################################################################
### STEP 2: DOWNLOAD CMIP6 data for "UKESM1-0-LL"
###############################################################################
# ============================================================================#
# day -- UKESM1-0-LL
# ============================================================================#
# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "snw", frequency = "day",
  period = "historical"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#
download_cmip6(
  model = "UKESM1-0-LL",
  variable = "snw", frequency = "day",
  period = "historical", directory = "D:/data-raw/cmip6/models"
)



# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "snw", frequency = "day",
  period = "future"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#
download_cmip6(
  model = "UKESM1-0-LL",
  variable = "snw", frequency = "day",
  period = "future",
  directory = "D:/data-raw/cmip6/models"
)





###############################################################################
### STEP 3: DOWNLOAD CMIP6 data for "GFDL-ESM4"
###############################################################################
# ============================================================================#
# day -- GFDL-ESM4
# ============================================================================#
# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "snw", frequency = "day",
  period = "historical"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#
download_cmip6(
  model = "GFDL-ESM4",
  variable = "snw", frequency = "day",
  period = "historical", directory = "D:/data-raw/cmip6/models"
)



# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "snw", frequency = "day",
  period = "future"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#
download_cmip6(
  model = "GFDL-ESM4",
  variable = "snw", frequency = "day",
  period = "future",
  directory = "D:/data-raw/cmip6/models"
)
