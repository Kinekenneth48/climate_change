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
### STEP 1: DOWNLOAD CMIP6 data for "ACCESS-CM2"
###############################################################################
# ============================================================================#
# day -- ACCESS-CM2
# ============================================================================#
# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "pr", frequency = "day",
  period = "historical"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#
download_cmip6(
  model = "ACCESS-CM2",
  variable = "pr", frequency = "day",
  period = "historical", directory = "D:/data-raw/cmip6/models"
)



# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "pr", frequency = "day",
  period = "future"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#
download_cmip6(
  model = "ACCESS-CM2",
  variable = "pr", frequency = "day",
  period = "future",
  directory = "D:/data-raw/cmip6/models"
)



# ============================================================================#
# month -- ACCESS-CM2
# ============================================================================#
# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "pr", frequency = "mon",
  period = "historical"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#
download_cmip6(
  model = "ACCESS-CM2",
  variable = "pr", frequency = "mon",
  period = "historical", directory = "D:/data-raw/cmip6/models"
)



# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "pr", frequency = "mon",
  period = "future"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#

download_cmip6(
  model = "ACCESS-CM2",
  variable = "pr", frequency = "mon",
  period = "future",
  directory = "D:/data-raw/cmip6/models"
)






###############################################################################
### STEP 2: DOWNLOAD CMIP6 data for "ACCESS-ESM1-5"
###############################################################################
# ============================================================================#
# day -- ACCESS-ESM1-5
# ============================================================================#
# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "pr", frequency = "day",
  period = "historical"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#
download_cmip6(
  model = "ACCESS-ESM1-5",
  variable = "pr", frequency = "day",
  period = "historical", directory = "D:/data-raw/cmip6/models"
)



# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "pr", frequency = "day",
  period = "future"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#
download_cmip6(
  model = "ACCESS-ESM1-5",
  variable = "pr", frequency = "day",
  period = "future",
  directory = "D:/data-raw/cmip6/models"
)



# ============================================================================#
# month -- ACCESS-ESM1-5
# ============================================================================#
# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "pr", frequency = "mon",
  period = "historical"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#
download_cmip6(
  model = "ACCESS-ESM1-5",
  variable = "pr", frequency = "mon",
  period = "historical", directory = "D:/data-raw/cmip6/models"
)



# ============================================================================#
# get the model names associated with the variable, frequency, and
# period provided
# ============================================================================#
model_names <- get_cmip6_model_names(
  variable = "pr", frequency = "mon",
  period = "future"
)


# ============================================================================#
# download the data by specifying the model name and the same variable, frequency, and
# period used in the get_cmip6_model_names function
# ============================================================================#

download_cmip6(
  model = "ACCESS-ESM1-5",
  variable = "pr", frequency = "mon",
  period = "future",
  directory = "D:/data-raw/cmip6/models"
)

