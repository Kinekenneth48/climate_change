
# ============================================================================#
# load libraries
# ============================================================================#
library(evd)
library(ismev)
library(lax)

source("R/gev_fit_time_varying.R")

# ============================================================================#
# generate GEV data
# ============================================================================#
set.seed(1253)
n <- 200
beta1 <- 0.015
X <- 1:n

## simulate data
Y <- rgev(n = n, loc = 0, scale = 1, shape = 0.15) + beta1 * X


# ============================================================================#
# return level values with time varying GEV parameters
# ============================================================================#
test <- gev_fit_time_varying(Y)
rl_time <- c(min(test, na.rm = TRUE), mean(test, na.rm = TRUE), max(test, na.rm = TRUE))

# ============================================================================#
# return level values with GEV parameters (assuming stationary)
# ============================================================================#
fit.gev1 <- gev.fit(xdat = Y, show = FALSE)
adj_gev_fit <- alogLik(fit.gev1)
rl_stationary <- return_level(adj_gev_fit, inc = 0.05, m = 50)
rl_stat <- rl_stationary[["rl_prof"]]

AIC(fit.gev1)

ydat_base <- matrix(rep(1:length(Y), each = 1), nrow = length(Y), ncol = 1)
fit.gev2 <- gev.fit(xdat = Y, show = FALSE,ydat = ydat_base, mul = 1)
fit.gev3 <- gev.fit(xdat = Y, show = FALSE,ydat = ydat_base, sigl = 1, mul = 1)
fit.gev4 <- gev.fit(xdat = Y, show = FALSE,ydat = ydat_base, sigl = 1, mul = 1,shl = 1)
AIC(fit.gev1)
AIC(fit.gev2)
AIC(fit.gev3)
AIC(fit.gev4)
# ============================================================================#
# caution: rl_time is linked but doesn't have the same interpenetration as rl_stat
#  x value risk plot : the probability that the largest load is higher than 7.621292
# constant risk plot: the risk that the highest snow load in a year is larger than
# 7.6 is less than 0.02
# ============================================================================#

rl_time
rl_stat
