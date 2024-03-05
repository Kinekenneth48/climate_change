
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
Time <- 1:n

## simulate data
Y <- rgev(n = n, loc = 0, scale = 1, shape = 0.15) + beta1 * X

r=ismev::gev.fit(
  xdat = Y, show = FALSE,
  method = "Nelder-Mead", maxit = 100000,
  muinit = 0, siginit=1, shinit=0.15
)

ydat_base= matrix(rep(1:n, each = 1), nrow = n, ncol = 1)
fit1 <- ismev::gev.fit(
  xdat = Y, ydat = ydat_base, mul = 1, show = FALSE,
  method = "Nelder-Mead", maxit = 100000
)

# Combine data into a dataframe
data_df <- data.frame(Time, X, Y)


fit00= fevd(Y, data_df,type = c("GEV"))
fit11= fevd(Y, data_df,type = c("GEV"), location.fun=~Time)
fit22= fevd(Y, data_df,type = c("GEV"), location.fun=~Time, scale.fun = ~Time)
fit33= fevd(Y, data_df,type = c("GEV"), location.fun=~Time, scale.fun = ~Time,shape.fun = ~Time)
lr.test(fit11, fit22)

as.vector(return.level(fit11,return.period = c(50)))
return.level(fit22,return.period = c(50))
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
