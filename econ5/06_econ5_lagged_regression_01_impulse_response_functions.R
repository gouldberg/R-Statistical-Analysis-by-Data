setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")


str(econ5)


car::some(econ5)




# ------------------------------------------------------------------------------
# Data transformation
# ------------------------------------------------------------------------------

# First logging and then removing the linear trend

U <- resid(lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(lm(log(econ5$consum) ~ time(log(econ5$consum))))


x <- cbind(G, C, U)


MTSplot(x)




# ----------
Ud <- diff(econ5$unemp)

Gd <- diff(econ5$govinv)

Cd <- diff(econ5$consum)



dat <- cbind(Gd, Cd, Ud)




# ------------------------------------------------------------------------------
# Check the cross-correlations among differenced time series
# ------------------------------------------------------------------------------


# differenced
acf(dat, max.lag = 20, cex.main = 2)



# -->
# Gd agasint Ud has lag 5,7,8
# Cd against Ud has lag 0 (concurrent)



# detrended 
acf(x, max.lag = 20, cex.main = 2)



# -->
# G against U has lag up to 12
# C against U has long lags




# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function
# ------------------------------------------------------------------------------

# for detrended time series

# estimated regression or impulse response function for G, with M = 32 and L = 15
# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero


mod_gtou <- astsa::LagReg(input = G, output = U, L = 15, M = 32, threshold = 0.01)



# -->
# Note that concurrent negative effect is large
# MSE = 0.019




# ----------
# inverse model

mod_utog <- astsa::LagReg(input = U, output = G, L = 15, M = 32, inverse = TRUE,  threshold = 0.01)



# -->
# Note that concurrent negative effect is large
# This is persimonious and MSE = 0.0010, but the scale is different between variables
# MSE = 0.0010 



summary(x)




# ------------------------------------------------------------------------------
# Compare with scaled data
# ------------------------------------------------------------------------------

xs <- scale(x)

str(xs)

mod_gtou <- astsa::LagReg(input = xs[,"G"], output = xs[,"U"], L = 15, M = 32, threshold = 0.01)


mod_utog <- astsa::LagReg(input = xs[,"U"], output = xs[,"G"], L = 15, M = 32, inverse = TRUE,  threshold = 0.01)



# -->
# MSE is larger for inverse model ...


