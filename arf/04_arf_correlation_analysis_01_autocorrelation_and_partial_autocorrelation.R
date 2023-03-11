setwd("//media//kswada//MyFiles//R//arf")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  arf
# ------------------------------------------------------------------------------

data(arf, package = "astsa")

str(arf)

head(arf)



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(1, 1))


# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.
astsa::acf2(arf, 200, main = "ARF")
astsa::acf2(log(arf), 200, main = "ARF")


# -->
# Suggesting long memory time series
# Long memory time series data tend to exhibit sample autocorrelations that are not necessarily large, but persist for a long time



# ----------
astsa::acf2(diff(arf), 48, main = "ARF")


# -->
# Suggesting ARMA(1,1) or AR(1) or MA(1) ??



# ----------
# removing trend
astsa::acf2(diff(resid(lm(arf ~ time(arf)))), 48, main = "ARF")


# --> same !!