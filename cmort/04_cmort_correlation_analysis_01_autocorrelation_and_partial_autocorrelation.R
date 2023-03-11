setwd("//media//kswada//MyFiles//R//cmort")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cmort (Pollution, Temperature and Mortality)
# ------------------------------------------------------------------------------

data(cmort, package = "astsa")
data(tempr, package = "astsa")
data(part, package = "astsa")


str(cmort)
str(tempr)
str(part)


cmort
tempr
part



# ------------------------------------------------------------------------------
# Correlation analysis:  sample autocorrelation function (ACF) for cmort
# ------------------------------------------------------------------------------

library(astsa)


acf2(cmort, max.lag = 52, plot=TRUE)


acf2(diff(cmort), max.lag = 52, plot=TRUE)



# ----------
sarima(cmort, 1,1,1, no.constant = TRUE)




# ----------
# detrended cmort --> AR(2)
dmort <- resid(lm(cmort ~ time(cmort)))


acf2(dmort, max.lag = 110, plot=TRUE)




# ------------------------------------------------------------------------------
# Correlation analysis:  sample autocorrelation function (ACF) for tempr
# ------------------------------------------------------------------------------

acf2(tempr, max.lag = 110, plot=TRUE)


acf2(diff(tempr), max.lag = 110, plot=TRUE)



# ----------
acf2(diff(dtempr), max.lag = 110, plot=TRUE)


sarima(dtempr, 1,1,1, no.constant = TRUE)




# ------------------------------------------------------------------------------
# Correlation analysis:  sample autocorrelation function (ACF) for particulates
# ------------------------------------------------------------------------------

acf2(part, max.lag = 110, plot=TRUE)


acf2(diff(part), max.lag = 110, plot=TRUE)



# ----------
sarima(part, 1,1,2, no.constant = TRUE)



