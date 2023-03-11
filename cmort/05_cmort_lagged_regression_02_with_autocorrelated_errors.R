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
# Check the auto-correlation and partial autocorrelation in residuas
# ------------------------------------------------------------------------------

# check residual autocorrelation

acf2(resid(fit5), max.lag = 52)



# -->
# Indicating the residuals is modelled by AR(2)




# ------------------------------------------------------------------------------
# model with auto-correlated errors
# ------------------------------------------------------------------------------


# extract individual time series from ts.intersect

cmort_2 <- data.frame(dat)$cmort

trend_2 <- data.frame(dat)$trend

temp_2 <- data.frame(dat)$temp

temp_22 <- data.frame(dat)$temp2

part4_2 <- data.frame(dat)$part4

part_2 <- data.frame(dat)$part




# ----------
# Regression with autocorrelated errors

sarima(cmort_2, p = 2, d = 0, q = 0, xreg = cbind(trend_2, temp_2, temp_22, part4_2, part_2))




# -->
# Note that some autocorrealtion is remained ....



