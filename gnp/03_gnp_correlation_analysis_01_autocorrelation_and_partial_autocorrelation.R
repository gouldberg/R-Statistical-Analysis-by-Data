setwd("//media//kswada//MyFiles//R//gnp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  GNP data
# ------------------------------------------------------------------------------

data(gnp, package = "astsa")

str(gnp)

head(gnp)



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(3, 1))


# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.

# The lags in terms of years
astsa::acf2(diff(log(gnp)), 24, main = "Quarterly U.S. GNP growth rate")



# -->
# ACF is cutting off at log2 and the PACF is tailing off.
# This would suggest the GNP growth rate follows an MA(2) process, or log GNP follows an ARIMA(0,1,2) model.

# Rather than focus on one model,
# we will also suggest that it appears that the ACF is tailing off and the PACF is cutting off at lag 1.
# This suggests an AR(1) model for the growth rate, or ARIMA(1,1,0) for log GNP.


