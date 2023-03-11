setwd("//media//kswada//MyFiles//R//so2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  so2
# ------------------------------------------------------------------------------

data(so2, package = "astsa")


str(so2)


so2



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1, 1))


# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.

astsa::acf2(diff(log(so2)), 104, main = "Sulfur Dioxide")



# -->
# Suggesting ARIMA(7,1,3)


