setwd("//media//kswada//MyFiles//R//birth")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  birth
# ------------------------------------------------------------------------------

data(birth, package = "astsa")

str(birth)

head(birth)



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(3, 1))


# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.

# The lags in terms of months
astsa::acf2(diff(log(birth)), 48)


# -->
# Clearly yearly seasonal cycle

# we first try and ARIMA(1,1,2) * (1,1,1)(12) on the logged data.
