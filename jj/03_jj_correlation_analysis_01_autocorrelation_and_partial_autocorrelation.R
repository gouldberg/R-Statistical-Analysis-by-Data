setwd("//media//kswada//MyFiles//R//jj")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Johnson & Johnson Quarterly Earnings
# ------------------------------------------------------------------------------

data(jj, package = "astsa")

str(jj)

jj


# ----------
x <- jj

lx <- log(x)

dlx <- diff(lx)

ddlx <- diff(dlx, 4)



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(3, 1))


# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.

# The lags in terms of years
astsa::acf2(dlx, 48)


# -->
# Clearly yearly seasonal cycle



# ----------
astsa::acf2(ddlx, 48)


# -->
# Seasonal Components:  It appears that at the seasons, the ACF is cutting off a lag 1s (s = 4), whereas the PACF is tailing off at lags 1s, 2s, 3s, 4s, ...
# These results implies SMA(1), P = 0, Q = 1, in the season (s = 4)

# Non seasonal components:  Inspecting the sample ACF and PACF at the lower lags, it appears as though both are tailing off.
# This suggests an ARMA(1,1) within the seasons, p = 1,  q = 1 or 3

# Thus, we first try and ARIMA(1,1,1) * (0,1,1)(4) on the logged data.

