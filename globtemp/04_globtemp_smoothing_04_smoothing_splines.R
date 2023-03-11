setwd("//media//kswada//MyFiles//R//globtemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  globtemp
# ------------------------------------------------------------------------------

data(globtemp, package = "astsa")

str(globtemp)



# ------------------------------------------------------------------------------
# Smoothing in time series context
# Smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

par(mfrow=c(1,1))

plot(globtemp)


# spar = 0.5 to emphasize local cycle, and spar = 1 to emphasize the trend
lines(smooth.spline(time(globtemp), globtemp, spar = 0.5), lwd = 2, col = 4)

lines(smooth.spline(time(globtemp), globtemp, spar = 1), lty = 2, lwd = 2, col = 2)



# -->
# Similar to lowess default smoother,
# smoothing splines shows increase of velocity of global temperature, too
