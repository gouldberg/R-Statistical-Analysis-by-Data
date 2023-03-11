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
# Smoothing in time series context
# Smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

# spar = 0.5 to emphasize the El Nino cycle, and spar = 1 to emphasize the trend

par(mfrow=c(3,1))


plot(cmort)
lines(smooth.spline(time(cmort), cmort, spar = 0.5), lwd = 2, col = 4)
lines(smooth.spline(time(cmort), cmort, spar = 1), lty = 2, lwd = 2, col = 2)


plot(tempr)
lines(smooth.spline(time(tempr), tempr, spar = 0.5), lwd = 2, col = 4)
lines(smooth.spline(time(tempr), tempr, spar = 1), lty = 2, lwd = 2, col = 2)


plot(part)
lines(smooth.spline(time(part), part, spar = 0.5), lwd = 2, col = 4)
lines(smooth.spline(time(part), part, spar = 1), lty = 2, lwd = 2, col = 2)



# -->
# cmort is somewhat down-trending
# tempr is upward-trending
# particulate is down and up



