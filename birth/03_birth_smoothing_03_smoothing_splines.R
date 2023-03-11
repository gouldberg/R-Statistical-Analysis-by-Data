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
# Smoothing in time series context
# Smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

par(mfrow=c(1,1))

plot(birth)


# spar = 0.5 to emphasize mid-term cycle, and spar = 1 to emphasize the trend
lines(smooth.spline(time(birth), birth, spar = 0.5), lwd = 2, col = 4)

lines(smooth.spline(time(birth), birth, spar = 1), lty = 2, lwd = 2, col = 2)



