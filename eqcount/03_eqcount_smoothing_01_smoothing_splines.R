setwd("//media//kswada//MyFiles//R//eqcount")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqcount
# ------------------------------------------------------------------------------

data(EQcount, package = "astsa")


str(EQcount)


EQcount



# ------------------------------------------------------------------------------
# Smoothing in time series context
# Smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

par(mfrow=c(1,1))

plot(EQcount)


# spar = 0.5 to emphasize the mid-term effect, and spar = 1 to emphasize the trend
lines(smooth.spline(time(EQcount), EQcount, spar = 0.3), lwd = 2, col = "blue")
lines(smooth.spline(time(EQcount), EQcount, spar = 0.5), lwd = 2, col = "red")
lines(smooth.spline(time(EQcount), EQcount, spar = 1), lwd = 2, col = "black")



# -->
# Notice that there is mid-term 20 years cycle  

