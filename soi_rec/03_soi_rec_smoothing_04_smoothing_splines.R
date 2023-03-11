setwd("//media//kswada//MyFiles//R//soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soi
# data:  rec
# ------------------------------------------------------------------------------

data(soi, package = "astsa")

data(rec, package = "astsa")




# ------------------------------------------------------------------------------
# Smoothing in time series context
# Smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

par(mfrow=c(2,1))

plot(soi)

lines(smooth.spline(time(soi), soi, spar = 0.5), lwd = 2, col = 4)

lines(smooth.spline(time(soi), soi, spar = 1), lty = 2, lwd = 2, col = 2)


plot(rec)

lines(smooth.spline(time(rec), rec, spar = 0.5), lwd = 2, col = 4)

lines(smooth.spline(time(rec), rec, spar = 1), lty = 2, lwd = 2, col = 2)



# spar = 0.5 to emphasize the El Nino cycle, and spar = 1 to emphasize the trend
# Also a negative trend in SOI would indicate the long-term warming of the Pacific Ocean
