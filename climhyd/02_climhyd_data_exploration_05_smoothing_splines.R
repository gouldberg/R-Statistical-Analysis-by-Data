setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)



# ------------------------------------------------------------------------------
# Smoothing in time series context
# Smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness


par(mfrow=c(2,1))


# ----------
# precipitation

obj_ts <- climhyd$Precip

plot(obj_ts, type = "l")

lines(smooth.spline(time(obj_ts), obj_ts, spar = 0.25), lwd = 2, col = gray(0.3))

lines(smooth.spline(time(obj_ts), obj_ts, spar = 0.5), lty = 2, lwd = 2, col = "blue")

lines(smooth.spline(time(obj_ts), obj_ts, spar = 0.75), lty = 2, lwd = 2, col = "red")



# ----------
# Inflow

obj_ts <- climhyd$Inflow

plot(obj_ts, type = "l")

lines(smooth.spline(time(obj_ts), obj_ts, spar = 0.25), lwd = 2, col = gray(0.3))

lines(smooth.spline(time(obj_ts), obj_ts, spar = 0.5), lty = 2, lwd = 2, col = "blue")

lines(smooth.spline(time(obj_ts), obj_ts, spar = 0.75), lty = 2, lwd = 2, col = "red")




# -->
# somewhat down trending


