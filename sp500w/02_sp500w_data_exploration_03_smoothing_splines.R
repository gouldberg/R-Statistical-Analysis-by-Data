setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
#   - weekly S&P 500 returns from to Jan. 2003 to Sept. 2012
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")


# this is xts object
str(sp500w)


# convert to ts object
sp500w_c <- ts(sp500w, start = 2003, freq = 52)




# ------------------------------------------------------------------------------
# Smoothing in time series context
# Smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

par(mfrow=c(1,1))

# plot(sp500w)
plot(sp500w_c)


# spar = 0.5 to emphasize the mid-term effect, and spar = 1 to emphasize the trend
lines(smooth.spline(time(sp500w_c), sp500w_c, spar = 0.25), lwd = 2, col = "blue")
lines(smooth.spline(time(sp500w_c), sp500w_c, spar = 0.5), lwd = 2, col = "red")
lines(smooth.spline(time(sp500w_c), sp500w_c, spar = 1), lwd = 2, col = "black")


