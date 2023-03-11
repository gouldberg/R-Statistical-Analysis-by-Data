rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Nile
# ------------------------------------------------------------------------------

data(Nile)


dim(Nile)


str(Nile)



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


ts.plot(Nile, type = "o")

abline(v = seq(1870, 1970, by = 10), lty = 2, col = "gray")



# ----------
forecast::ndiffs(Nile)



# ----------
ts.plot(diff(Nile))

abline(v = seq(1870, 1970, by = 10), lty = 2, col = "gray")



# -->
# 10 years some cycle




# ------------------------------------------------------------------------------
# data exploration:  smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

par(mfrow=c(1,1))

plot(Nile)


lines(smooth.spline(time(Nile), Nile, spar = 0.5), lwd = 2, col = 4)

lines(smooth.spline(time(Nile), Nile, spar = 1), lty = 2, lwd = 2, col = 2)

abline(v = seq(1870, 1970, by = 10), lty = 2, col = "gray")



# -----------
par(mfrow=c(1,1))

plot(diff(Nile))


lines(smooth.spline(time(diff(Nile)), diff(Nile), spar = 0.5), lwd = 2, col = 4)

lines(smooth.spline(time(diff(Nile)), diff(Nile), spar = 1), lty = 2, lwd = 2, col = 2)

abline(v = seq(1870, 1970, by = 10), lty = 2, col = "gray")




