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
# Moving average smoother
# ------------------------------------------------------------------------------

# boxcar-type weights
k <- 5

( wgts <- c(.5, rep(1, k), .5) / (k + 2) )

plot(wgts, type = "b")



# sides = 2:  for convolution filters only, if sides = 1 the filter coeffs are for past values only, if side = 2 they are centered around lag 0
( globf <- stats::filter(globtemp, sides = 2, filter = wgts) )



# ----------
par(mfrow=c(1,1))

plot(globtemp)

lines(globf, lwd=2, col = 4)



# ----------
nwgts <- c(rep(0, 10), wgts, rep(0, 10))

plot(nwgts, type = "l", xaxt = 'n', yaxt = 'n', ann = FALSE)

