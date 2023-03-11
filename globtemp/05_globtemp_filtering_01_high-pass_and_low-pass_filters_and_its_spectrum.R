setwd("//media//kswada//MyFiles//R//globtemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  globtemp
# ------------------------------------------------------------------------------

data(globtemp, package = "astsa")

str(globtemp)



# ------------------------------------------------------------------------------
# Original time series + 1st difference (high-pass filter) and 12 months filter (low-pass filter)
#   - differencing:  example of a high-pass filter because it retains or passes the higher frequencies
#   - moving average:  low-pass filter because it passes the lower or slower frequencies
# ------------------------------------------------------------------------------


par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.6, 0.6, 0))

plot(globtemp, main = "globtemp")

plot(diff(globtemp), main = "First Difference")



# ----------
# symmetric moving average filter

( k <- kernel("modified.daniell", 3) )

( globf <- kernapply(globtemp, k) )

plot(globf, main = " Moving Average")




# -->
# Note that the slower periods are enhanced in the symmetric moving average and the yearly frequencies are attenuated.


# same by 'filter'
# ( k2 <- c(k$coef[4:1], k$coef[2:4]) )
# globf2 <- stats::filter(globtemp, filter = k2, method = "convolution", sides = 2)
# plot(globf2, main = " Moving Average")




# ------------------------------------------------------------------------------
# Spectrum analysis of the low-pass filtered series
# ------------------------------------------------------------------------------

spectrum(globf, spans = 3, log = "no")


