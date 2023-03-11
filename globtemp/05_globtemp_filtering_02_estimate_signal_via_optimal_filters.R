setwd("//media//kswada//MyFiles//R//globtemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  globtemp
# ------------------------------------------------------------------------------

data(globtemp, package = "astsa")

str(globtemp)




# ------------------------------------------------------------------------------
# Extract Signal via Optimal Filters:  by kernapply
# ------------------------------------------------------------------------------

# symmetric moving average filter

( k <- kernel("modified.daniell", 6) )

( globf <- kernapply(globtemp, k) )




par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6, 0.6, 0))

plot(diff(globtemp), main = "First Difference")

plot(globtemp, main = "globtemp and Moving Average")

lines(globtempf, lwd = 2, col = "blue")

# lines(ksmooth(time(globtemp), soi, "normal", bandwidth = 1), lwd = 2, lty = 2, col = "red")




# ------------------------------------------------------------------------------
# Extract Signal via Optimal Filters:  by astsa::sigExtract
# ------------------------------------------------------------------------------

# Performs signal extraction and optimal filtering
# L:  degree of smoothing
# M:  number of terms used in the lagged regression approximation
# max.freq:  truncation frequency, which must be larger than 1/M


astsa::SigExtract(diff(globtemp), L = 3, M = 10, max.freq = 0.05)

