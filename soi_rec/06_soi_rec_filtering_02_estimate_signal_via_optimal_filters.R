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
# Estimating the El Nino Signal via Optimal Filters:  by kernapply
# ------------------------------------------------------------------------------

# annual symmetric moving average filter
# y(t) = 1 / 24 * (x(t-6) + x(t+6)) + 1 / 12 * sum(x(t-r))    r = -5 to 5

( k <- kernel("modified.daniell", 6) )

( soif <- kernapply(soi, k) )




par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6, 0.6, 0))

plot(diff(soi), main = "First Difference")

plot(soi, main = "SOI and Seasonal Moving Average")

lines(soif, lwd = 2, col = "blue")

# lines(ksmooth(time(soi), soi, "normal", bandwidth = 1), lwd = 2, lty = 2, col = "red")


# -->
# Moving average filter shows a good deal of higher frequency chatter riding on the smoothed version,
# which has been introduced by the higher frequencies that leak through in the squared frequency response.




# ------------------------------------------------------------------------------
# Estimating the El Nino Signal via Optimal Filters:  by astsa::sigExtract
# ------------------------------------------------------------------------------

# Performs signal extraction and optimal filtering
# L:  degree of smoothing
# M:  number of terms used in the lagged regression approximation
# max.freq:  truncation frequency, which must be larger than 1/M


astsa::SigExtract(soi, L = 9, M = 64, max.freq = 0.05)


# -->
# Smooth extracted signals conveys the essence of the underlying El Nino signal.

