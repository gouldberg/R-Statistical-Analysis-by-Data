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
# Original time series + 1st difference (high-pass filter) and 12 months filter (low-pass filter)
#   - differencing:  example of a high-pass filter because it retains or passes the higher frequencies
#   - moving average:  low-pass filter because it passes the lower or slower frequencies
# ------------------------------------------------------------------------------

par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.6, 0.6, 0))

plot(soi, main = "SOI")

plot(diff(soi), main = "First Difference")



# annual symmetric moving average filter
# y(t) = 1 / 24 * (x(t-6) + x(t+6)) + 1 / 12 * sum(x(t-r))    r = -5 to 5

( k <- kernel("modified.daniell", 6) )

( soif <- kernapply(soi, k) )

plot(soif, main = "Seasonal Moving Average")




# -->
# Note that the slower periods are enhanced in the symmetric moving average and the seasonal or yearly frequencies are attenuated.
# The filtered series makes about 9 cycles in the length of the data (about one cycle every 52 months) and
# the moving average filter tends to enhance or extract the El Nino signal.
# Moreover, by low-pass filtering the data, we get a better sense of the El Nino effect and its irregularity.




# ------------------------------------------------------------------------------
# Spectrum analysis of the low-pass filtered series
# ------------------------------------------------------------------------------

spectrum(soif, spans = 9, log = "no")

abline(v = 12/52, lty = "dashed")


