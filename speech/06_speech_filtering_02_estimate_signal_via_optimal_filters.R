setwd("//media//kswada//MyFiles//R//speech")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  speech
# ------------------------------------------------------------------------------

data(speech, package = "astsa")

str(speech)


head(speech)



# ------------------------------------------------------------------------------
# Estimating the El Nino Signal via Optimal Filters
# ------------------------------------------------------------------------------

par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.6, 0.6, 0))

plot(speech, main = "speech")

plot(diff(speech), main = "First Difference")



# ----------
# symmetric moving average filter
# y(t) = 1 / 24 * (x(t-6) + x(t+6)) + 1 / 12 * sum(x(t-r))    r = -5 to 5

( k <- kernel("modified.daniell", 6) )
( speechf <- kernapply(speech, k) )
plot(speechf, main = "Seasonal Moving Average")




# ----------
# Performs signal extraction and optimal filtering
# L:  degree of smoothing
# M:  number of terms used in the lagged regression approximation
# max.freq:  truncation frequency, which must be larger than 1/M
astsa::SigExtract(speech, L = 9, M = 64, max.freq = 0.05)



