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
# Original time series + 1st difference (high-pass filter) and 12 months filter (low-pass filter)
#   - differencing:  example of a high-pass filter because it retains or passes the higher frequencies
#   - moving average:  low-pass filter because it passes the lower or slower frequencies
# ------------------------------------------------------------------------------

par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.6, 0.6, 0))

plot(Nile, main = "Nile")
abline(v = seq(1870, 1970, by = 10), lty = 2, col = "gray")

plot(diff(Nile), main = "First Difference")
abline(v = seq(1870, 1970, by = 10), h = 0, lty = 2, col = "gray")



# ----------
# 10 years filter

( k <- kernel("modified.daniell", 5) )

( nilef <- kernapply(Nile, k) )

plot(nilef, main = "10 years filter")
abline(v = seq(1870, 1970, by = 10), lty = 2, col = "gray")





# ------------------------------------------------------------------------------
# Spectrum analysis of the low-pass filtered series
# ------------------------------------------------------------------------------

spectrum(nilef, spans = 9, log = "no")


