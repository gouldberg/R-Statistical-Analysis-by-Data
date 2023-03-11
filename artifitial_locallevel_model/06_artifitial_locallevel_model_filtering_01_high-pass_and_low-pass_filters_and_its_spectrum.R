rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Artifitial local level model
# ------------------------------------------------------------------------------

# generate local level model data


set.seed(23)


library(dlm)



# ----------
# set model
W <- 1

V <- 2

m0 <- 10

C0 <- 9

mod <- dlmModPoly(order = 1, dW = W, dV = V, m0 = m0, C0 = C0)



# ----------
# generate observation by kalman forecast

t_max <- 200

sim_data <- dlmForecast(mod = mod, nAhead = t_max, sampleNew = 1)

y <- ts(as.vector(sim_data$newObs[[1]]))

plot(y, ylab = "y")



# ------------------------------------------------------------------------------
# Original time series + 1st difference (high-pass filter) and 12 months filter (low-pass filter)
#   - differencing:  example of a high-pass filter because it retains or passes the higher frequencies
#   - moving average:  low-pass filter because it passes the lower or slower frequencies
# ------------------------------------------------------------------------------

par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.6, 0.6, 0))

plot(y, main = "y")
abline(v = seq(0, 200, by = 10), lty = 2, col = "gray")

plot(diff(y), main = "First Difference")
abline(v = seq(0, 200, by = 10), lty = 2, col = "gray")



# ----------
( k <- kernel("modified.daniell", 5) )

( yf <- kernapply(y, k) )

plot(yf, main = "filter")
abline(v = seq(0, 200, by = 10), lty = 2, col = "gray")





# ------------------------------------------------------------------------------
# Spectrum analysis of the low-pass filtered series
# ------------------------------------------------------------------------------

spectrum(yf, spans = 9, log = "no")


