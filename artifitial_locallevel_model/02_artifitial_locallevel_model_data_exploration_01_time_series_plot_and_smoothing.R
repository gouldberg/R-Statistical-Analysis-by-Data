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
# data exploration:  time series plot
# ------------------------------------------------------------------------------

ts.plot(y)

abline(v = seq(0, 200, by = 10), lty = 2, col = "gray")



# ----------
forecast::ndiffs(y)



# ----------
ts.plot(diff(y))

abline(v = seq(0, 200, by = 10), lty = 2, col = "gray")




# ------------------------------------------------------------------------------
# data exploration:  smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

par(mfrow=c(1,1))

plot(y)


lines(smooth.spline(time(y), y, spar = 0.5), lwd = 2, col = 4)

lines(smooth.spline(time(y), y, spar = 1), lty = 2, lwd = 2, col = 2)

abline(v = seq(0, 200, by = 10), lty = 2, col = "gray")




# -----------
par(mfrow=c(1,1))

plot(diff(y))


lines(smooth.spline(time(diff(y)), diff(y), spar = 0.5), lwd = 2, col = 4)

lines(smooth.spline(time(diff(y)), diff(y), spar = 1), lty = 2, lwd = 2, col = 2)

abline(v = seq(0, 200, by = 10), lty = 2, col = "gray")




