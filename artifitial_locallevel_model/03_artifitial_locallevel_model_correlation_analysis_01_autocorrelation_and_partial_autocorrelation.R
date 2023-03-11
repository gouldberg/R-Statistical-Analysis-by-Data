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
# Correlation analysis:  autocorrelation and partial autocorrelation
#   - partial autocorrelation:
#     regressed the values of the time series at all shorter lags
#     It contrasts with the autocorrelation function, which does not control for other lags
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1, 1))


# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.

astsa::acf2(y, 48, main = "y")

astsa::acf2(diff(y), 48, main = "y")




# ----------
library(astsa)

sarima(y, p = 1, d = 1, q = 1, no.constant = TRUE)



