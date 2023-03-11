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
# Filtered series
# ------------------------------------------------------------------------------

# prior distribution at t = 0:  theta(0) ~ N(m(0), C(0))
# observed equation:  Vt = F(t) * theta(t) + v(t),  v(t) ~ N(0, V(t))
# system equation:  theta(t) ~ G(t) * theta(t-1) + w(t),  w(t) ~ N(0, W(t))


# apply estimated parameters
oFitted.DLM <- funModel(oMLE.DLM$par)

oFitted.DLM



# ----------
# Filtering:
# y:  observed series
# a:  average of predicted distribution
# U.R, D.R:  matrix produced by SVD of variance of predicted errors (a - theta)
#            variance = dlmSvd2var(U.R, D.R) 
# m:  average of state-vector distribution  (note that start at t = 0)
#     here in random-walk plus noise model, m(0) = E(y(1)) = E(theta)
# U.C, D.C:  matrix produced by SVD of variance of predicted errors (m - theta0)
#            variance = dlmSvd2var(U.C, D.C)
# f:  average of one-step-ahead prediction distribution = F %:% a
# y - f:  innovation


oFiltered.DLM <- dlmFilter(Nile, oFitted.DLM)

names(oFiltered.DLM)



# ----------
par(mfrow = c(1,1))
ts.plot(result_hat, pred$pred, se1, se2, gpars = list(lt = c(2,3,4), col = c(2,3,4), ylim = range(Nile)))
lines(Nile, type = "o", col = c("darkgray"))
lines(dropFirst(oFiltered.DLM$m), lty = 1, col = "blue")

abline(v = 1898, lty = 2)



# -->
# Note that state-space model is faster in chatch up the observed data



# ------------------------------------------------------------------------------
# Increase the variance of system noise
# ------------------------------------------------------------------------------

oFitted2.DLM <- dlmModPoly(order = 1, dV = exp(oMLE.DLM$par[1]), dW = exp(oMLE.DLM$par[1]) * 0.05)

oFiltered2.DLM <- dlmFilter(Nile, oFitted2.DLM)



# ----------
oFitted3.DLM <- dlmModPoly(order = 1, dV = exp(oMLE.DLM$par[1]), dW = exp(oMLE.DLM$par[1]) * 0.5)

oFiltered3.DLM <- dlmFilter(Nile, oFitted3.DLM)



# ----------
par(mfrow = c(1,1))
ts.plot(Nile, type = "o", col = c("darkgray"))

lines(dropFirst(oFiltered.DLM$m), lty = "solid")
lines(dropFirst(oFiltered2.DLM$m), lty = "longdash")
lines(dropFirst(oFiltered3.DLM$m), lty = "dotdash")

abline(v = 1898, lty = 2)



# -->
# Note that the larger W/V, filtered series catch up the observed data.


