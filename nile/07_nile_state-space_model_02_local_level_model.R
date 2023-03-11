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
# State-Space model:  local level model (random walk + noize)
# ------------------------------------------------------------------------------


nStateVar <- 1


nHyperParam <- 2


n <- length(Nile)



# ----------
# dV: variance of observation noise
# dW: diagonal elements of the variance matrix of the system noise
# require exp() to be positive (as variance)
# rep(0, 2) as initial value
# hessian = T:  for commputing standard errors


funModel <- function(parm){ dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2])) }


oMLE.DLM <- dlmMLE(Nile, parm = rep(0, 2), build = funModel, hessian = T)



# ----------
# Check whether the convergence is achieved (as zero)

oMLE.DLM$convergence



# ----------
# log likelihood
oMLE.DLM$value



# ------------------------------------------------------------------------------
# Estimated parameters: dV and dW
# ------------------------------------------------------------------------------

oMLE.DLM$par

exp(oMLE.DLM$par[1])

exp(oMLE.DLM$par[2])



# ----------
# noise / signal  --> large noise

exp(oMLE.DLM$par[2]) / exp(oMLE.DLM$par[1])



# ------------------------------------------------------------------------------
# Estimated standard errors
# ------------------------------------------------------------------------------

oMLE.DLM$par

sqrt(diag(solve(oMLE.DLM$hessian)))


