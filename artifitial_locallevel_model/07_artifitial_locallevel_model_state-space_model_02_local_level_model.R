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
# State-Space model:  local level model (random walk + noize)
# ------------------------------------------------------------------------------


nStateVar <- 1

nHyperParam <- 2

n <- length(y)



# ----------
# dV: variance of observation noise
# dW: diagonal elements of the variance matrix of the system noise
# require exp() to be positive (as variance)
# rep(0, 2) as initial value
# hessian = T:  for commputing standard errors


funModel <- function(parm){ dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2])) }

oMLE.DLM <- dlmMLE(y, parm = rep(0, 2), build = funModel, hessian = T)



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


