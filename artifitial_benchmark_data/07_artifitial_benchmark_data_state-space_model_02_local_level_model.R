rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Artifitial benchmark data
# ------------------------------------------------------------------------------

# generate benchmark data


set.seed(23)


library(dlm)



# ----------
# set model
W <- 1

V <- 2

m0 <- 10

C0 <- 9


# State Equation
f <- function(x, t) 1 / 2 * x + 25 * x / (1 + x^2) + 8 * cos(1.2 * t)


# Observation
h <- function(x) x^2 / 20


t_max <- 100


x_true <- rep(NA_real_, times = t_max + 1)

y <- rep(NA_real_, times = t_max + 1)


x_true[1] <- m0

for(it in (1:t_max) + 1){
  x_true[it] <- f(x_true[it - 1], it) + rnorm(n = 1, sd = sqrt(W))
  
  y[it] <- h(x_true[it]) + rnorm(n = 1, sd = sqrt(V))
}


x_true <- x_true[-1]

y <- y[-1]



# ----------

dat <- data.frame(x_true = x_true, y = y)

ts.plot(dat, ylab = "y", type = "l", lty = c(1,1), col = c("black", "gray"))


y <- as.ts(y)



# ------------------------------------------------------------------------------
# Fit ARIMA model
# ------------------------------------------------------------------------------


result <- arima(y, order = c(1,0,3), transform.pars = FALSE)


# ----------
summary(result)




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
# signal - noise ratio  --> large noise

exp(oMLE.DLM$par[2]) / exp(oMLE.DLM$par[1])



# ------------------------------------------------------------------------------
# Estimated standard errors
# ------------------------------------------------------------------------------

oMLE.DLM$par

sqrt(diag(solve(oMLE.DLM$hessian)))


