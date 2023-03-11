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


oFiltered.DLM <- dlmFilter(y, oFitted.DLM)

names(oFiltered.DLM)



# ----------
par(mfrow = c(1,1))
ts.plot(result_hat, pred$pred, se1, se2, gpars = list(lt = c(2,3,4), col = c(2,3,4), ylim = range(y)))
lines(y, type = "o", col = c("darkgray"))
lines(dropFirst(oFiltered.DLM$m), lty = 1, col = "blue", lwd = 2)



# -->
# Note that state-space model is only following average (mean)
# since the signal to noise ratio is very small



# ------------------------------------------------------------------------------
# Increase the variance of system noise
# ------------------------------------------------------------------------------

oFitted2.DLM <- dlmModPoly(order = 1, dV = exp(oMLE.DLM$par[1]), dW = exp(oMLE.DLM$par[1]) * 0.05)

oFiltered2.DLM <- dlmFilter(y, oFitted2.DLM)



# ----------
oFitted3.DLM <- dlmModPoly(order = 1, dV = exp(oMLE.DLM$par[1]), dW = exp(oMLE.DLM$par[1]) * 0.5)

oFiltered3.DLM <- dlmFilter(y, oFitted3.DLM)



# ----------
par(mfrow = c(1,1))
ts.plot(y, type = "o", col = c("darkgray"))

lines(dropFirst(oFiltered.DLM$m), lty = "solid")
lines(dropFirst(oFiltered2.DLM$m), lty = "longdash")
lines(dropFirst(oFiltered3.DLM$m), lty = "solid", lwd = 2, col = "blue")



# -->
# Note that the larger W/V, filtered series catch up the observed data.


