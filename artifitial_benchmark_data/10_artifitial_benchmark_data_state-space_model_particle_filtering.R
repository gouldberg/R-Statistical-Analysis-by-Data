rm(list=ls())

setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\artifitial_benchmark_data")

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



# ----------
# ARIMA model
result <- arima(y, order = c(1,0,3), transform.pars = FALSE)
ahat <- result$residuals
result_hat <- y - ahat



# ----------
# DLM model
nStateVar <- 1
nHyperParam <- 2
n <- length(y)
funModel <- function(parm){ dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2])) }
oMLE.DLM <- dlmMLE(y, parm = rep(0, 2), build = funModel, hessian = T)

oFitted.DLM <- funModel(oMLE.DLM$par)
oFiltered.DLM <- dlmFilter(y, oFitted.DLM)
m <- oFiltered.DLM$m

var <- dlmSvd2var(oFiltered.DLM$U.R, oFiltered.DLM$D.R)
hwid <- qnorm(0.25, lower = FALSE) * sqrt(unlist(var))

# 50%
filt <- cbind(dropFirst(oFiltered.DLM$m), as.vector(dropFirst(oFiltered.DLM$m)) + hwid %o% c(-1, 1))


oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)
s <- oSmoothed.DLM$s
var <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)
hwid <- qnorm(0.025, lower = FALSE) * sqrt(unlist(var))
# 95%
smooth <- cbind(oSmoothed.DLM$s, as.vector(oSmoothed.DLM$s) + hwid %o% c(-1, 1))



# ----------
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead = 10, sampleNew = 3)
a <- oFcst.DLM$a



# ------------------------------------------------------------------------------
# State-space model Particle filtering
#   - Parameters are known
# ------------------------------------------------------------------------------

set.seed(4521)


# Number of particle filters
N <- 10000


# recognize prior distribution as time 1
y <- c(NA_real_, y)



k <- matrix(1:N, nrow = N, ncol = t_max+1)  



# ----------
# set prior distribution
# particle 

x <- matrix(NA_real_, nrow = t_max+1, ncol = N)

x[1, ] <- rnorm(N, mean = m0, sd = sqrt(C0))



# ----------
# particle weight

w <- matrix(NA_real_, nrow = t_max+1, ncol = N)

w[1, ] <- 1 / N


dim(w)



# ----------
# forward filtering

for (t in (1:t_max)+1){
  
  # State Equation:  particle
  x[t, ] <- rnorm(N, mean = f(x = x[t-1, ], t = t), sd = sqrt(W))
  
  # Observation Equation:  update particle weight
  w[t, ] <- w[t-1, ] * dnorm(y[t], mean = h(x[t, ]), sd = sqrt(V))
  
  
  # standardize weight
  w[t, ] <- w[t, ] / sum(w[t, ])
  
  
  # ----------
  # re-sampling
  # k:  index for resampling  (N * (t_max+1))
  k[, t] <- sample(1:N, prob = w[t, ], replace = TRUE, size = N)
  
  # particle
  x[t, ] <- x[t, k[, t]]
  
  # reset particle weight
  w[t, ] <- 1 / N
}



# ----------
# remove priors
y <- ts(y[-1])

k <- k[, -1, drop = FALSE]

x <- x[-1, , drop = FALSE]

w <- w[-1, , drop = FALSE]



# ------------------------------------------------------------------------------
# compute 50%, 25%, and 75%
# ------------------------------------------------------------------------------

scratch_m       <- sapply(1:t_max, function(t){
  mean(x[t, ])
})


scratch_m_quant <- lapply(c(0.25, 0.75), function(quant){
  sapply(1:t_max, function(t){
    quantile(x[t, ], probs = quant)
  })
})




# ------------------------------------------------------------------------------
# plot filtered series and 50% interval
# ------------------------------------------------------------------------------

ts.plot(cbind(y, x_true, scratch_m),
        col = c("lightgray", "blue", "red"),
        lty = c("solid", "solid", "solid"), lwd = 3)

legend(legend = c("観測値", "状態の真値",  "平均 （粒子フィルタリング)"),
       lty = c("solid", "solid", "solid"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)



# ----------
#ts.plot(cbind(y, filt[,c(2,3)], do.call("cbind", scratch_m_quant)),
#        col = c("lightgray", "blue", "blue", "red", "red"),
#        lty = c("solid", "solid", "solid", "dashed", "dashed"))


#legend(legend = c("観測値", "50%区間 （カルマンフィルタリング)",  "50%区間 （粒子フィルタリング)"),
#       lty = c("solid", "solid", "dashed"),
#       col = c("lightgray", "blue", "red"),
#       x = "topright", text.width = 70, cex = 0.6)


ts.plot(cbind(y, do.call("cbind", scratch_m_quant)),
        col = c("lightgray", "blue", "blue", "red", "red"),
        lty = c("solid", "solid", "solid", "dashed", "dashed"))


legend(legend = c("観測値", "50%区間 （粒子フィルタリング)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)

