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
# State-space model Particle filtering:  Particle Smoothing (Kitagawa Algorithm)
#   - Parameters are known
# ------------------------------------------------------------------------------

set.seed(4521)


smoothing_index <- function(t_current){
  
  index <- 1:N

  for (t in (t_current+1):t_max){
    index <- index[k[, t]]
  }
  return(index)
}



ki <- sapply(1:(t_max-1), function(t){ x[t, smoothing_index(t)] })


ki <- t(cbind(ki, x[t_max, ]))



# ------------------------------------------------------------------------------
# compute 50%, 25% and 75%
# ------------------------------------------------------------------------------

scratch_s         <- sapply(1:t_max, function(t){
  mean(ki[t, ])
})


scratch_s_quant   <- lapply(c(0.25, 0.75), function(quant){
  sapply(1:t_max, function(t){
    quantile(ki[t, ], probs = quant)
  })
})




# ------------------------------------------------------------------------------
# plot smoothed series and 50% interval
# ------------------------------------------------------------------------------

ts.plot(cbind(y, dropFirst(s), scratch_s),
        col = c("lightgray", "blue", "red"),
        lty = c("solid", "solid", "dashed"))


legend(legend = c("観測値", "平均 （カルマン平滑化)",  "平均 （粒子平滑化）"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)



# ----------
ts.plot(cbind(y, do.call("cbind", s_quant), do.call("cbind", scratch_s_quant)),
        col = c("lightgray", "blue", "blue", "red", "red"),
        lty = c("solid", "solid", "solid", "dashed", "dashed"))


legend(legend = c("観測値", "50%区間 （カルマン平滑化)",  "50%区間 （粒子平滑化)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)




# ------------------------------------------------------------------------------
# State-space model Particle filtering:  Particle Smoothing (FFBSi:  Forward Filtering Backward Simulation Algorithm)
#   - Parameters are known
# ------------------------------------------------------------------------------

set.seed(4521)


path_max <- 500


progress_bar <- txtProgressBar(min = 2, max = path_max, style = 3)


# smoothed particles
b  <- array(NA_real_, dim = c(t_max, N, path_max))


# smoothed particles weight
rho  <- matrix(NA_real_, nrow = t_max, ncol = N)

rho[t_max, ]  <- w[t_max, ]



# ----------

for (path in 1:path_max){
  setTxtProgressBar(pb = progress_bar, value = path)

  # initialize the smoothed particles distribution at t_max
  b[t_max, , path] <- sample(x[t_max, ],
                             prob = w[t_max, ], replace = TRUE, size = N)

  # Backward simulation
  for (t in (t_max-1):1){
    
    # weight
    rho[t, ] <- w[t, ] * dnorm(b[t+1, , path], mean = x[t, ], sd = sqrt(W))
    
    # standardize weight
    rho[t, ] <- rho[t, ] / sum(rho[t, ])
    
    # ----------
    # re-sampling
    FFBSi_index <- sample(1:N, prob = rho[t, ], replace = TRUE, size = N)
    b[t, , path] <- x[t, FFBSi_index]
    
    # reset weight
    rho[t, ] <- 1 / N
  }
}



# ----------
scratch_s         <- sapply(1:t_max, function(t){
  mean(b[t, ,])
})


scratch_s_quant   <- lapply(c(0.25, 0.75), function(quant){
  sapply(1:t_max, function(t){
    quantile(b[t, ,], probs = quant)
  })
})



# ----------
ts.plot(cbind(y, dropFirst(s), scratch_s),
        col = c("lightgray", "blue", "red"),
        lty = c("solid", "solid", "dashed"))



legend(legend = c("観測値", "平均 （カルマン平滑化)",  "平均 （粒子平滑化）"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)



ts.plot(cbind(y, do.call("cbind", s_quant), do.call("cbind", scratch_s_quant)),
        col = c("lightgray", "blue", "blue", "red", "red"),
        lty = c("solid", "solid", "solid", "dashed", "dashed"))


legend(legend = c("観測値", "50%区間 （カルマン平滑化)",  "50%区間 （粒子平滑化)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)
