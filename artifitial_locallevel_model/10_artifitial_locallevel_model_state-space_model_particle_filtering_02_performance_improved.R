rm(list=ls())

setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\artifitial_locallevel_model")

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



# load(file = "ArtifitialLocalLevelModel.RData")



# ----------
# ARIMA model
result <- arima(y, order = c(1,1,1), transform.pars = FALSE)
ahat <- result$residuals
result_hat <- y - ahat



# ----------
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
filt <- cbind(dropFirst(oFiltered.DLM$m), as.vector(dropFirst(oFiltered.DLM$m)) + dropFirst(hwid) %o% c(-1, 1))



oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)
s <- oSmoothed.DLM$s
var <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)
hwid <- qnorm(0.025, lower = FALSE) * sqrt(unlist(var))
# 95%
smooth <- cbind(oSmoothed.DLM$s, as.vector(oSmoothed.DLM$s) + hwid %o% c(-1, 1))



# ----------
# Kalman-Filtering
dlmSmoothed_obj <- dlmSmooth(y = y, mod = mod)

s <- dropFirst(dlmSmoothed_obj$s)

s_sdev <- sqrt(
  dropFirst(as.numeric(
    dlmSvd2var(dlmSmoothed_obj$U.S, dlmSmoothed_obj$D.S)
  ))
)

# 50%
s_quant <- list(s + qnorm(0.25, sd = s_sdev), s + qnorm(0.75, sd = s_sdev))




# ------------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------------

# logsumexp to avoid underflow

normalize <- function(l){
  max_ind <- which.max(l)
  
  # scaling
  return(
    l - l[max_ind] -
      log1p(sum(exp(l[-max_ind] - l[max_ind])))
  )
}



sys_resampling <- function(N, w){
  w <- exp(w)
  
  sfun <- stepfun(x = cumsum(w), y = 1:(N+1))
  
  sfun((1:N - runif(n = 1)) / N)
}



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

x[1, ] <- rnorm(N, mean = mod$m0, sd = sqrt(mod$C0))



# ----------
# particle weight

w <- matrix(NA_real_, nrow = t_max+1, ncol = N)

#### REVISED (take log)
# w[1, ] <- 1 / N
w[1, ] <- log(1 / N)


dim(w)



# ----------
# forward filtering

for (t in (1:t_max)+1){
  
  # State Equation:  particle
  x[t, ] <- rnorm(N, mean = x[t-1, ], sd = sqrt(mod$W))
  
  # Observation Equation:  update particle weight --> #### REVISED  (take log and "+", not "*")
  # w[t, ] <- w[t-1, ] * dnorm(y[t], mean = x[t, ], sd = sqrt(mod$V))
  w[t, ] <- w[t-1, ] + dnorm(y[t], mean = x[t, ], sd = sqrt(mod$V), log = TRUE)
  
  
  # standardize weight  --> #### REVISED logsumexp to avoid underflow
  # w[t, ] <- w[t, ] / sum(w[t, ])
  w[t, ] <- normalize(w[t, ])
  
  
  # ----------
  # re-sampling
  # k:  index for resampling  (N * (t_max+1)) --> #### REVISED for sampling
  # k[, t] <- sample(1:N, prob = w[t, ], replace = TRUE, size = N)
  k[, t] <- sys_resampling(N = N, w = w[t, ])
  
  # particle
  x[t, ] <- x[t, k[, t]]

  # reset particle weight --> #### REVISED (take log)
  # w[t, ] <- 1 / N
  w[t, ] <- log(1 / N)
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

ts.plot(cbind(y, dropFirst(m), scratch_m),
        col = c("lightgray", "blue", "red"),
        lty = c("solid", "solid", "dashed"))


legend(legend = c("観測値", "平均 （カルマンフィルタリング)",  "平均 （粒子フィルタリング)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)



# ----------
ts.plot(cbind(y, filt[,c(2,3)], do.call("cbind", scratch_m_quant)),
        col = c("lightgray", "blue", "blue", "red", "red"),
        lty = c("solid", "solid", "solid", "dashed", "dashed"))


legend(legend = c("観測値", "50%区間 （カルマンフィルタリング)",  "50%区間 （粒子フィルタリング)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)

