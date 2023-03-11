rm(list=ls())

setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\nile")

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Nile
# ------------------------------------------------------------------------------

data(Nile)


dim(Nile)


str(Nile)


y <- as.vector(Nile)



# ------------------------------------------------------------------------------
# Arima model
# ------------------------------------------------------------------------------

result <- arima(y, order = c(1,0,3), transform.pars = FALSE)
ahat <- result$residuals
result_hat <- y - ahat



# ------------------------------------------------------------------------------
# DLM
# ------------------------------------------------------------------------------
nStateVar <- 1
nHyperParam <- 2
n <- length(y)

funModel <- function(parm){ dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2])) }
oMLE.DLM <- dlmMLE(y, parm = rep(0, 2), build = funModel, hessian = T)

oFitted.DLM <- mod <- funModel(oMLE.DLM$par)


# ----------
# filtering
oFiltered.DLM <- dlmFilter(y, oFitted.DLM)
m <- oFiltered.DLM$m
m_sdev <- sqrt(as.numeric(dlmSvd2var(oFiltered.DLM$U.R, oFiltered.DLM$D.R)))
m <- dropFirst(m)
m_quant <- list(m + qnorm(0.25, sd = m_sdev), m + qnorm(0.75, sd = m_sdev))
m_95 <- list(m + qnorm(0.025, sd = m_sdev), m + qnorm(0.975, sd = m_sdev))


# ----------
# smoothing
oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)
s <- oSmoothed.DLM$s
s_sdev <- sqrt(as.numeric(dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)))
s <- dropFirst(s)
s_sdev <- dropFirst(s_sdev)
s_quant <- list(s + qnorm(0.25, sd = s_sdev), s + qnorm(0.75, sd = s_sdev))
s_95 <- list(s + qnorm(0.025, sd = s_sdev), s + qnorm(0.975, sd = s_sdev))



# ----------
# forecasting
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead = 10, sampleNew = 3)
a <- oFcst.DLM$a



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



kernel_smoothing <- function(realization, w, a){
  w <- exp(w)
  
  mean_realization <- weighted.mean(realization, w)
  
  var_realization <- weighted.mean((realization - mean_realization)^2, w)
  
  mu <- a * realization + (1 - a) * mean_realization
  
  sigma2 <- (1 - a^2) * var_realization
  
  return(list(mu = mu, sigma = sqrt(sigma2)))
}



# quantile function for weighted particle clouds
weighted.quantile <- function(x, w, probs)
{
  ## Make sure 'w' is a probability vector
  if ((s <- sum(w)) != 1)
    w <- w / s
  ## Sort 'x' values
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  ## Evaluate cdf
  W <- cumsum(w)
  ## Invert cdf
  tmp <- outer(probs, W, "<=")
  n <- length(x)
  quantInd <- apply(tmp, 1, function(x) (1 : n)[x][1])
  ## Return
  ret <- x[quantInd]
  ret[is.na(ret)] <- x[n]
  return(ret)
}



# ------------------------------------------------------------------------------
# State-space model Liu and West filtering
# ------------------------------------------------------------------------------

set.seed(4521)


# ----------
# Number of particle filters
N <- 10000

# exponentially weighted moving average for parameters
a <- 0.975

# maximum value for parameter W and V
W_max <- 10 * var(diff(y))

V_max <- 10 * var(y)



# ----------
# recognize prior distribution as time 1
y <- c(NA_real_, y)



# ----------
# set prior distribution
# particle: parameter W

W <- matrix(NA_real_, nrow = t_max+1, ncol = N)

W[1, ] <- log(runif(N, min = 0, max = W_max))


# particle: parameter V
V <- matrix(NA_real_, nrow = t_max+1, ncol = N)

V[1, ] <- log(runif(N, min = 0, max = V_max))


# particle: State
x <- matrix(NA_real_, nrow = t_max+1, ncol = N)

# parameter unknown
x[1, ] <- rnorm(N, mean = 0, sd = sqrt(1e+7))



# ----------
# particle weight

w <- matrix(NA_real_, nrow = t_max+1, ncol = N)

w[1, ] <- log(1 / N)

dim(w)



# ----------
# forward kernel smoothing + auxiliary particle filter

for (t in (1:t_max)+1){
  
  # parameters moving average
  W_ks <- kernel_smoothing(realization = W[t-1, ], w = w[t-1, ], a = a)
  V_ks <- kernel_smoothing(realization = V[t-1, ], w = w[t-1, ], a = a)
  
  
  # ----------
  # resampling
  probs <- w[t-1, ] + dnorm(y[t], mean = x[t-1, ], sd = sqrt(exp(V_ks$mu)), log = TRUE)
  
  k <- sys_resampling(N = N, w = normalize(probs))

  

  # ----------
  W[t, ] <- rnorm(N, mean = W_ks$mu[k], sd = W_ks$sigma)
  V[t, ] <- rnorm(N, mean = V_ks$mu[k], sd = V_ks$sigma)
  
  
  # State Equation:  generate particle realization
  x[t, ] <- rnorm(N, mean = x[t-1, k], sd = sqrt(exp(W[t, ])))
  
  
  # Observation Equation:  update particle weight
  w[t, ] <- dnorm(y[t], mean = x[t, ], sd = sqrt(exp(V[t, ])), log = T) -
    dnorm(y[t], mean = x[t-1, k], sd = sqrt(exp(V_ks$mu[k])), log = T)

  
  # standardize weight
  w[t, ] <- normalize(w[t, ])
}



# ----------
# remove priors
y <- ts(y[-1])

W <- W[-1, , drop = FALSE]

V <- V[-1, , drop = FALSE]

x <- x[-1, , drop = FALSE]

w <- w[-1, , drop = FALSE]



# ------------------------------------------------------------------------------
# compute 50%, 25%, and 75%
# ------------------------------------------------------------------------------

LWF_W_m     <- sapply(1:t_max, function(t){exp(
  weighted.mean(W[t, ], w = exp(w[t, ]))
)})


LWF_W_quant <- lapply(c(0.25, 0.75), function(quant){
  sapply(1:t_max, function(t){exp(
    weighted.quantile(W[t, ], w = exp(w[t, ]), probs = quant)
  )})
})


LWF_V_m     <- sapply(1:t_max, function(t){exp(
  weighted.mean(V[t, ], w = exp(w[t, ]))
)})


LWF_V_quant <- lapply(c(0.25, 0.75), function(quant){
  sapply(1:t_max, function(t){exp(
    weighted.quantile(V[t, ], w = exp(w[t, ]), probs = quant)
  )})
})


LWF_m       <- sapply(1:t_max, function(t){
  weighted.mean(x[t, ], w = exp(w[t, ]))
})


LWF_m_quant <- lapply(c(0.25, 0.75), function(quant){
  sapply(1:t_max, function(t){
    weighted.quantile(x[t, ], w = exp(w[t, ]), probs = quant)
  })
})



# ------------------------------------------------------------------------------
# plot filtered series and 50% interval
# ------------------------------------------------------------------------------

ts.plot(cbind(y, m, LWF_m),
        col = c("lightgray", "blue", "red"),
        lwd = c(2,2,2),
        lty = c("solid", "dashed", "solid"))


legend(legend = c("観測値", "平均 （カルマンフィルタリング)",  "平均 （リュウ・ウエストフィルタ)"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 90, cex = 0.6)



# ----------
ts.plot(cbind(y[2:100], do.call("cbind", m_quant)[2:100,]),
        col = c("lightgray", "blue", "blue"),
        lwd = c(2,2,2),
        lty = c("solid", "dashed", "dashed"))

lines(LWF_m_quant[[1]][2:100], col = "red", lty = "solid", lwd = 2)

lines(LWF_m_quant[[2]][2:100], col = "red", lty = "solid", lwd = 2)


legend(legend = c("観測値", "50%区間 （カルマンフィルタリング)",  "50%区間 （リュウ・ウエストフィルタ)"),
       lty = c("solid", "dashed", "solid"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 90, cex = 0.6)




# ----------
ts.plot(cbind(LWF_W_m, do.call("cbind", LWF_W_quant)),
        lty=c("solid", "dashed", "dashed"), ylab = "W", lwd = c(2,1,1))
abline(h = mod$W, col = "lightgray", lwd = 2)
mtext(sprintf("%g", mod$W), at = mod$W, side = 2, adj = 0, cex = 0.8)

legend(legend = c("真値", "平均値", "50%区間"),
       col = c("lightgray", "black", "black"),
       lty = c("solid", "solid", "dashed"),
       x = "topright", cex = 1.0)



# ----------
ts.plot(cbind(LWF_V_m, do.call("cbind", LWF_V_quant)),
        lty=c("solid", "dashed", "dashed"), ylab = "V", lwd = c(2,1,1))
abline(h = mod$V, col = "lightgray", lwd = 2)
mtext(sprintf("%g", mod$V), at = mod$V, side = 2, adj = 0, cex = 0.8)

legend(legend = c("真値", "平均値", "50%区間"),
       col = c("lightgray", "black", "black"),
       lty = c("solid", "solid", "dashed"),
       x = "topright", cex = 1.0)


