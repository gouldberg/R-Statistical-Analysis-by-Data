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
# State-space model Particle filtering
#   - Parameters are known
# ------------------------------------------------------------------------------

t_max <- length(y)


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

w[1, ] <- 1 / N


dim(w)



# ----------
# forward filtering

for (t in (1:t_max)+1){
  
  # State Equation:  particle
  x[t, ] <- rnorm(N, mean = x[t-1, ], sd = sqrt(mod$W))
  
  # Observation Equation:  update particle weight
  w[t, ] <- w[t-1, ] * dnorm(y[t], mean = x[t, ], sd = sqrt(mod$V))

  
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

ts.plot(cbind(y, m, scratch_m),
        col = c("lightgray", "blue", "red"),
        lty = c("solid", "dashed", "solid"), lwd = c(2,2,2))


legend(legend = c("観測値", "平均 （カルマンフィルタリング)",  "平均 （粒子フィルタリング)"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)



# ----------
ts.plot(cbind(y[2:100], do.call("cbind", m_quant)[2:100,]),
        col = c("lightgray", "blue", "blue"),
        lwd = c(2,2,2),
        lty = c("solid", "dashed", "dashed"))

lines(scratch_m_quant[[1]][2:100], col = "red", lty = "solid", lwd = 2)

lines(scratch_m_quant[[2]][2:100], col = "red", lty = "solid", lwd = 2)


legend(legend = c("観測値", "50%区間 （カルマンフィルタリング)",  "50%区間 （粒子フィルタリング)"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)

