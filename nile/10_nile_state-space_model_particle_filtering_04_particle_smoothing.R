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

ts.plot(cbind(y, s, scratch_s),
        col = c("lightgray", "blue", "red"),
        lwd = c(2,2,2),
        lty = c("solid", "dashed", "solid"))


legend(legend = c("観測値", "平均 （カルマン平滑化)",  "平均 （粒子平滑化）"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)



# ----------
ts.plot(cbind(y, do.call("cbind", s_quant), do.call("cbind", scratch_s_quant)),
        lwd = c(2,2,2),
        col = c("lightgray", "blue", "blue", "red", "red"),
        lty = c("solid", "dashed", "dashed", "solid", "solid"))


legend(legend = c("観測値", "50%区間 （カルマン平滑化)",  "50%区間 （粒子平滑化)"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
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
    rho[t, ] <- w[t, ] * dnorm(b[t+1, , path], mean = x[t, ], sd = sqrt(mod$W))
    
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
ts.plot(cbind(y, s, scratch_s),
        col = c("lightgray", "blue", "red"),
        lwd = c(2,2,2),
        lty = c("solid", "dashed", "solid"))


legend(legend = c("観測値", "平均 （カルマン平滑化)",  "平均 （粒子平滑化）"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)



# ----------
ts.plot(cbind(y, do.call("cbind", s_quant), do.call("cbind", scratch_s_quant)),
        lwd = c(2,2,2),
        col = c("lightgray", "blue", "blue", "red", "red"),
        lty = c("solid", "dashed", "dashed", "solid", "solid"))


legend(legend = c("観測値", "50%区間 （カルマン平滑化)",  "50%区間 （粒子平滑化)"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)
