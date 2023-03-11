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
# State-space model Particle filtering:  Particle prediction
#   - Parameters are known
# ------------------------------------------------------------------------------

set.seed(4521)

x <- rbind(x, matrix(NA_real_, nrow = 10, ncol = N))

w <- rbind(w, matrix(NA_real_, nrow = 10, ncol = N))



# ----------
# time forward process
for (t in t_max+(1:10)){
  
  # State Equatoin
  x[t, ] <- rnorm(N, mean = x[t-1, ], sd = sqrt(mod$W))
  
  # Update particle weight
  w[t, ] <- w[t-1, ]
}




# ------------------------------------------------------------------------------
# compute 50%, 25% and 75%
# ------------------------------------------------------------------------------

scratch_a       <- sapply(t_max+(1:10), function(t){
  mean(x[t, ])
})


scratch_a_quant <- lapply(c(0.25, 0.75), function(quant){
  sapply(t_max+(1:10), function(t){
    quantile(x[t, ], probs = quant)
  })
})



scratch_a <- ts(scratch_a, start = t_max+1)


scratch_a_quant <- lapply(scratch_a_quant, function(dat){
  ts(dat, start = t_max+1)
})



# ------------------------------------------------------------------------------
# plot prediction and 50% interval
# ------------------------------------------------------------------------------

a <- ts(a, start = 101, end = 110)

ts.plot(cbind(y, a, scratch_a),
        col = c("lightgray", "blue", "red"),
        lwd = c(2,2,2),
        lty = c("solid", "dashed", "solid"))


legend(legend = c("観測値", "平均 （カルマン予測)",  "平均 （粒子予測)"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)



# ----------
ts.plot(cbind(y, do.call("cbind", scratch_a_quant)),
        lwd = c(2,2,2),
        col = c("lightgray", "red", "red"),
        lty = c("solid", "solid", "solid"))


legend(legend = c("観測値", "50%区間 （粒子予測)"),
       lty = c("solid", "solid"),
       lwd = c(2,2),
       col = c("lightgray", "red"),
       x = "topright", text.width = 70, cex = 0.6)
