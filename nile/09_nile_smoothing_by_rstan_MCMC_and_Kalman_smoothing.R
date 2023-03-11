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
# Smoothing by rstan MCMC and Kalman smoothing
#   - Not estimate x itself, but set Linear Gaussian DLM (F and G = 1) and estimate W and V
# ------------------------------------------------------------------------------

set.seed(123)

library(rstan)



# ----------
# auto write

# rstan_options(auto_write = TRUE)

options(mc.cores = parallel::detectCores())

theme_set(theme_get() + theme(aspect.ratio = 3/4))



# ----------
# Parameters are unknown
stan_mod_out <- stan_model(file = ".\\stan\\stan_model3.stan")

stan_mod_out



# ----------
# sampling
# W and V are unknown (not estimate x itself)

rm(fit_stan)

dim(mod$m0) <- 1


# G and F are matrix(1)
mod$G

mod$F


fit_stan <- sampling(object = stan_mod_out,
                     data = list(t_max = t_max, y = matrix(y, nrow = 1), 
                                 G = mod$G, F = t(mod$F),
                                 m0 = mod$m0, C0 = mod$C0),
                     pars = c("W", "V"),
                     seed = 123
)




# ----------
fit_stan

mod$W;  mod$V;



# -->
# W = 2664 and V = 14827
# W is much larger than mod$W



# ------------------------------------------------------------------------------
# traceplot
# ------------------------------------------------------------------------------

traceplot(fit_stan, pars = c("W", "V"), alpha = 0.5)



# ------------------------------------------------------------------------------
# extract output
# ------------------------------------------------------------------------------

stan_mcmc_out <- rstan::extract(fit_stan, pars = c("W", "V"))

str(stan_mcmc_out)



# ------------------------------------------------------------------------------
# Smoothing by FFBS algorithm
# ------------------------------------------------------------------------------

it_seq <- seq_along(stan_mcmc_out$V[,1,1])

progress_bar <- txtProgressBar(min = 1, max = max(it_seq), style = 3)



# ----------
# FFBS (Forward Filtering Backward Sampling) 

x_FFBS <- sapply(it_seq, function(it){

  setTxtProgressBar(pb = progress_bar, value = it)
  
  mod$W[1,1] <- stan_mcmc_out$W[it, 1, 1]
  
  mod$V[1,1] <- stan_mcmc_out$V[it, 1, 1]
  
  return(dlmBSample(dlmFilter(y = y, mod = mod)))
})



# ----------
# remove x0
x_FFBS <- t(x_FFBS[-1, ])




# ------------------------------------------------------------------------------
# compute 50%, 25%, 75%
# ------------------------------------------------------------------------------

s_FFBS <- colMeans(x_FFBS)


s_FFBS_quant <- apply(x_FFBS, 2, FUN = quantile, probs = c(0.25, 0.75))




# ------------------------------------------------------------------------------
# plot smoothed series and 50% interval
# ------------------------------------------------------------------------------

ts.plot(cbind(y, s), col = c("lightgray", "blue"), lty = c("solid", "dashed"), lwd = 2)

lines(s_FFBS, col = "red", lty = "solid", lwd = 2)

legend(legend = c("観測値", "平均 （カルマン平滑化)",  "平均 （MCMC+FFBS)"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 50, cex = 0.6)



# ----------
# 50% interval

ts.plot(cbind(y, do.call("cbind", s_quant)),
        col = c("lightgray", "blue", "blue"), lty = c("solid", "dashed"), lwd = 2)

lines(s_FFBS_quant["25%", ], col = "red", lty = "solid", lwd = 2)

lines(s_FFBS_quant["75%", ], col = "red", lty = "solid", lwd = 2)

legend(legend = c("観測値", "50%区間 （カルマン平滑化)",  "50%区間 （MCMC+FFBS)"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 60, cex = 0.6)

