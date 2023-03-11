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
oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)
m <- oFiltered.DLM$m
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



# -->
# mean of W and V are 0.98 and 2.07  (close to true value)
# Note that effective sample size and Rhat is improved from previous models



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
# FFBS (Forward Filtering Backward Samppling) 

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

ts.plot(cbind(y, s), col = c("lightgray", "blue"))

lines(s_FFBS, col = "red", lty = "dashed")

legend(legend = c("観測値", "平均 （カルマン平滑化)",  "平均 （MCMC+FFBS)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 50, cex = 0.6)



# ----------
# 50% interval

ts.plot(cbind(y, do.call("cbind", s_quant)),
        col = c("lightgray", "blue", "blue"))

lines(s_FFBS_quant["25%", ], col = "red", lty = "dashed")

lines(s_FFBS_quant["75%", ], col = "red", lty = "dashed")

legend(legend = c("観測値", "50%区間 （カルマン平滑化)",  "50%区間 （MCMC+FFBS)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 60, cex = 0.6)

