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

dim(m0) <- 1

fit_stan <- sampling(object = stan_mod_out,
                     data = list(t_max = t_max, y = matrix(y, nrow = 1), 
                                 G = matrix(1), F = t(matrix(1)),
                                 m0 = m0, C0 = matrix(C0)),
                     pars = c("W", "V"),
                     seed = 123
)




# ----------
fit_stan



# -->
# mean of W and V are 0.25 and 30.48 ...
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

mod <- dlmModPoly(order = 1, dW = mean(stan_mcmc_out$W), dV = mean(stan_mcmc_out$V), m0 = m0, C0 = C0)

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

ts.plot(cbind(y, smooth[,c(2:3)]),
        col = c("lightgray", "blue", "blue"))

lines(s_FFBS_quant["25%", ], col = "red", lty = "dashed")

lines(s_FFBS_quant["75%", ], col = "red", lty = "dashed")

legend(legend = c("観測値", "50%区間 （カルマン平滑化)",  "50%区間 （MCMC+FFBS)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 60, cex = 0.6)

