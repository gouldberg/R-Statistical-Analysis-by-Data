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
# Smoothing by rstan MCMC
#   - estimate x (each state) itself
#   - Parameters are known  (estimated by DLM first)
# ------------------------------------------------------------------------------

set.seed(123)

library(rstan)



# ----------
# auto write

# rstan_options(auto_write = TRUE)

options(mc.cores = parallel::detectCores())

theme_set(theme_get() + theme(aspect.ratio = 3/4))



# ----------
# Parameters are known
stan_mod_out <- stan_model(file = ".\\stan\\stan_model1.stan")

stan_mod_out


# ----------
# sampling

t_max <- length(y)

rm(fit_stan)

fit_stan <- sampling(object = stan_mod_out,
                     data = list(t_max = t_max, y = y, 
                                 W = mod$W, V = mod$V, 
                                 m0 = mod$m0, C0 = mod$C0),
                     pars = c("x"),
                     seed = 123
)



# ----------
fit_stan



# ------------------------------------------------------------------------------
# traceplot
# ------------------------------------------------------------------------------

traceplot(fit_stan, pars = c(sprintf("x[%d]", 100), "lp__"), alpha = 0.5)



# ------------------------------------------------------------------------------
# extract output
# ------------------------------------------------------------------------------

stan_mcmc_out <- rstan::extract(fit_stan, pars = "x")

str(stan_mcmc_out)




# ------------------------------------------------------------------------------
# compute 50%, 25%, 75%
# ------------------------------------------------------------------------------

s_mcmc <- colMeans(stan_mcmc_out$x)

s_mcmc_quant <- apply(stan_mcmc_out$x, 2, FUN = quantile, probs=c(0.25, 0.75))




# ------------------------------------------------------------------------------
# plot smoothed series and 50% interval
# ------------------------------------------------------------------------------

ts.plot(cbind(y, s), col = c("lightgray", "blue"), lty = c("solid", "dashed"), lwd = 2)

lines(s_mcmc, col = "red", lty = "solid", lwd = 2)

legend(legend = c("観測値", "平均 （カルマン平滑化)",  "平均 （MCMC)"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 50, cex = 0.6)



# ----------
# 50% interval
ts.plot(cbind(y, do.call("cbind", s_quant)),
        col = c("lightgray", "blue", "blue"), lty = c("solid", "dashed"), lwd = 2)

lines(s_mcmc_quant["25%", ], col = "red", lty = "solid", lwd = 2)

lines(s_mcmc_quant["75%", ], col = "red", lty = "solid", lwd = 2)

legend(legend = c("観測値", "50%区間 （カルマン平滑化)",  "50%区間 （MCMC)"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 60, cex = 0.6)

