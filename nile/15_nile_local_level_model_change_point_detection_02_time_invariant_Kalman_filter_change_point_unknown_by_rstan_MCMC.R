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



# ----------
y <- Nile


t_max <- length(y)



# ----------
# basic model
nStateVar <- 1
nHyperParam <- 2
n <- length(y)

funModel <- function(parm){ dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2])) }
oMLE.DLM <- dlmMLE(y, parm = rep(0, 2), build = funModel, hessian = T)


mod <- funModel(oMLE.DLM$par)

mod$W
mod$V



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
# Smoothing by plug-in  (not using FFBS algorithm)
# ------------------------------------------------------------------------------

mod_MCMC <- mod

mod_MCMC$W[1, 1] <- summary(fit_stan)$summary["W[1,1]", "mean"]

mod_MCMC$V[1, 1] <- summary(fit_stan)$summary["V[1,1]", "mean"]

cat(mod_MCMC$W, mod_MCMC$V, "\n")



# ----------
# smoothing
dlmSmoothed_obj <- dlmSmooth(y = y, mod = mod_MCMC)


s_MCMC <- dropFirst(dlmSmoothed_obj$s)



# ------------------------------------------------------------------------------
# plot smoothed series
# ------------------------------------------------------------------------------

ts.plot(cbind(y, s, s_MCMC),
        lty = c("solid", "dashed", "solid"),
        lwd = c(2,2,2),
        col = c("lightgray", "blue", "red"))


legend(legend = c("観測値", "平均（時不変MCMC)", "平均（時不変カルマン平滑化）"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
       col = c("lightgray", "blue", "red"),
       x = "topright", cex = 0.7)
