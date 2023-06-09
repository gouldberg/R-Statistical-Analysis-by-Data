rm(list=ls())

setwd("C:\\Users\\kswad\\OneDrive\\fXNgbv\\ZpÍ­»_vðÍ\\51_ðÍXNvg\\z_for_demo_uncompleted\\artifitial_locallevel_model")

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
# Smoothing by rstan MCMC
#   - estimate x (each state) itself
#   - Parameters are unknown
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
stan_mod_out <- stan_model(file = ".\\stan\\stan_model2.stan")



# ----------
# sampling
# W and V are unknown

rm(fit_stan)

dim(mod$m0) <- 1

fit_stan <- sampling(object = stan_mod_out,
                     data = list(t_max = t_max, y = matrix(y, nrow = 1), 
                                 m0 = mod$m0, C0 = mod$C0),
                     pars = c("W", "V", "x"),
                     seed = 123
)




# ----------
fit_stan


# -->
# W = 0.99 and V = 2.07 and standard deviation is small.



# ------------------------------------------------------------------------------
# traceplot
# ------------------------------------------------------------------------------

traceplot(fit_stan, pars = c("W", "V"), alpha = 0.5)



# ------------------------------------------------------------------------------
# extract output
# ------------------------------------------------------------------------------

stan_mcmc_out <- rstan::extract(fit_stan, pars = "x")

str(stan_mcmc_out)




# ------------------------------------------------------------------------------
# compute 50%, 25% and 75%
# ------------------------------------------------------------------------------

s_mcmc <- colMeans(stan_mcmc_out$x)

s_mcmc_quant <- apply(stan_mcmc_out$x, 2, FUN = quantile, probs=c(0.25, 0.75))




# ------------------------------------------------------------------------------
# plot smoothed series and 50% interval
# ------------------------------------------------------------------------------

ts.plot(cbind(y, s), col = c("lightgray", "blue"))

lines(s_mcmc, col = "red", lty = "dashed")

legend(legend = c("Ïªl", "½Ï iJ}½»)",  "½Ï iMCMC)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 50, cex = 0.6)



# ----------
# 50% interval

ts.plot(cbind(y, do.call("cbind", s_quant)),
        col = c("lightgray", "blue", "blue"))

lines(s_mcmc_quant["25%", ], col = "red", lty = "dashed")

lines(s_mcmc_quant["75%", ], col = "red", lty = "dashed")

legend(legend = c("Ïªl", "50%æÔ iJ}½»)",  "50%æÔ iMCMC)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 60, cex = 0.6)

