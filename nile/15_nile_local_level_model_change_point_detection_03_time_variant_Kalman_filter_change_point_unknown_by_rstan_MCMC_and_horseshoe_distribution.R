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
# horseshoe distribution
# ------------------------------------------------------------------------------

set.seed(123)

dLaplace <- function(x){ 1/2 * exp(-abs(x)) }

sample_size <- 1e+5

lambda <- abs(rcauchy(sample_size, location = 0, scale = 1))

horseshoe <- rnorm(sample_size, sd = lambda * 1)


breaks <- function(samples){
        return(c(
                c(min(samples), -3), seq(from = -3, to = 3, by = 0.1), c(3, max(samples))
        ))
}

xlim <- c(-3, 3); ylim <- c(0, 0.6)



# ----------
#curve(dnorm(x),
#      xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i",
#      xlab = "実現値", ylab = "密度")

curve(dcauchy(x),
      xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i",
      xlab = "実現値", ylab = "密度", lty = "solid")

curve(dLaplace(x), add = TRUE,
      xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i", xlab = "", ylab = "",
      lty = "dashed")

hist(horseshoe, breaks = breaks(horseshoe), add = TRUE,
     xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i", xlab = "", ylab = "",
     col = "#80808040", border = "#80808040")

legend(legend = c("コーシー分布", "ラプラス分布", "馬蹄分布"),
       col = c("black", "black", "lightgray"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(1, 1, 5),
       x = "topright", cex = 0.7)



# ------------------------------------------------------------------------------
# Smoothing by rstan MCMC with horseshoe distribution:  time varying MCMC
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
stan_mod_out <- stan_model(file = ".\\stan\\stan_model4.stan")

stan_mod_out



# ----------
# sampling

rm(fit_stan)


fit_stan <- sampling(object = stan_mod_out,
                     data = list(t_max = t_max, y = y,
                                 miss = as.integer(is.na(y)),
                                 m0 = modtv$m0, C0 = modtv$C0[1,1]),
                     pars = c("lambda", "W_sqrt", "V_sqrt"),
                     seed = 123
)



# ----------
fit_stan

print(fit_stan, probs = c(0.025, 0.5, 0.975))




# ------------------------------------------------------------------------------
# traceplot
# ------------------------------------------------------------------------------

traceplot(fit_stan, pars = c("W_sqrt", "V_sqrt"), alpha = 0.5)



# ------------------------------------------------------------------------------
# Smoothing by plug-in  (not using FFBS algorithm)
# ------------------------------------------------------------------------------

modtv_MCMC <- modtv

modtv_MCMC$X[ , 1] <- (summary(fit_stan)$summary[   1:100, "mean"] *
                               summary(fit_stan)$summary["W_sqrt", "mean"])^2

modtv_MCMC$V[1, 1] <- (summary(fit_stan)$summary["V_sqrt", "mean"])^2

as.vector(modtv_MCMC$X); modtv_MCMC$V



# ----------
dlmSmoothed_obj <- dlmSmooth(y = y, mod = modtv_MCMC)

stv_MCMC <- dropFirst(dlmSmoothed_obj$s)



# ------------------------------------------------------------------------------
# plot smoothed series
# ------------------------------------------------------------------------------

ts.plot(cbind(y, stv, stv_MCMC),
        lty = c("solid", "dashed", "solid"),
        lwd = c(2,2,2),
        col = c("lightgray", "blue", "red"))


legend(legend = c("観測値", "平均（時不変カルマン平滑化）", "平均（時変MCMC：馬蹄分布)"),
       lty = c("solid", "dashed", "solid"),
       lwd = c(2,2,2),
       col = c("lightgray", "blue", "red"),
       x = "topright", cex = 0.7)
