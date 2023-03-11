# setwd("//media//kswada//MyFiles//R//divorce//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//divorce//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  divorce
# ------------------------------------------------------------------------------

source("divorce_data.R")

length(dat)

data.ls <- list(J = length(dat), tt = dat)

table(dat)



# ------------------------------------------------------------------------------
# Fit weibull distritbution
# ------------------------------------------------------------------------------

library(gamlss)


gamlss.dist::WEI()


# -->
# WEI is log link for mu and sigma both.


# ----------
gamlss(dat ~ 1, family = WEI)



# ----------
par(mfrow = c(1,1))

m1 <- histDist(dat, family = WEI)


m1



# eta: scale parameter
exp(4.953)


# m: shape parameter
exp(0.249)



# mode = eta * (1 - 1/m) ^ 1/m
exp(4.953) * (1 - 1 / exp(0.249)) ^ (1 / exp(0.249))





# ----------
wp(m1)


