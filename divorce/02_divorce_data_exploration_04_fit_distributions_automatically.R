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
# Fit a set of predetermined distributions to the data and choose the best
#   - fitDist() uses gamlssML() to fit a set of predetermined distributions to the data
#     and choose the best according to the GAIC, with default penalty k = 2
# ------------------------------------------------------------------------------

# All of the gamlss.family distributions on the real line have been considered

f1 <- fitDist(dat, type = "realplus", k = 2, trace = TRUE)



# ----------
# best model is BCPEo
# Note that WEI family si still in the 3rd to 5th best
f1$fits


f1$failed



# ------------------------------------------------------------------------------
# Residuals diagnostics
# ------------------------------------------------------------------------------

wp(f1, ylim.all = 0.5)



# -->
# but really bad..



# ------------------------------------------------------------------------------
# Plot best model fitting
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

histDist(dat, family = BCPEo)
