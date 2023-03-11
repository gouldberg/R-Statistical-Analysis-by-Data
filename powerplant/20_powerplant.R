setwd("//media//kswada//MyFiles//R//powerplant")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS", "timsac")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Power Plant
#   - 500 observations on 3 variables:  comamnd, temperature and fuel
# ------------------------------------------------------------------------------

data(Powerplant, package = "timsac")


dim(Powerplant)


str(Powerplant)


head(Powerplant)


colnames(Powerplant) <- c("command", "temperature", "fuel")




# ------------------------------------------------------------------------------
# Multivariate Bayesian Method of AR Model Fitting
# ------------------------------------------------------------------------------


z <- mulbar(Powerplant, max.order = 10, plot = TRUE)



# ----------
names(z)



# ----------
# innovation variance
z$v


# innovation variance of the Bayesian model
z$v.bay



# ----------
# partial autoregression coefficients (forward model)
z$pacoef.for


# partial autoregression coefficients (backward model)
z$pacoef.back





tmp <- fpec(Powerplant, max.order = 10)

names(tmp)

tmp$fpec

