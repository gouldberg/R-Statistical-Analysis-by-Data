setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")

str(econ5)


car::some(econ5)




# ------------------------------------------------------------------------------
# Data transformation
# ------------------------------------------------------------------------------

# First logging and then removing the linear trend

U <- resid(lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(lm(log(econ5$consum) ~ time(log(econ5$consum))))


x <- cbind(G, U, C)


MTSplot(x)




# ------------------------------------------------------------------------------
# Select VAR model by VARselect
# ------------------------------------------------------------------------------

# vars fit vector AR models via least squares.
library(vars)



# ----------
# detrended time series
# "none" fits no constant and no trend

VARselect(x, lag.max = 10, type = "none")




# -->
# Note that BIC picks the order p = 2 model while AIC, FPE and Hannan-Quinn select order p = 3 model




# ------------------------------------------------------------------------------
# Fit VAR model by p = 2
# ------------------------------------------------------------------------------

summary(fit <- VAR(x, p = 2, type = "none"))

