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
# Examine the residuals:  cross-correlations of the residulas
# ------------------------------------------------------------------------------

# Acf2 can not be used for multivariate time series ...

acf(resid(fit), lag.max = 20)


acf(resid(fit), 20)$acf



# -->
# ACFs of the individual residual series along the diagonal.
# The off diagonals display the CCFs between pairs of residual series.
# Note that second named series is the one that leads.

# Notice that most of the correlations in the residual series are negliible,
# however, note that there are some zero-order correlation

# This means that the AR model is not capturing the concurrent effect




# ------------------------------------------------------------------------------
# Examine the multivariate version of the Q-test
# ------------------------------------------------------------------------------


vars::serial.test(fit, lags.pt = 12, type = "PT.adjusted")



# -->
# Thus, not unexpectedly, the Q-test rejects the null hypothesis that the noise is white.

