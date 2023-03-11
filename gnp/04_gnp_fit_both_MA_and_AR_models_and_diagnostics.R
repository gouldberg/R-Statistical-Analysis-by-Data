setwd("//media//kswada//MyFiles//R//gnp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  GNP data
# ------------------------------------------------------------------------------

data(gnp, package = "astsa")

str(gnp)

head(gnp)



# ------------------------------------------------------------------------------
# Fit MA(2) model using MLE
# ------------------------------------------------------------------------------

# MA(2) model
# Note that sarim fit drift automatically  (if no constant, "no.constant = TRUE")
mod_ma <- sarima(diff(log(gnp)), 0, 0, 2)

mod_ma


# -->
# estimated MA(2) model:
# x(t) = 0.008 + 0.303 * w(t-1) + 0.204 * w(t-2) + w(t)


# All of the regression coefficients are significant, including the constant.
# We make a special note of this because, as a default, some computer packages do not fit a constant in a differenced model.
# That is, these packages assume, by default, that there is no drift.


# -->
# Model diagnostics:
# Inspection of the time plot of the standardized residuals shows no obvious patterns.
# Notice that there may be outliers, with a few values exceeding 3 standard deviations in magnitude.
# The ACF of the standardized residuals shows no apparent departure from the model assumptions, and the Q-statistic
# is never significant at the lags shown.
# The normal Q-Q plot of the residuals shows that the assumption of normality is reasonable, with the exception of the possible outliers.

# The model appears to fit well.



# ------------------------------------------------------------------------------
# Fit AR(1) model using MLE
# ------------------------------------------------------------------------------

# AR(1) model
mod_ar <- sarima(diff(log(gnp)), 1, 0, 0)

mod_ar


# -->
# estimated MA(2) model:
# x(t) = 0.008 * (1 - 0.347) + 0.347 * x(t-1) + w(t)

# Note that the constant is 0.008 * (1 - 0.347) = 0.005


# -->
# in causal form:  x(t) = 0.35 * w(t-1) + 0.35^2 * w(t-2) + 0.35^3 + w(t-3) + ...
# --> almost x(t) = 0.35 * w(t-1) + 0.12 * w(t-2) + w(t)
# which is similar to the fitted MA(2) model



# ------------------------------------------------------------------------------
# Psi-weights
# ------------------------------------------------------------------------------

ARMAtoMA(ar = 0.35, ma = 0, 10)


