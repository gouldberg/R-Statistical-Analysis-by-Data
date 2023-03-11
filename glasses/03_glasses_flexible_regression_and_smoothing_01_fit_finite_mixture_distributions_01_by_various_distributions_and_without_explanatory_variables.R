setwd("//media//kswada//MyFiles//R//glasses")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  glasses
# ------------------------------------------------------------------------------
data("glasses", package = "gamlss.data")


str(glasses)

car::some(glasses)



# ------------------------------------------------------------------------------
# Fit two-component distribution models
# ------------------------------------------------------------------------------

library(gamlss.mx)

set.seed(3683)



# ----------
# 2-component mixtures of normal, gamma, inverse Gaussian, Weibull, reverse Gumbel and logistic distributions
readNO <- gamlssMX(ageread ~ 1, family = NO, K = 2, data = glasses)

readGA <- gamlssMX(ageread ~ 1, family = GA, K = 2, data = glasses)

readIG <- gamlssMX(ageread ~ 1, family = IG, K = 2, data = glasses)

readWEI <- gamlssMX(ageread ~ 1, family = WEI, K = 2, data = glasses)

readRG <- gamlssMX(ageread ~ 1, family = RG, K = 2, data = glasses)

readLO <- gamlssMX(ageread ~ 1, family = LO, K = 2, data = glasses)



# ----------
# Compare models
AIC(readNO, readGA, readIG, readWEI, readRG, readLO)


# -->
# The best model appears to be the two-component logistic (LO) model, although the gamma (GA) model gives a very comparable
# fit and may be preferred (because it has a positive response variable)



# ------------------------------------------------------------------------------
# Verify that we have reached a global maximum
# ------------------------------------------------------------------------------

# We can verify that we have reached a global maximum using the function gamlssMXfits()
# n = 5:  fitting 5 models with different starting values
readLO1 <- gamlssMXfits(n = 5, ageread ~ 1, family = LO, K = 2, data = glasses)



# ----------
# The resulting fitted model readLO1 is unchanged from readLO
readLO
readLO1



# ----------
# models
readLO1$models



# ------------------------------------------------------------------------------
# Plot the two-component distributions
# ------------------------------------------------------------------------------
# As the logistic distribution has default identity link for mu and log link for sigma,
# the estimated model is given by

x <- seq(0, 80, 1)
plot(0.17 * dLO(x, mu = 18.86, sigma = exp(1.492)) + 0.83 * dLO(x, mu = 47.04, sigma = exp(1.553)), type = "l")



# ---------
# getpdfMX() creates a new function which can hen be used to plot the fitted pdf for a specified observation.
# here we choose observation 1 since a single distribution is fitted with no explanatory variables,
# and the fitted mixture distribution applies to all observations

# We choose observation 1 since a single distribution is fitted with no explanatory variables, and the fitted mixture distribution applis to all observations

fnLO <- getpdfMX(readLO, observation = 1)
fnGA <- getpdfMX(readGA, observation = 1)

truehist(glasses$ageread, nbins = 25, col = "grey", xlab = "Age", ylab = "Density", ymax = 0.05)
lines(seq(0.5, 90.5, 1), fnLO(seq(0.5, 90.5, 1)), lty = 1, lwd = 2)
lines(seq(0.5, 90.5, 1), fnGA(seq(0.5, 90.5, 1)), lty = 2, lwd = 2)
legend("topleft", legend = c("logistic", "gamma"), lty = 1:2, cex = 1.5)



# ------------------------------------------------------------------------------
# residual plot
# ------------------------------------------------------------------------------

plot(readLO)


# -->
# Here we have fitted a constant model for mu, so all fitted values are the same and should lie in a vertical line in the plot.
# The fact that one of them does not appear to do so is beacuase of a small numerical discrepancy.
# This feature appears often in cases when no explanatory variables are involved in the fit, and should be ignored.


