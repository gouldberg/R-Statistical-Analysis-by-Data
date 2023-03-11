setwd("//media//kswada//MyFiles//R//rugged")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rugged
# ------------------------------------------------------------------------------

data("rugged", package = "rethinking")

d <- rugged

dim(d)

str(d)


car::some(d)



# ------------------------------------------------------------------------------
# Fit two-component distribution models
# ------------------------------------------------------------------------------

library(gamlss.mx)

set.seed(3683)



# ----------
# 2-component mixtures of normal, gamma, inverse Gaussian, Weibull, reverse Gumbel and logistic distributions

K <- 3

gNO <- gamlssMX(rgdppc_2000 ~ 1, family = NO, K = K, data = na.omit(d))

gGA <- gamlssMX(rgdppc_2000 ~ 1, family = GA, K = K, data = na.omit(d))

gIG <- gamlssMX(rgdppc_2000 ~ 1, family = IG, K = K, data = na.omit(d))

gWEI <- gamlssMX(rgdppc_2000 ~ 1, family = WEI, K = K, data = na.omit(d))

gRG <- gamlssMX(rgdppc_2000 ~ 1, family = RG, K = K, data = na.omit(d))

gLO <- gamlssMX(rgdppc_2000 ~ 1, family = LO, K = K, data = na.omit(d))



# ----------
# Compare models
AIC(gNO, gGA, gIG, gWEI, gRG, gLO)



# -->
# The best model appears to be the two-component Inverse Gausiaan (IG) model  --> Gamma model (GA)



# ------------------------------------------------------------------------------
# Verify that we have reached a global maximum
# ------------------------------------------------------------------------------

# We can verify that we have reached a global maximum using the function gamlssMXfits()
# n = 5:  fitting 5 models with different starting values

gIG1 <- gamlssMXfits(n = 5, rgdppc_2000 ~ 1, family = IG, K = 3, data = na.omit(d))
gGA1 <- gamlssMXfits(n = 5, rgdppc_2000 ~ 1, family = GA, K = 3, data = na.omit(d))



# ----------
# The resulting fitted model gIG1 is quite differenct from gIG
gIG

gIG1

gGA

gGA1



# ----------
# models
gIG1$models

gGA1$models



# ------------------------------------------------------------------------------
# Plot the two-component distributions
# ------------------------------------------------------------------------------

# As the Inverse Gaussian distribution has default log link for mu and log link for sigma,
# the estimated model is given by

gIG1
gGA1

MASS::truehist(d$rgdppc_2000, nbins = 25)


x <- seq(0, 60000, 100)
plot(0.7395 * dIG(x, mu = exp(7.129), sigma = exp(-4.29)) + 0.2605 * dIG(x, mu = exp(8.842), sigma = exp(-5.245)), type = "l")



# ---------
# getpdfMX() creates a new function which can hen be used to plot the fitted pdf for a specified observation.
# here wer choose observation 1 since a single distribution is fitted with no explanatory variables,
# and the fitted mixture distribution applies to all observations

# We choose observation 1 since a single distribution is fitted with no explanatory variables, and the fitted mixture distribution applis to all observations

fnIG <- getpdfMX(gIG1, observation = 1)

fnGA <- getpdfMX(gGA1, observation = 1)

truehist(d$rgdppc_2000, nbins = 25, col = "grey", xlab = "rgdppc_2000", ylab = "Density", ymax = 0.00015)

x <- seq(0, 60000, 100)

lines(x, fnIG(x), lty = 1, lwd = 2)
lines(x, fnGA(x), lty = 2, lwd = 2)
legend("topleft", legend = c("inverse gaussian", "gamma"), lty = 1:2, cex = 1.5)



# -->
# not good fit ....



# ------------------------------------------------------------------------------
# residual plot
# ------------------------------------------------------------------------------

plot(gGA1)


# -->
# Here we have fitted a constant model for mu, so all fitted values are the same and should lie in a vertical line in the plot.
# The fact that one of them does not appear to do so is beacuase of a small numerical discrepancy.
# This feature appears often in cases when no explanatory variables are involved in the fit, and should be ignored.


