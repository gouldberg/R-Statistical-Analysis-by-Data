setwd("//media//kswada//MyFiles//R//film90")

packages <- c("dplyr", "gamlss", "gamlss.add")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  film90
# ------------------------------------------------------------------------------
data("film90", package = "gamlss.data")


str(film90)

car::some(film90)



# ------------------------------------------------------------------------------
# Check the model m12 by multiple worm plots
# ------------------------------------------------------------------------------

# multiple worm plots
dtop(m12, xvar = film90$lboopen, n.inter = 9)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

y <- seq(from = 5, to = 17, length.out = 100)


# ----------
# Use the BCPE distribution and take the last observation
# (since we want to calibrate the distribution of the last film)
# The interval 5 to 17 is a suitable range for the pdf of the last observation
mu <- tail(fitted(m12, parameter = "mu"), 1)

sigma <- tail(fitted(m12, parameter = "sigma"), 1)

nu <- tail(fitted(m12, parameter = "nu"), 1)

tau <- tail(fitted(m12, parameter = "tau"), 1)



# ----------
# Calculate corresponding normalized quantile residulas
( residuals.quantile <- pBCPE(y, mu = mu, sigma = sigma, nu = nu, tau = tau) )

( residuals.normalized <- qNO(residuals.quantile) )


plot(residuals.quantile, type = "l")

par(mfrow = c(2,1))
qqnorm(residuals.quantile, col = "black")
qqnorm(residuals.normalized, col = "blue")



# ----------
# Get the pdf of the last observation i = n, evaluated at the sequence y.
density.y <- dBCPE(y, mu = mu, sigma = sigma, nu = nu, tau = tau)

par(mfrow = c(1,1))
plot(density.y, type = "l")



# ----------
# Fit a univariate distribution to all the fitted model m12 residuals
# and get its fitted distribution parameters
residuals.m12 <- resid(m12)

par(mfrow = c(2,2))
qqnorm(residuals.m12, col = "red")
qqnorm(residuals.quantile, col = "black")
qqnorm(residuals.normalized, col = "black")



model.residuals.m12 <- gamlssML(residuals.m12, family = SHASHo)



# ----------
# get the fitted parameters
# Here we are using the SHASHo distribution
mu0 <- tail(fitted(model.residuals.m12, parameter = "mu"), 1)
sigma0 <- tail(fitted(model.residuals.m12, parameter = "sigma"), 1)
nu0 <- tail(fitted(model.residuals.m12, parameter = "nu"), 1)
tau0 <- tail(fitted(model.residuals.m12, parameter = "tau"), 1)



# ----------
# get the pdf evaluated at residuals.quantile
residual.density <- dSHASHo(residuals.normalized, mu = mu0, sigma = sigma0, nu = nu0, tau = tau0)



# ----------
# Calibrate the original pdf
calibrated.density <- (residual.density / dNO(residuals.normalized, mu = 0, sigma = 1)) * density.y



# ----------
par(mfrow = c(1,1))
plot(density.y ~ y, type = "l")
lines(y = calibrated.density, x = y, col = "red")



# ----------
# Calibrate the density using the residuals of the second worm plot in wp()
# "local calibration"
wp(m12, xvar = ~ lboopen + lnosc, n.inter = 2, ylim.worm = 2)






