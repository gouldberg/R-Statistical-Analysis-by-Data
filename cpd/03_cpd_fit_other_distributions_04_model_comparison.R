setwd("//media//kswada//MyFiles//R//cpd")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cpd
# ------------------------------------------------------------------------------

data(cpd, package="faraway")

str(cpd)

car::some(cpd)



# ------------------------------------------------------------------------------
# plot the predicted lines
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))
plot(actual ~ projected, cpd)
abline(lmod)
abline(igi, lty = 2, col = "blue")
abline(gl, lty = 3, col = "gray")
abline(llg, lty = 4, col = "red")


# -->
# We can see that there is a cleaar difference in the estimates of the slope.



# ------------------------------------------------------------------------------
# diagnostics:  residuals vs. fitted values
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))
plot(lmod, 1)
plot(llg, 1)
plot(gl, 1)
plot(igi, 1)



# ------------------------------------------------------------------------------
# diagnostics:  sqrt of standardized deviance residuals
# ------------------------------------------------------------------------------
par(mfrow = c(2,2))
plot(lmod, 3)
plot(llg, 3)
plot(gl, 3)
plot(igi, 3)



# ------------------------------------------------------------------------------
# diagnostics:  deviance residuals plot
# ------------------------------------------------------------------------------

# Deviance residuals plot for inverse Gaussian model and gamma GLM model
par(mfrow = c(1,2))
plot(residuals(igi) ~ log(fitted(igi)), ylab = "Deviance residulas", xlab = expression(log(hat(mu))))
abline(h = 0)

plot(residuals(gl) ~ log(fitted(gl)), ylab = "Deviance residulas", xlab = expression(log(hat(mu))))
abline(h = 0)



# -->
# We see that the variance of the residuals is decreasing with error for inverse Gaussian model,
# indicating that the inverse Gaussian variance function is too strong for this data.
# We have used log(mu) so that the points are more evenly spread horizontally making it easier.

# A Gamma GLM is a better choice here.


