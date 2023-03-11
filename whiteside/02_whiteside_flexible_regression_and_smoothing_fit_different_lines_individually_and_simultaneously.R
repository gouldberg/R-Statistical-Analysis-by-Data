setwd("//media//kswada//MyFiles//R//whiteside")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  whiteside
# ------------------------------------------------------------------------------
data("whiteside", package = "MASS")


str(whiteside)

car::some(whiteside)



# ------------------------------------------------------------------------------
# Flexible regression and smoothing:
# Fit two different lines in an analysis of covariance by update()
# ------------------------------------------------------------------------------

gasB <- gamlss(Gas ~ Temp, data = subset(whiteside, Insul == "Before"))



# ----------
# Update() can be used to fit two different lines in an analysis of covariance.
gasA <- update(gasB, data = subset(whiteside, Insul == "After"))



# ----------
# Show the gas consumption against the average outside temperature in degrees Celsius, before and after insulation.
with(whiteside, plot(Temp, Gas, pch = (16:17)[unclass(Insul)], col = c("red", "green3")[unclass(Insul)]))
with(whiteside, lines(Temp[Insul == "Before"], fitted(gasB), col = "red", lwd = 2))
with(whiteside, lines(Temp[Insul == "After"], fitted(gasA), col = "green3", lty = 2, lwd = 2))
legend("topright", pch = 16:17, lty = 1:2, lwd = 2, col = c("red", "green3"), legend = c("before", "after"))



# ------------------------------------------------------------------------------
# Flexible regression and smoothing:
# Fit complete model
# ------------------------------------------------------------------------------

a1 <- gamlss(Gas ~ Temp * Insul, sigma.fo =~Insul, data = whiteside)



# ----------
# Deviance of gasB and gasA add up to the deviance of the combined model.
deviance(a1)
deviance(gasA)
deviance(gasB)



# ----------
# Show the gas consumption against the average outside temperature in degrees Celsius, before and after insulation.
with(whiteside, plot(Temp, Gas, pch = 21, bg = c("red", "green3")[unclass(Insul)]))
posA <- which(whiteside$Insul == "After")
posB <- which(whiteside$Insul == "Before")
with(whiteside, lines(fitted(a1)[posB] ~ whiteside$Temp[posB], pch = 19))
with(whiteside, lines(fitted(a1)[posA] ~ whiteside$Temp[posA], pch = 19))



# ----------
coef(a1)
coef(gasA)
coef(gasB)



# ----------
# Compare the estimates for the sigma parameter. Note that models gasB and gasA have different estimates for this parameter.
coef(a1, "sigma")
coef(gasA, "sigma")
coef(gasB, "sigma")


