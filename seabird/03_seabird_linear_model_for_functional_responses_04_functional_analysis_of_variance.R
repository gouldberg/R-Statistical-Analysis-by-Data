setwd("//media//kswada//MyFiles//R//canadian_weather")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  seabird
# ------------------------------------------------------------------------------
data("seabird", package = "fda")

str(seabird)

car::some(seabird)



# ------------------------------------------------------------------------------
# Set up functional parameter object
# ------------------------------------------------------------------------------
# For intercept and food coefficients, use cubic b-spline basis

betabasis1 = create.bspline.basis(c(1, 20), 21, 4, yearCode)


# but here we set 10^1 (not 10^0.5)
lambda = 10
betafdPar1 = fdPar(betabasis1, 2, lambda)

betalist[[1]] = betafdPar1

betalist[[2]] = betafdPar1



# wer only want constant functions for the bird regression coefficient functions effects since there are only the mean counts
# at the two sites available to estimate any bird's effect.
betabasis2 = create.constant.basis(c(1,20))

betafdPar2 = fdPar(betabasis2)

for (j in 3:15) betalist[[j]] = betafdPar2



# ------------------------------------------------------------------------------
# Functional analysis of variance
# ------------------------------------------------------------------------------

birdRegress = fRegress(birdfd3, xfdlist, betalist)

betaestlist = birdRegress$betaestlist



# ----------
# plot the regression parameters without the confidence intervals

op = par(mfrow=c(2,1), ask = FALSE)
plot(betaestlist$const$fd, main = "Intercept")
plot(betaestlist$diet$fd, main = "Food Effect")
par(op)



# ------------------------------------------------------------------------------
# Plot predicted functions for each combination of birds and bay
# ------------------------------------------------------------------------------
#  plot predicted functions

birdYhatfdobj = birdRegress$yhatfdobj

plotfit.fd(logCounts2, yearCode, birdYhatfdobj$fd[1:26])
# *** Click on the plot to advance to the next ...



# ------------------------------------------------------------------------------
# Functional analysis of variance:  here we try lambda = 10^0.5
# ------------------------------------------------------------------------------
betafdPar1$lambda = 10^0.5

for (j in 1:2) betalist[[j]] = betafdPar1

fitShellfish.5 = fRegress(birdfd3, xfdlist, betalist)

betaestlist.5 = fitShellfish.5$betaestlist


# ----------
# plot and compare lambd = 10^1 (blue) and 10^0.5
op = par(mfrow=c(2,1), ask = FALSE)
plot(betaestlist$const$fd, main = "Intercept", col = "blue", lty = 1, lwd = 2)
lines(betaestlist.5$const$fd, col = "black", lty = 2, lwd = 1)

plot(betaestlist$diet$fd, main = "Food Effect", col = "blue", lty = 1, lwd = 2)
lines(betaestlist.5$diet$fd, lty = 2, lwd = 1)
par(op)


