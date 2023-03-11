setwd("//media//kswada//MyFiles//R//mcycle")

packages <- c("dplyr", "lattice", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on Chapter 7. Introductin GAMs from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  mcycle
# ------------------------------------------------------------------------------

data(mcycle, package = "MASS")

str(mcycle)




# ------------------------------------------------------------------------------
# Residual auto-correlation at lag 1
# (average correlation between each residual and the previous residual)
# ------------------------------------------------------------------------------

# weights
w <- c(rep(400, 20), rep(1,113))

m <- 40


# trial log(sp)
sp <- seq(-13, 12, length = m)

AC1 <- EDF <- rep(0, m)

for (i in 1:m){
  b <- gam(accel ~ s(times, k = 40), data = mcycle, weights = w, sp = exp(sp[i]))
  EDF[i] <- sum(b$edf)
  AC1[i] <- acf(residuals(b), plot = FALSE)$acf[2]
}



# ----------
# plot residual auto-correlation at lag 1 against the model effective degrees of freedom
par(mfrow = c(1,1))
plot(EDF, AC1, type="l")
abline(0, 0, col = 2)



# -->
# positive auto-correlation at very low effective degrees of freedom.
# As effected degrees of freedom increases, the auto-correlation is decreased.

# Smoothers operate by forming weighted averages of neighbouring data, the cause of the negative autocorrelation can be understood by
# examining the simplest weighted average smoother: the k-point running mean.


