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
# Influence Matrix
# ------------------------------------------------------------------------------

n <- nrow(mcycle)


# influence matrix A, mapping the response data to the fitted values, given a smoothing paramter
A <- matrix(0, n, n)

k <- 40

# sp: smoothing parameter
for (i in 1:n) {
  mcycle$y <- mcycle$accel * 0
  mcycle$y[i] <- 1
  A[,i] <- fitted(gam(y ~ s(times, k = k), data = mcycle, sp = mod_gam$sp))
}


rowSums(A)



# ------------------------------------------------------------------------------
# Equivalent kernel of the fitted spline
#   - Any smoothing model that can be represented as mu = A * y, simply replaces each value y
#     by a weighted sum of neighbouring y values.
# ------------------------------------------------------------------------------

# plot the weights (= equivalent kernel of the fitted spline)
# used to form mu of 65th datum
par(mfrow=c(1, 1))
i <- 65
plot(mcycle$times, A[,i], type = "l", ylim=c(-0.05, 0.15))




# ----------
# plot all the equivalent kernels
# Note that peak heights vary.

plot(mcycle$times, A[,i], type = "l", ylim=c(-0.05, 0.15))
for (i in 1:n) lines(mcycle$times, A[,i])




# ------------------------------------------------------------------------------
# Vary smoothing parameter around the GCV selected value and check equivalent kernel
# ------------------------------------------------------------------------------

mcycle$y <- mcycle$accel * 0

mcycle$y[65] <- 1


par(mfrow=c(2,2))

for (lambda in 1:4)
  plot(mcycle$times, fitted(gam(y ~ s(times, k = k), data = mcycle, sp = mod_gam$sp * 10 ^ (lambda - 1.5))), type = "l", ylab = "A[65,]", ylim = c(-0.01,0.12))





