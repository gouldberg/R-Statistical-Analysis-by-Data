setwd("//media//kswada//MyFiles//R//tension")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tension
# ------------------------------------------------------------------------------

data("tension", package = "MPsychoR")


str(tension)



# ----------
# convert to fdata object
library(fda.usc)

# tension time series
tension1 <- as.matrix(tension[, 1:800])

cond <- tension$condition


ftension <- fdata(tension1, argvals = seq(1, 80, length.out = 800),
                  names = list(main = "Music tension", xlab = "Time (sec)", ylab = "Tension"))


ftension



# ------------------------------------------------------------------------------
# Compute the first-order derivative:  velocity
# ------------------------------------------------------------------------------

ftension1 <- ftensionNP$fdata.est

deriv1 <- fdata.deriv(ftension1)



names(deriv1)



# ----------
# first person7s velocity
deriv1[1,]



# ------------------------------------------------------------------------------
# plot velocity trajectory for sample
# ------------------------------------------------------------------------------

graphics.off()

# velocity for only sampled person

idx <- 7
# idx <- 10

op <- par(mfrow = c(3,1), mar = c(2,2,2,2) + 0.1)

matplot(t(tension1)[,idx], main = paste0("original Tension: Person "), ylab = "", type = "l", cex.main = 2)
abline(h = 0, lty = 2, col = "darkgray")

plot(ftension1[idx,], main = paste0("Smooth Tension: Person ", idx), ylab = "Functional Tension Values", cex.main = 2)
abline(h = 0, lty = 2, col = "darkgray")

plot(deriv1[idx,], main = "", ylab = "Velocity Tension Values", cex.main = 2)
abline(h = 0, lty = 2, col = "darkgray")

par(op)



# -->
# The person 7 experiences a tension peak at around 30s.
# The velocity reflects the rate of change in tension, that is, how rapidly it rises and how rapidly it drops.
# In some applications the second-order derivative (acceleration) can be of interest as well.
