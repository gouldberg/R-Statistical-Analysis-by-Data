setwd("//media//kswada//MyFiles//R//gait")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gait
# ------------------------------------------------------------------------------

data("gait", package = "fda")

str(gait)

dim(gait)



# ------------------------------------------------------------------------------
# Plot mean and its velocity and acceleration
# ------------------------------------------------------------------------------

graphics.off()
par(mfcol=c(3,2))

plot(hipfdMean, xlab='', ylab='', ylim=c(0, 80), main='Mean Hip Angle', lwd=2)
abline(v=c(7.5, 14.7), lty='dashed')

plot(deriv(hipfdMean), xlab='', ylab='', main='Hip Angle Velocity', lwd=2)
abline(v=c(7.5, 14.7), h=0, lty='dashed')

plot(deriv(hipfdMean, 2), xlab='', ylab='', main='Hip Angle Acceleration', lwd=2)
abline(v=c(7.5, 14.7), h=0, lty='dashed')


plot(kneefdMean, xlab='', ylab='', ylim=c(0, 80), main='Mean Knee Angle', lwd=2)
abline(v=c(7.5, 14.7), lty='dashed')

plot(deriv(kneefdMean), xlab='', ylab='', main='Knee Angle Velocity', lwd=2)
abline(v=c(7.5, 14.7), h=0, lty='dashed')

plot(deriv(kneefdMean, 2), xlab='', ylab='', main='Knee Angle Acceleration', lwd=2)
abline(v=c(7.5, 14.7), h=0, lty='dashed')



# -->
# The cycle begins at the point where the child's heel under the leg being observed strikes the ground.

# We can see three distinct phases in knee angle of roughly equal durations:

# From time 0 to 7.5:  the leg is bearing the weight of the child by itself, and the knee is close to being straight.
# This corresponds to the small loop in the cycle plot starting just before the marker "l" and up to the cusp.

# From time 7.5 to 14.7:  the knee flexes in order to lift the foot off the ground,
# reaching a maximum mean angle of about 70 degrees.

# From time 14.7 to 20:  the knee is extended to receive the local at the next heel-strike.

# Together the second and third phases look like straight forward harmonic motion.
# A similar analysis of the hip motion reveals only a single harmonic phase.
# We wonder how the hip motion is coupled to knee motion.



# ----------
# alternatively
gaitmeanfd <- mean.fd(gaitfd)

op <- par(mfcol=2:3)
plot(gaitmeanfd)
plot(gaitmeanfd, Lfdobj=1)
plot(gaitmeanfd, Lfdobj=2)
par(op)



# ------------------------------------------------------------------------------
# Phase-Plane Plot
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
phaseplanePlot(gaitfine, kneefdMean,
               labels=list(evalarg=gaittime, labels=1:20),
               xlab='Knee Velocity', ylab='Knee Acceleration')

