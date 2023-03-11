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
# Explore the relationship betwen knee and hip acceleration
# ------------------------------------------------------------------------------

xfdlist2 = list(const=rep(1,39), hip=deriv(hipfd, 2))

kneefd.accel = deriv(kneefd, 2)

gaitAccelRegr = fRegress(kneefd.accel, xfdlist2, betalist)


gaitt3 = seq(0, 20, length=401)

beta.hipFine = predict(gaitAccelRegr$betaestlist$hip$fd, gaitt3)



# ----------
# Squared multiple correlation
kneeAccel.pred = predict(gaitAccelRegr$yhatfd$fd, gaitt3)

kneeAccel.     = predict(kneefd.accel, gaitt3)

MS.pred = sd(t(kneeAccel.pred))^2

MS.accelfd = sd(t(kneeAccel.))^2

kneeAccel.R2 = (MS.pred / MS.accelfd)



# ----------
par(mfrow=c(1,1))
plot(gaitt3, beta.hipFine, type ='l', ylim=c(0, max(beta.hipFine)), xlab='', ylab='Hip acceleration and squared multiple correlation', lwd=2)

# ??? why errors ...
lines(gaitt3, kneeAccel.R2, lty='dashed', lwd=2)
abline(v=c(7.5, 14.7), lty='dotted')



# -->
# The solid line shows the regression function multiplying hip angle acceleration in the prediction of knee angle acceleration,
# and the dashed line indicates the corresponding squared multiple correlation funciton.

# This can be interesting because neural activation of these two musclegroups produces contraction,
# and contraction impacts acceleration directly by Newton's Second Law.
# Now it is apparent that these two angles are coupled at a nearly evenly spaced series of 7 points in the gait cycle.

