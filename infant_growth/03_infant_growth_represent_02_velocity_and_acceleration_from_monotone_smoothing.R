setwd("//media//kswada//MyFiles//R//infant_growth")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  infant growth
# ------------------------------------------------------------------------------
data("infantGrowth", package = "fda")


dim(infantGrowth)


head(infantGrowth)



# ------------------------------------------------------------------------------
# Velocity and Acceleration from Monotone Smoothing
# ------------------------------------------------------------------------------

dayfine  = seq(1, n, len=151)


# smoothed value from monotone smoothing
tibhat   = beta[1] + beta[2] * eval.monfd(dayfine, Wfd)


# velocity
Dtibhat  =        beta[2] * eval.monfd(dayfine, Wfd, 1)


# acceleration
D2tibhat =        beta[2] * eval.monfd(dayfine, Wfd, 2)



# ------------------------------------------------------------------------------
# plot Velocity and Acceleration from Monotone Smoothing
# ------------------------------------------------------------------------------

op = par(mfrow=c(3,1), mar=c(5,5,3,2), lwd=2)

plot(day, tib, type = "p", cex=1.2, las=1,
     xlab="Day", ylab='', main="Tibia Length (mm)")
lines(dayfine, tibhat, lwd=2)

plot(dayfine, Dtibhat, type = "l", cex=1.2, las=1,
     xlab="Day", ylab='', main="Tibia Velocity (mm/day)")

plot(dayfine, D2tibhat, type = "l", cex=1.2, las=1,
     xlab="Day", ylab='', main="Tibia Acceleration (mm/day/day)")
lines(c(1,n),c(0,0),lty=2)

par(op)

