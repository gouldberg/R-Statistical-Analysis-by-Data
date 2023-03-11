setwd("//media//kswada//MyFiles//R//berkeley_growth")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  berkeley growth
#   - The data from the Berkeley Growth Study (Tuddenham and Snyder, 1954).
# ------------------------------------------------------------------------------

data("growth", package = "fda")

str(growth)



# ----------
# 39 boys and 31 points
# a 31 by 39 numeric matrix giging the heights in centimeters of 39 boys at 31 ages
dim(growth$hgtm)


# 54 girls and 31 points
# a 31 by 54 numeric matrix giging the heights in centimeters of 54 girls at 31 ages
dim(growth$hgtf)



# ----------
head(growth$hgtm)


head(growth$hgtf)



# ----------
growth$age


# -->
# The ages are not equally spaced;
# There are four measurements while the child is one year old, annual measurements from two to eight years,
# followed by heights measured biannually



# ------------------------------------------------------------------------------
# Pairs of Derivatives: Phase-Plane Plots
#   - The second derivative or acceleration curves are plotted against the first derivative or velocity curves
# ------------------------------------------------------------------------------

# velocity and acceleration

hgtf.vel = predict(hgtfmonfd$yhatfd, agefine, 1)

hgtf.acc = predict(hgtfmonfd$yhatfd, agefine, 2)



# ----------
(i11.7 = which(abs(agefine - 11.7) == min(abs(agefine - 11.7)))[1])


par(mfrow=c(1,1))
plot(hgtf.vel, hgtf.acc, type='n', xlim=c(0, 12), ylim=c(-5, 2),
     xlab='Velocity (cm/yr)', ylab=expression(Acceleration (cm/yr^2)),
     las=1)

for(i in 1:10){
  lines(hgtf.vel[, i], hgtf.acc[, i])
  points(hgtf.vel[i11.7, i], hgtf.acc[i11.7, i])
}

abline(h=0, lty='dotted')



# -->
# Each curve begins in time off the lower right with the strong velocity and deceleration of infant growth.
# The velocities are accelerations at age 11.7 years, the average age of the middle of the growth spurt, are marked on each curve by circles.

# The acceleration is positive for a while as the velocity increases until the acceleration drops again to zero
# on the right at the middle of the spurt.
# The large negative swing terminates near the origin where both velocity and acceleration vanish at the beginning of adulthood.

# Variability is greatest in the lower right in early childhood, but it is curious that two of the 10 girls have quite distinctive curves in that region.
#  - Why does the pubertal growth spurt show up as a loop ?
#  - What information does the size of the loop convey ?
#  - Why are the larger loops tending to be on the right and the smaller on the left ?
#  - We see from the shapes of the loop and from the position of the 11.7 year marker that girls with early pubertal spurts
#    (marker point well to the left) tend to have very large loops, and late-spurt girls have small ones.
#  - Does interchild variability correspond to smothing like growth energy ?



# ------------------------------------------------------------------------------
# For check: smoothed and interpolated growth curve and phase-plane plot
# ------------------------------------------------------------------------------

# specify the girl
i <- 10

par(mfrow=c(1,2))

with(growth, matplot(age, hgtf[, i], pch='o', col=1, xlab='Age (years)', ylab='Height (cm)', ylim=c(60, 183)) )
matlines(agefine, hgtf.vec[,i], lty=1, col=1)
abline(v=11.7, lty = 'dotted')

plot(hgtf.vel, hgtf.acc, type='n', xlim=c(0, 12), ylim=c(-5, 2), xlab='Velocity (cm/yr)', ylab=expression(Acceleration (cm/yr^2)), las=1)
lines(hgtf.vel[, i], hgtf.acc[, i])
points(hgtf.vel[i11.7, i], hgtf.acc[i11.7, i])
abline(h=0, lty='dotted')
