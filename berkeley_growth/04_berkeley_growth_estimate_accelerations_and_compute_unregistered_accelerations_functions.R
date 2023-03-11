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
# Estimate accelerations of heights for 10 girls, measured in centimeters per year per year
# ------------------------------------------------------------------------------

accfvec = predict(hgtfmonfd$yhatfd, agefine, Lfdobj=2)


matplot(agefine, accfvec, type='l', lty=1, ylim=c(-4, 2),
        xlab='Age (years)', ylab=expression(Acceleration (cm/yr^2)),
        xlim=c(1, 18), las=1)


abline(h=0, lty='dotted')


# The heavy dashed line is the cross-sectional mean and is a rather poor summary of the curves
lines(agefine, rowMeans(accfvec), lty='dashed', lwd=2)



# -->
# The pubertal growth spurt shows up as a pulse of strong positive acceleration followed by sharp negative deceleration.
# But most recoreds also show a bump at around six years that is termed the midspurt.
# Some of the variation from curve to curve can be explained at the level of certain derivatives.

# The rapid growth during puberty is visible in all curves, but both the timing and the intensity of pubertal growth differ from girl to girl.
# It can be important to separate variation in timing of significant growth events, such as pubertal growth spurt,
# from variation in the intensity of growth.  --> curve registration



# ------------------------------------------------------------------------------
# Compute unregistered accelerations functions and ther mean function
# ------------------------------------------------------------------------------

accelfdUN     = deriv.fd(hgtfmonfd$yhatfd, 2)

accelmeanfdUN = mean(accelfdUN)



#  plot unregistered curves
plot(accelfdUN, xlim=c(1,18), ylim=c(-4,3), lty=1, lwd=2,
     cex=2, xlab="Age", ylab="Acceleration (cm/yr/yr)")


