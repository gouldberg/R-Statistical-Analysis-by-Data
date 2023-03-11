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
# Plot
#   - intercept + mean knee angle
#   - hip coef + squared multiple correlation
# ------------------------------------------------------------------------------

ylim1 = range(kneeIntercept, kneeMean)

ylim2 = c(0, max(hipCoef, Rsqr))


graphics.off()
par(mfrow=c(2,1))


# Plot intercept & mean knee angle
plot(gaitfine, kneeIntercept, ylim=ylim1, lwd=2,
     main="Intercept and Mean Knee Angle", type='l',
     xlab='', ylab='')

lines(gaitfine, kneeMean, lty='dashed')
abline(h=0, v=c(7.5, 14.7), lty='dotted')


# Plot Hip coefficient & squared multiple correlation
plot(gaitfine, hipCoef, lwd=2, xlab='', ylab='', ylim=ylim2, type='l',
     main='Hip Coefficient and Squared Multiple Correlation')

lines(gaitfine, Rsqr, lty='dashed')
abline(v=c(7.5, 14.7), lty='dotted')



# -->
# The top panel shows as a solid line the intercept term in the prediction of knee angle from hip angle;
# the dashed line indicates the mean knee angle assuming no hip angle effect.

# The bottome panel shows as a solid line the functional regression coefficient multiplying hip angle in the functional concurrent linear model,
# and the dashed line shows the squared multiple correlation coefficient function associated with this model.

# Vertical dashed lines indicated boundaries between the three phases of the gait cycle.

# The R^2 function tracks pretty closely the variation in the hip regression coefficient.

