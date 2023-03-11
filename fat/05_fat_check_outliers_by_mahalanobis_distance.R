setwd("//media//kswada//MyFiles//R//fat")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fat
# ------------------------------------------------------------------------------

data(fat, package="faraway")

str(fat)



# take only body circumference
names(fat)

cfat <- fat[,9:18]



# ------------------------------------------------------------------------------
# Check the outliers by mahalanobis distance
#  - Like variances, principal components analysis can be very sensitive to outliers so it is essential to check for these.
#    Mahalanobis distance is a measure of the distance of a point from the mean that adjusts for the correlation in the data:
#    di = sqrt( (x - mu)T * measure of covariance * (x - mu) )
#  - Since we are concerned about outliers, it is sensible to use robust measures of center and covariance
# ------------------------------------------------------------------------------

library(MASS)



# ----------
# robust measures of center and covariance
robfat <- cov.rob(cfat)

robfat$center

robfat$cov



# ----------
# mahalanobis function returns di^2.
# If the data are multivariate normal with dimension m, then we expect d^2 to follow a chi^2 distribution
# we can check this with a Q-Q plot
md <- mahalanobis(cfat, center = robfat$center, cov = robfat$cov)

n <- nrow(cfat)
p <- ncol(cfat)

par(mfrow=c(1,1))
plot(qchisq(1:n/(n+1), p), sort(md), xlab = expression(paste(chi^2, "quantiles")), ylab = "Sorted Mahalanobis distances")



# -->
# We see that there are some outliers and that we can investigate the sensitivity of the PCA
# to these values by re-analysing the data after removing these points.

