setwd("//media//kswada//MyFiles//R//brain")

packages <- c("dplyr", "lattice", "gamair", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  brain
# ------------------------------------------------------------------------------
data(brain, package = "gamair")

str(brain)



# 2 voxels appear problematic, these voxels have medFPQ values recorded as 3 * 10^-6 and 4 * 10^-7, while the remaining 1565 voxels have values in the range 0.003 to 20.
# Residual plots from all attempts to model the data set including these two voxels consistently show them as grotesque outliers.

# exclude 2 outliers.
brain <- brain[brain$medFPQ > 5e-3,]



# ------------------------------------------------------------------------------
# Attempt to use a Gaussian model without transformation
# ------------------------------------------------------------------------------

m0 <- mgcv::gam(medFPQ ~ s(Y, X, k = 100), data = brain)


summary(m0)



# ----------
# gam.check is a routine that produces some basic residual plots, and a little further information about the success or otherwise of the fitting process.
# k': the maximum possible effective degrees of freedom for the smooth, to compare with the actual EDF
# k-index: the ratio of the model estimated scale parameter to an estimate based on differencing neighbouring residuals
# the p-value: for the residual randomization test
graphics.off()
par(mfrow = c(2,2))
gam.check(m0)


# -->
# Here the results are problematic
# There are clear problems with the constant variance assumption, the variance is increasing with the mean.
# The lower left histogram of residuals confirms the pattern evident in the QQ-plot:  there are too many residuals in the tails and centre of the distribution
# relative to its shoulders



# ------------------------------------------------------------------------------
# If we assume that var(yi) is proportional to mui^beta, where mui = E(yi) and beta is some parameter
# ------------------------------------------------------------------------------
e <- residuals(m0)

fv <- fitted(m0)

summary(lm(log(e^2) ~ log(fv)))



# -->
# the estimated beta is almost 2, that is, it appears that the variance of the data increase with the square of the mean.
# This in turn suggests using the gamma distribution, which has this mean-variance relationship.


# Alternatively the apparent mean-variance relationship suggests using a log transform of medFPQ to stabilize the variance,
# but in practice a less extreme 4th root transform produces better residual plots.




