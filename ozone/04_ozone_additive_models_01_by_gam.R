setwd("//media//kswada//MyFiles//R//ozone")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ozone
# ------------------------------------------------------------------------------

data(ozone, package="faraway")

str(ozone)




# ------------------------------------------------------------------------------
# Additive model by gam package
#   - gam package originates from the work of Hastie and Tibshirani (1900)
#     The gam package allows more choice in the smoothers used
#     while the mgcv package has an automatic choice in the amount of smoothing as well as wider functionality
#   - gam packages use backfitting algorithm for fitting.
#     The algorithm is iterated until convergence. Hastie and Tibshirani show that convergence is assured under some rather loose conditions.
# ------------------------------------------------------------------------------

library(gam)


# We fit an additive model using a Gaussian response as the default
# lo:  loess smoother
# s:  smoothing splines

amgam <- gam(O3 ~ lo(temp) + lo(ibh) + lo(ibt), data = ozone, trace=TRUE)

summary(amgam)



# ----------
# R2 = 1 - Residual Deviance / Null Deviance
# R2 is larger than simple regression model
1 - amgam$deviance / amgam$null.deviance




# ----------
# For anova for parametric effects, lo(ibt) is not significant.
# gam pakcage uses a score test for the predictors, however the p-values are only approximate at best and should be viewed
# with some skepticism.
# It is generally better to fit the model without the predictor of interest and then construct the F-test
amgamr <- gam(O3 ~ lo(temp) + lo(ibh), data = ozone)

summary(amgamr)

anova(amgamr, amgam, test = "F")


# -->
# Again the p-value is an approximation, but we can see there is some evidence that ibt is not significant.



# ----------
# we examine the fit
par(mfrow=c(1,3))
plot(amgam, residuals = TRUE, se = TRUE, pch = ".")



# -->
# Transformations on the predictors chosen by the gam fit on the ozone data.
# Partial residuals and approximate 95% pointwise confidence bands are shown.

# For ibt, a constant function would fit between the confidence bands.
# This reinforces the conclusion that this predictor is NOT SIGNIFICANT.

# For temperature, we can see a change in the slope arond 60%, while for ibh, there is a clear maximum.
# The partial residuals allow us to check for outliers or influential points.
# We see no such problems here.
