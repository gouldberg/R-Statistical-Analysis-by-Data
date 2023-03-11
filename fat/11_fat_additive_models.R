setwd("//media//kswada//MyFiles//R//fat")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fat
# ------------------------------------------------------------------------------

data(fat, package="faraway")

str(fat)



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
amgam <- gam(siri ~ lo(age) + lo(weight) + lo(height) + lo(adipos) + lo(neck) + lo(chest) + lo(abdom) + lo(hip) + lo(thigh) + lo(knee) +
               lo(ankle) + lo(biceps) + lo(forearm) + lo(wrist), data = fat, trace=TRUE)

summary(amgam)



# ----------
# R2 = 1 - Residual Deviance / Null Deviance
1 - amgam$deviance / amgam$null.deviance



# ----------
# For anova for parametric effects, lo(ibt) is not significant.
# gam pakcage uses a score test for the predictors, however the p-values are only approximate at best and should be viewed
# with some skepticism.
# It is generally better to fit the model without the predictor of interest and then construct the F-test
amgamr <- gam(siri ~ lo(age) + lo(neck) + lo(abdom) + lo(forearm) + lo(wrist), data = fat, trace = TRUE)

summary(amgamr)

anova(amgamr, amgam, test = "F")

1 - amgamr$deviance / amgamr$null.deviance



# ----------
# we examine the fit
par(mfrow=c(1,3))
plot(amgamr, residuals = TRUE, se = TRUE, pch = ".")



# ------------------------------------------------------------------------------
# Additive model by mgcv package
#   - mgcv package is part of the recommended suite that comes with the default installation of R and is based on methods described in Wood (2000).
#   - mgcv package has an automatic choice in the amount of smoothing as well as wider functionality.
#     but splines are only choice of smoother, but the appropriate amount of smoothing is internally chose by default.
#   - mgcv package employs a penalized smoothing spline approach and likelihood approach.
#     GCV is used to select lambda (controlling the amount of smoothing for each variable)
# ------------------------------------------------------------------------------

library(mgcv)

ammgcv <- mgcv::gam(siri ~ s(age) + s(weight) + s(height) + s(adipos) + s(neck) + s(chest) + s(abdom) + s(hip) + s(thigh) + s(knee) +
               s(ankle) + s(biceps) + s(forearm) + s(wrist), data = fat, trace=TRUE)

summary(ammgcv)

ammgcvr <- mgcv::gam(siri ~ s(age) + s(abdom) + s(hip) + s(biceps) + s(wrist), data = fat, trace=TRUE)

summary(ammgcvr)



# ----------
# it is over 80% !
1 - ammgcvr$deviance / ammgcvr$null.deviance



# ----------
par(mfrow=c(2,3))
plot(ammgcvr)


# ----------
anova(ammgcv, ammgcvr, test = "F")


# -->
# The p-value is only approximate, but it is not significant change ??



# ----------
# we can do bivariate transformations with mgcv.

amint <- mgcv::gam(siri ~ s(age, abdom) + s(hip) + s(biceps) + s(wrist), data = fat, trace=TRUE)

summary(amint)

anova(ammgcvr, amint, test = "F")



# ----------
par(mfrow=c(2,2))
plot(amint)


vis.gam(amint, theta = -45, color = "gray")


# -->
# Given that the countours appear almost parallel and the perspective view looks like 
# it could be constructed with peice of paper rippled in one direction,
# we conclude that there is no significant interaction.
# One interesting side effect is that hip is now significant.



# ----------
par(mfrow=c(1,2))
plot(predict(ammgcvr), residuals(ammgcvr), xlab = "Predicted", ylab = "Residuals")
qqnorm(residuals(ammgcvr), main = "")


# -->
# This shows the constant variance, and no problem with normality










