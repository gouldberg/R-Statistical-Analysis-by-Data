setwd("//media//kswada//MyFiles//R//ethanol")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ethanol
# ------------------------------------------------------------------------------

data(ethanol, package="lattice")

str(ethanol)



# ------------------------------------------------------------------------------
# Additive model by mgcv package:  Fit a GAM with a bivariate s() on the predictors
#   - mgcv package is part of the recommended suite that comes with the default installation of R and is based on methods described in Wood (2000).
#   - mgcv package has an automatic choice in the amount of smoothing as well as wider functionality.
#     but splines are only choice of smoother, but the appropriate amount of smoothing is internally chose by default.
#   - mgcv package employs a penalized smoothing spline approach and likelihood approach.
#     GCV is used to select lambda (controlling the amount of smoothing for each variable)
# ------------------------------------------------------------------------------

library(mgcv)

amint <- mgcv::gam(NOx ~ s(C, E), data = ethanol)

summary(amint)


# -->
# Intercept is significant


# ----------
par(mfrow=c(1,1))
plot(amint, select = 1)

vis.gam(amint, theta = -45, color = "gray")



# ----------
# check the residuals
plot(predict(amint), residuals(amint), xlab = "Predicted", ylab = "Residuals")

qqnorm(residuals(amint), main = "")
qqline(residuals(amint))


# -->
# This shows the constant variance, and but some problem with normality at the tail.



# ------------------------------------------------------------------------------
# Additive model by mgcv package:  Refit the GAM but with an appropriate kind of smoothing.
# ------------------------------------------------------------------------------





# ------------------------------------------------------------------------------
# Additive model by mgcv package:  Fit a GAM with univariate smooths on the predictors
# ------------------------------------------------------------------------------

# this produces error...
# ammgcv <- mgcv::gam(NOx ~ s(C, k=4) + s(E, k=4), data = ethanol)


# add k to s()
ammgcv <- mgcv::gam(NOx ~ s(C, k=4) + s(E, k=4), data = ethanol)

summary(ammgcv)


par(mfrow=c(1,2))
plot(ammgcv)

