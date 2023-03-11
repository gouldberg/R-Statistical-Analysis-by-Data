setwd("//media//kswada//MyFiles//R//ozone")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ozone
# ------------------------------------------------------------------------------

data(ozone, package="faraway")

str(ozone)




# ------------------------------------------------------------------------------
# Additive model by mgcv package
#   - mgcv package is part of the recommended suite that comes with the default installation of R and is based on methods described in Wood (2000).
#   - mgcv package has an automatic choice in the amount of smoothing as well as wider functionality.
#     but splines are only choice of smoother (??), but the appropriate amount of smoothing is internally chose by default.
#   - mgcv package employs a penalized smoothing spline approach and likelihood approach.
#     GCV is used to select lambda (controlling the amount of smoothing for each variable)
# ------------------------------------------------------------------------------

library(mgcv)


# s:  spline smoother
ammgcv <- mgcv::gam(O3 ~ s(temp) + s(ibh) + s(ibt), data = ozone)


summary(ammgcv)



# -->
# We can compute the equivalent degrees of freedom by analogy to linear models.
# For linear smoothers, the relationship between the observed and fitted values may be written as y = Py.
# The trace of P then estimates the effective number of parameters.
# The column marked Ref.df is a modified computation of the degrees of freedom.


plot(ammgcv, residuals = TRUE, select = 1)
plot(ammgcv, residuals = TRUE, select = 2)
plot(ammgcv, residuals = TRUE, select = 3)


# -->
# Note how the some scale has been deliberately used on all three plots. This allows us to easily compare the relative contribution of each variable.
# Note that the label on the vertical axis give the degrees of freedom for the smooth.
# The chosen transformations are again similar. ibt does not appear to be significant.
# In the case of ibt, we see that a horizontal line at zero would fit between the bands.
# GCV = 19.4, using effectively 19.4 parameters including the intercept.




# ----------
# dropping ibt
am1 <- mgcv::gam(O3 ~ s(temp) + s(ibh), data = ozone)

am2 <- mgcv::gam(O3 ~ temp + s(ibh), data = ozone)

anova(am2, am1, test = "F")



# -->
# The p-value is only approximate, but it certainly seems there really is a change in the trend for temperature.



# ------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------

# we can predict new values with standard error
predict(ammgcv, data.frame(temp = 60, ibh = 2000, ibt = 100), se = T)



# but if we try to make predictions for predictor values outside the original range of the data
# we will need to linearly extrapolate the spline fits. This is dangerous for all the usual reasons
predict(ammgcv, data.frame(temp = 120, ibh = 2000, ibt = 100), se = T)

# we see that the standard error is much larger although this likely does not fully reflect the uncertainty.



# ----------
par(mfrow=c(1,2))
plot(predict(ammgcv), residuals(ammgcv), xlab = "Predicted", ylab = "Residuals")
abline(h = 0)
qqnorm(residuals(ammgcv), main = "")
qqline(residuals(ammgcv), main = "")


# -->
# This shows the nonconstant variance, suggesting that a Poisson response might be suitable.
# There are also somewhat long tails for the residuals.
