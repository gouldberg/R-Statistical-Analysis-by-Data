setwd("//media//kswada//MyFiles//R//airquality")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  airquality
# ------------------------------------------------------------------------------
data("airquality")


str(airquality)

car::some(airquality)




# ------------------------------------------------------------------------------
# Fit gamma GAM
# ------------------------------------------------------------------------------

# gamlss() function does not work with NA's, so before fitting the model the cases with missing values have to be removed.
da <- na.omit(airquality)



# pb():  current version of P-splines which uses SVD in the fitting
r3 <- gamlss(Ozone ~ pb(Temp) + pb(Wind) + pb(Solar.R), data = da, family = GA)



# ----------
AIC(r2, r3)



# -->
# According to the AIC, the GAM model with smoothers is better than the simple GLM



# ----------
summary(r3)



# -->
# When smoothers are fitted all standard error shown should be treated with caution.

# The first is that the resulting coefficients of each smoother and its standard error refer only to the linear part of the smoother
# and not to the smother's contribution as a whole, which is decomposed into a linear plus a nonlinear smoothing part.

# The second issue has to do with the standard errors of the linear part of the model, 
# i.e. of the terms Temp, Wind and Solar.R.
# Those standard errors are estimated assuming that the smoother terms are fixed at their fitted values and
# therefore do not take into the account the uncertainty introduced by estimating the smoothing terms.




# ------------------------------------------------------------------------------
# plot the fitted terms for mu for gamma GAM
# ------------------------------------------------------------------------------

par(mfrow=c(2,2))

term.plot(r3, pages = 1, what = "mu", ask = FALSE)



# -->
# Note that we get different relationship among variable and response
# The plot show that the predictor log(mu) for the mean Ozone
# The shaded areas are the pointwise 95% confidence bands for the smoothing curves.



# ------------------------------------------------------------------------------
# Test the contribution of the smoother as a whole (including the linear term) by drop1()
# ------------------------------------------------------------------------------

# drop1 check for the approximate significance of the contribution of the smoothers (including the linear terms)
drop1(r3)



# -->
# All terms contribute significantly to modelling the predictor log(mu)



# ------------------------------------------------------------------------------
# Check the adequacy of the fitted GAM model by worm plot
# ------------------------------------------------------------------------------

# worm plot is a de-trended QQ plot of the residuals
wp(r2, ylim.all = 2)

wp(r3, ylim.all = 2)



# -->
# The worm here is below the lower curve on the left of the figure.

# To improve the residuals of the GAM, we shall next model the parameter sigma of the gamma distribution as a function of the explanatory variables.



