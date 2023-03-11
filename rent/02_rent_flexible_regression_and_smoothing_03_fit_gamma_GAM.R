setwd("//media//kswada//MyFiles//R//rent")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rent
# ------------------------------------------------------------------------------
data("rent", package = "gamlss.data")


str(rent)

car::some(rent)



# ------------------------------------------------------------------------------
# Fit gamma GAM
# ------------------------------------------------------------------------------

# pb(): penalized B-splines.
# Uses the B-spline basis of a piecewise polynomial of degree d with equally spaced knots over the x range.
# The default intervals = 20
# The default degree of the piecewise polynomial = 3

# We apply pb() to continuous variable Fl and A
r3 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc, family = GA, data = rent)


AIC(r2, r3)



# -->
# According to the AIC, the GAM model with smoothers is better than the simple GLM with linear terms for Fl and A



# ----------
summary(r3)


# -->
# When smoothers are fitted all standard error shown should be treated with caution.

# The first is that the resulting coefficients of each smoother and its standard error refer only to the linear part of the smoother
# and not to the smother's contribution as a whole, which is decomposed into a linear plus a nonlinear smoothing part.

# The second issue has to do with the standard errors of the linear part of the model, i.e. of the terms H and loc.
# Those standard errors are estimated assuming that the smoother terms are fixed at their fitted values and
# therefore do not take into the account the uncertainty introduced by estimating the smoothing terms.



# ------------------------------------------------------------------------------
# Test the contribution of the smoother as a whole (including the linear term) by drop1()
# ------------------------------------------------------------------------------

# drop1 check for the approximate significance of the contribution of the smoothers (including the linear terms)
drop1(r3)



# -->
# All terms contribute significantly to modelling the predictor log(mu)
# Note that the degrees of freedom reduction (1.8445) resulting from dropping H from the model r3 is different from 1 because dropping H result
# in a slight changes in the automatically chosen degrees of freedom used in the smoothing terms pb(Fl) and pb(A)



# ------------------------------------------------------------------------------
# Plot of the fitted terms
# ------------------------------------------------------------------------------

term.plot(r3, pages = 1, ask = FALSE)



# -->
# The plot show that the predictor log(mu) for the mean rent rises almost linearly with floor space Fl,
# but nonlinearly with age A, remaining stable if the flat was built before the 1960s and rising after that.

# The contribution of the two factors H and loc are what we would expect, i.e. lower rent if the flat does not have central heating (H=1)
# and increasing rent as the locaiton of the flat changes from below average to average and then to above average (loc = 1, 2 and 3, respectively).

# The shaded areas are the pointwise 95% confidence bands for the smoothing curves and factor levels.



# ------------------------------------------------------------------------------
# Check the adequacy of the fitted GAM model by worm plot
# ------------------------------------------------------------------------------

# worm plot is a de-trended QQ plot of the residuals
wp(r3, ylim.all = .6)



# -->
# The worm here is below the lower curve on the left of the figure.

# To improve the residuals of the GAM, we shall next model the parameter sigma of the gamma distribution as a function of the explanatory variables.



