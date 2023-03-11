setwd("//media//kswada//MyFiles//R//abdom")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  abdom
# ------------------------------------------------------------------------------

data("abdom", package = "gamlss.data")


str(abdom)

car::some(abdom)



# ------------------------------------------------------------------------------
# Flexible regression and smoothing
# ------------------------------------------------------------------------------

abd10 <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = abdom, family = BCT, trace = FALSE)


summary(abd10)



# ------------------------------------------------------------------------------
# Diagnostics: Multiple worm plot
#   - This plot highlight failures of the model within different ranges of the explanatory variable.
#     This should be applied to each explanatory variable in turn and is especially important when one of the explanatory variables is dominant in the analysis.
#
#   - A quantitative explanatory variable is cut into n.inter non-overlapping intervals with equal numbers of observations in each,
#     and the worm plots of the residuals for each interval are plotted.
#   - A categorical explanatory variable (factor) splits the data into its different levels and plots on worm plot for each level.
# ------------------------------------------------------------------------------

# We are interested in whether the model fits well at the different regions of age.
# n.inter to specify nine intervals of age with equal numbers of observations, for the worm plot

coef.1 <- wp(abd10, xvar = abdom$x, n.inter = 9)



# ----------
# The table of intervals gives the nine non-overlapping x (i.e. age) ranges in weeks, 
# which are also plotted in steps above the worm plot.
# Individual worm plots are read along rows from bottom left to top right, corresponding to the nine age intervals

coef.1$classes



# ----------
# Fitted constant, linear, quadratic and cubic coefficients respectively, for each of te nine cubic polynomials fitted to the nine worm plots. 
coef.1$coef



# -->
# van Buuren and Fredriks [2001] categorize absolute values of those coefficients in excess of threshold values 0.10, 0.10, 0.05, and 0.03,
# respectively, as misfits or model violations, indicating differences between the mean, variance, skewness and kurtosis of the theoretical model residuals
# and the fitted residuals, respectively, within the particular age range.

# beta1 (constant):  no misfits
# beta2 (linear):  one misfit 0.156 (age group 3)
# beta3 (quadratic):  no misfit
# beta4 (cubic):  3 misfits at 3rd, 5th, and 7th ranges of age.

# With respect to the misfits at the 3rd and 5th range of age, the model seems to suggest that the tails of the BCT are too heavy
# with respect to the data in these age ranges.
# However, the 7th range of the data suggests otherwise.


# -->
# Plausible solutions are to provide a model for the tau parameter, or increase the degrees of freedom in the model for tau, e.g. pb(x, df = 4),
# or assume a different distributions.
# However, occasional misfits may occur even when the model is correct.




