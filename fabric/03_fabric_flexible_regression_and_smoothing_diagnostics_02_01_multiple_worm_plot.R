setwd("//media//kswada//MyFiles//R//fabric")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fabric
# ------------------------------------------------------------------------------
data("fabric", package = "gamlss.data")


str(fabric)

car::some(fabric)



# ------------------------------------------------------------------------------
# Diagnostics: Multiple worm plot
#   - This plot highlight failures of the model within different ranges of the explanatory variable.
#     This should be applied to each explanatory variable in turn and is especially important when one of the explanatory variables is dominant in the analysis.
#
#   - A quantitative explanatory variable is cut into n.inter non-overlapping intervals with equal numbers of observations in each,
#     and the worm plots of the residuals for each interval are plotted.
#   - A categorical explanatory variable (factor) splits the data into its different levels and plots on worm plot for each level.
# ------------------------------------------------------------------------------

# We are interested in whether the model fits well at the different regions of x.
# n.inter to specify nine intervals of age with equal numbers of observations, for the worm plot

coef.1 <- wp(mPO2, xvar = fabric$x, n.inter = 4)



# ----------
# The table of intervals gives the nine non-overlapping x (i.e. x) ranges in weeks, 
# which are also plotted in steps above the worm plot.
# Individual worm plots are read along rows from bottom left to top right

coef.1$classes



# ----------
# Fitted constant, linear, quadratic and cubic coefficients respectively, for each of te nine cubic polynomials fitted to worm plots. 
coef.1$coef



# -->
# van Buuren and Fredriks [2001] categorize absolute values of those coefficients in excess of threshold values 0.10, 0.10, 0.05, and 0.03,
# respectively, as misfits or model violations, indicating differences between the mean, variance, skewness and kurtosis of the theoretical model residuals
# and the fitted residuals, respectively, within the particular x range.



