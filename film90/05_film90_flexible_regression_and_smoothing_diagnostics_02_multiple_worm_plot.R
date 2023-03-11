setwd("//media//kswada//MyFiles//R//film90")

packages <- c("dplyr", "gamlss", "gamlss.add")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  film90
# ------------------------------------------------------------------------------
data("film90", package = "gamlss.data")


str(film90)

car::some(film90)



# ------------------------------------------------------------------------------
# Diagnostics: Multiple worm plot
#   - This plot highlight failures of the model within different ranges of the explanatory variable.
#     This should be applied to each explanatory variable in turn and is especially important when one of the explanatory variables is dominant in the analysis.
#
#   - A quantitative explanatory variable is cut into n.inter non-overlapping intervals with equal numbers of observations in each,
#     and the worm plots of the residuals for each interval are plotted.
#   - A categorical explanatory variable (factor) splits the data into its different levels and plots on worm plot for each level.
# ------------------------------------------------------------------------------

# We are interested in whether the model fits well at the different regions of lboopen.
# n.inter to specify 7 intervals of lboopen with equal numbers of observations, for the worm plot

# coef.1 <- wp(m3, xvar = film90$lboopen, n.inter = 7)
coef.1 <- wp(m6, xvar = film90$lboopen, n.inter = 7)



# ----------
# The table of intervals gives the 7 non-overlapping x (i.e. lboopen) ranges, 
# which are also plotted in steps above the worm plot.
# Individual worm plots are read along rows from bottom left to top right, corresponding to 7 lboopen intervals

coef.1$classes



# ----------
# Fitted constant, linear, quadratic and cubic coefficients respectively, for each of 7 cubic polynomials fitted to the 7 worm plots. 
coef.1$coef



# -->
# van Buuren and Fredriks [2001] categorize absolute values of those coefficients in excess of threshold values 0.10, 0.10, 0.05, and 0.03,
# respectively, as misfits or model violations, indicating differences between the mean, variance, skewness and kurtosis of the theoretical model residuals
# and the fitted residuals, respectively, within the particular age range.










