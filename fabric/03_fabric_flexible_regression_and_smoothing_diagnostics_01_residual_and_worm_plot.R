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
# Diagnostics: Residual plots
# ------------------------------------------------------------------------------
# plot.gamlss() produces 4 plots for checking the normalized (randomized) quantile residuals
#  1. residuals against the fitted values of the parameter
#  2. residuals against an index or a specified covariate
#  3. a kernel density estimate of the residuals
#  4. a QQ-normal plot of the residuals

# also print out the summary
plot(mPO1)
plot(mPO2)



# ----------
# We use the option xvar to change the top right-hand plot so the plot shows the residuals against x instead of the index.

newpar <- par(mfrow=c(2,2), mar = par("mar") + c(0,1,0,0), col.axis = "blue4", col = "blue4", col.main = "blue4", col.lab = "blue4", pch = "+",
              cex = .45, cex.lab = 1.2, cex.axis = 1, cex.main = 1.2)

plot(mPO2, xvar = fabric$x, par = newpar)



# ------------------------------------------------------------------------------
# Diagnostics: single worm plot
#   - The resulting plot is equivalent to the normal Q-Q plot detrended by subtracting the line (intercept 0 and slope 1)
#     This allows the worm plot to focus on deviations from a standard normal distribution in the distribution of the residuals.
#   - The approxiamate point-wise 95% confidence intervals given by the two elliptic curves in the figure.
#     If the model is correct we would expect approximately 95% of the points to lie between the two elliptic curves and 5% outside.
#   - The fitted curve is a cubic fit to the worm plot points.
# ------------------------------------------------------------------------------

wp(mPO1)

wp(mPO2)



# ----------
qqnorm(resid(mPO2))

qqline(resid(mPO2))




