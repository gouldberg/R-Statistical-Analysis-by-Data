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
# Diagnostics: Residual plots
# ------------------------------------------------------------------------------
# plot.gamlss() produces 4 plots for checking the normalized (randomized) quantile residuals
#  1. residuals against the fitted values of the parameter
#  2. residuals against an index or a specified covariate
#  3. a kernel density estimate of the residuals
#  4. a QQ-normal plot of the residuals

# also print out the summary
plot(abd10)



# ----------
# We use the option xvar to change the top right-hand plot so the plot shows the residuals against age instead of the index.

newpar <- par(mfrow=c(2,2), mar = par("mar") + c(0,1,0,0), col.axis = "blue4", col = "blue4", col.main = "blue4", col.lab = "blue4", pch = "+",
              cex = .45, cex.lab = 1.2, cex.axis = 1, cex.main = 1.2)

plot(abd10, xvar = abdom$x, par = newpar)



# ------------------------------------------------------------------------------
# Diagnostics: single worm plot
#   - The resulting plot is equivalent to the normal Q-Q plot detrended by subtracting the line (intercept 0 and slope 1)
#     This allows the worm plot to focus on deviations from a standard normal distribution in the distribution of the residuals.
#   - The approxiamate point-wise 95% confidence intervals given by the two elliptic curves in the figure.
#     If the model is correct we would expect approximately 95% of the points to lie between the two elliptic curves and 5% outside.
#   - The fitted curve is a cubic fit to the worm plot points.
# ------------------------------------------------------------------------------

wp(abd10)


# -->
# Since all the observations fall in the "acceptance" region inside the two elliptic curves and no specific shape is detected in the points,
# the model appears to fit well overall.


# ----------
qqnorm(resid(abd10))
qqline(resid(abd10))




