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
# Try increasing degrees of freedom
# ------------------------------------------------------------------------------

# pb(x, df = 4) to increase degrees of freedom
abd10_2 <- gamlss(y ~ pb(x, df = 4), sigma.fo = ~pb(x, df = 4), data = abdom, family = BCT, trace = FALSE)


summary(abd10)
summary(abd10_2)


# degrees of freedom is increased from 11.76 to 14.00



# ----------
# Recheck by multiple worm plot

coef.2 <- wp(abd10_2, xvar = abdom$x, n.inter = 9)



# ----------
# Fitted constant, linear, quadratic and cubic coefficients respectively, for each of te nine cubic polynomials fitted to the nine worm plots. 
coef.1$coef

coef.2$coef


# -->
# van Buuren and Fredriks [2001] categorize absolute values of those coefficients in excess of threshold values 0.10, 0.10, 0.05, and 0.03,
# respectively, as misfits or model violations, indicating differences between the mean, variance, skewness and kurtosis of the theoretical model residuals
# and the fitted residuals, respectively, within the particular age range.

# Still there are misfits...
# beta1 (constant):  no misfits
# beta2 (linear):  one misfit 0.166 (age group 3)
# beta3 (quadratic):  no misfit
# beta4 (cubic):  3 misfits at 3rd, 5th, and 7th ranges of age.



