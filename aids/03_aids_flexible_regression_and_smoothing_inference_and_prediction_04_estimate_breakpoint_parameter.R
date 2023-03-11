setwd("//media//kswada//MyFiles//R//aids")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  aids
# ------------------------------------------------------------------------------
data("aids", package = "gamlss.data")


str(aids)

car::some(aids)



# ------------------------------------------------------------------------------
# Estimate the breakpoint parameter of x
# ------------------------------------------------------------------------------
# Now we use prof.term() to find a breakpoint in the relationship between the response and one of the explanatory variables.
# Stasinopoulos and Rigby have shown that the aids data provide clear evidence of a breakpoint in the AIDS cases over time.
# Here we consider model x + (x > break) * (x - break)
# This predictor model is linear in x up to x = break, then continues linearly in x but with a different slope,
# i.e. a piecewise linear model with continuity at the breakpoint.
# We are interesting in estimating the breakpoint.


aids.1 <- quote(gamlss(y ~ x + I((x > this) * (x - this)) + qrt, family = NBI, data = aids))

prof.term(aids.1, min = 16, max = 21, length = 20, criterion = "GD")



# -->
# The profile plot suggests strong support for a breakpoint since the confidence interval for the breakpoint lies within the range of x.
# The estimated breakpoint is 18.33 with a 95% (profile likelihood) confidence interval given by (17.20, 19.42)



# ----------
# Plot the aids data with the fitted breakppoint model
aids.2 <- gamlss(y ~ x + I((x > 18.33) * (x - 18.33)) + qrt, family = NBI, data = aids)
plot(aids$x, aids$y)
lines(aids$x, fitted(aids.2), col = "red")

