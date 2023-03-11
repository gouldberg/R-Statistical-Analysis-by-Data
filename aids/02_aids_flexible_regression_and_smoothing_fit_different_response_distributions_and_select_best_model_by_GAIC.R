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
# Flexible regression and smoothing by different distributions by update()
# ------------------------------------------------------------------------------

# We start by using the Poisson family to model the number of reported cases.
# Explanatory variables are time (a continuous variable), which we smooth with a P-spline smoother, and qrt,
# a factor representing a quarterly seasonal effect.

# Fit the Poisson distribution model
h.po <- gamlss(y ~ pb(x) + qrt, family = PO, data = aids)


term.plot(h.po)

wp(h.po)



# ----------
# update with a negative binomial 
h.nb <- update(h.po, family = NBI)


term.plot(h.nb)

wp(h.nb)



# ----------
# update the smoothing usinc cs()
h.nb1 <- update(h.nb, ~ cs(x, 8) + qrt)


term.plot(h.nb1)

wp(h.nb1)



# ----------
# remove qrt
h.nb2 <- update(h.nb, ~ .-qrt)

wp(h.nb2)



# ----------
# put back qrt, take log of y and fit a normal distribution
h.nb3 <- update(h.nb1, log(.) ~ . + qrt, family = NO)

wp(h.nb3)



# ------------------------------------------------------------------------------
# Compare models by GAIC
# ------------------------------------------------------------------------------

GAIC(h.po, h.nb, h.nb1, h.nb2, h.nb3)





