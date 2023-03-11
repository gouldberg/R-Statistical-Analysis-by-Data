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
# Generalized Likelihood Ratio Test (GLRT)
#   - For testing the significance of individual terms, given all the rest of the terms in the model,
#     it is generally better to use drop1() rather than relying on p-values from summary()
#   - The drop1() function provides the generalized likelihood ratio test (GLRT) for dropping each term, which is generally much more reliable
#     than the Wald test based on the standard error given by the p-value.
#   - The GLRT has an asymptotic Chi-squared distribution with degrees of freedom equal to the number of parameters in the term droped.
#     This only applies if the model does not include smoothing terms.
#     In the presence of smoothing terms in the model, drop1() could be used as a rough guide to the significance of each of the parametric terms,
#     with the smoothing degrees of freedom fixed at their values chosen from the model prior to drop1().
# ------------------------------------------------------------------------------

m100 <- gamlss(y ~ x + qrt, data = aids, family = NBI)

m200 <- gamlss(y ~ pb(x) + qrt, data = aids, family = NBI)


# fix the smoothing degrees of freedom
m300 <- gamlss(y ~ pb(x, df = m200$mu.nl.df) + qrt, data = aids, family = NBI)



# ----------
# Generalized Likelihood Ratio Test (GLRT)
drop1(m100)


# -->
# The resulting test p-value for the parametric term qrt is 0.661, however this is unreliable because the linear dependence of log(mu)
# on x is inadequate.



# ----------
drop1(m300)


# -->
# This gives a rough guide to testing qrt.
# The corresponding test not fixing the smothing degrees of freedom is certainly may be unreliable.

