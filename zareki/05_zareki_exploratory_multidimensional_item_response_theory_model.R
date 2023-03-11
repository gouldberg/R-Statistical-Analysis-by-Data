setwd("//media//kswada//MyFiles//R//zareki")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  zareki
# ------------------------------------------------------------------------------

data("zareki", package = "MPsychoR")

str(zareki)



# using all binary ZAREKI addition and subtraction items
itzareki <- zareki[, 1:16]



# ------------------------------------------------------------------------------
# Compute an exploratory Princals in order to get an idea of the structure and dimensionality
# ------------------------------------------------------------------------------

library(Gifi)


przar <- princals(itzareki)

przar



# ----------
plot(przar)


plot(przar, "screeplot")



# -->
# The scree plot suggests that two dimensions should be sufficient.
# From the lodings plot, we see that the dimensions are not addition vs. subtraction, as we might have expected.



# ----------
plot(przar, "loadplot")




# ------------------------------------------------------------------------------
# Exploratory Multidimensional IRT
#   - Each item is free to load on each factor.
#     Typically formulated via the intercept-slope parameterization
#     logit(P(X(vi))) = alpha(i) * theta(v) + d(i)
#     alpha(i):  a vector of length p containing the slope/discrimination parameters of item i on each dimension
#     theta(v):  multidimensional person parameter, each person gets a parameter (factor score) on each dimension
#     d(i): Unfortunately cannot interpret d(i) , which is a scalar, as item location parameter.
#     But the following transformation does the trick:  beta(i) = - d(i) / sqrt(alpha(i) * t(alpha))
#     beta(i):  the multidimensional item location
#     The denominator reflects "the multidimensional item discrimination".
# ------------------------------------------------------------------------------

# Fit two models: a unidimensional 2-PL and a two-dimensional 2-PL

zar1d <- mirt(itzareki, 1, itemtype = "2PL")

zar2d <- mirt(itzareki, 2, itemtype = "2PL")



# ----------
# apply an LR-test
anova(zar1d, zar2d)



# -->
# suggesting that 2D-model fits significantly better than 1D-model.

