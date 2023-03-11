setwd("//media//kswada//MyFiles//R//dti")

packages <- c("dplyr", "fda", "refund")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Diffusion tensor imaging (DTI)
# ------------------------------------------------------------------------------
data("DTI", package = "refund")

str(DTI)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

Y <- DTI$case

X <- DTI$cca


argvals <- seq(0, 1, length = ncol(X))

Xdata <- data.frame(X = X)



# ----------
# pfr():  Penalized functional regression
# lf():  Construct an FLM regression term
#        define a term for inclusion in an mgcv::gam formula as constructed by pfr

dti_fit <- pfr(Y ~ lf(X, argvals = argvals), family = binomial(link = "probit"), data = Xdata)



# ----------
# plot the intercept beta(t)
plot(dti_fit, xlab = "time", ylab = "expression(paste(beta(t)))")



# -->
# We see that the parameter estimate is mostly negative, 
# meaning that the thinner CCA tracts are associated with MS.
# There is a pronounced dip towards the end suggesting that thickness right before the end of the parameter estimate seem to be positive (or are nearly so).
# One might be tempted to interpret this as saying that thicker ends of the CCA are associated with MS.
# However, this is incorrect as one must consider the estimates jointly across the tract.
# Namely, since the ends are positive and the middle is negative, this indicates that it is the relative difference between the thickness at the ends
# and the thickness in the middle which is associated with MS.

# This is an important point to keep in mind, when dealing with a functional prdictor that has both positive and negative estimates,
# it means that it is the contrast at different points which is important for the outcome.


