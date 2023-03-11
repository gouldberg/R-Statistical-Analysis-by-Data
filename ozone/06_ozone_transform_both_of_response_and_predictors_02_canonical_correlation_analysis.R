setwd("//media//kswada//MyFiles//R//ozone")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ozone
# ------------------------------------------------------------------------------

data(ozone, package="faraway")

str(ozone)



# ------------------------------------------------------------------------------
# Parametric form of Alternating Conditional Expection
#
#  - The canonical correlation method is an ancestor to ACE.
#    Given two sets of random variables X and Y, we find unit vectors a and b such that corr(aX, bY) is maximized.
#  - One generalization of canonical correlation is to allow some the X and Y to be power transforms of the original variables,
#    this results in a parametric form of ACE.
# ------------------------------------------------------------------------------

y <- cbind(ozone$O3, ozone$O3^2, sqrt(ozone$O3))

x <- ozone[,c("temp", "ibh", "ibt")]

cancor(x, y)



# -->
# We see that it is possible to obtain a correlation of 0.832 by taking particular linear combinations of O3, O3^2, and sqrt(O3) with 3 predictors.
# this meaans that R2 = 0.692 (= 0.832^2), this is not a particularly competitive fit.

# So ACE model's R2 = 0.7258 is somewhat overfitting.



