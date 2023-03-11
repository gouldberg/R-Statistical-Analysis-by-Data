setwd("//media//kswada//MyFiles//R//penicillin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  penicillin
# ------------------------------------------------------------------------------
data("penicillin", package = "faraway")

str(penicillin)

car::some(penicillin)



# ------------------------------------------------------------------------------
# Predicted random effects
#
#  - A purely Bayesian approach would specify the parameters of the priors (or specify priors for these) and compute a posterior distribution
# ------------------------------------------------------------------------------

# predicted random effects (alha(i))
ranef(mmod)$blend


# fixed effects
( cc <- model.tables(lmod) )



# ----------
# The predicted random effects are related to the fixed effects.
# Ratio to random effects
cc[[1]]$blend / ranef(mmod)$blend



# -->
# We see that the predicted random effects are exactly in proportion to the fixed effects.
# Typically predicted random effects are smaller and could be viewed as a type of shrinkage estimate.



# ------------------------------------------------------------------------------
# 95% confidence intervals for the random effects
# ------------------------------------------------------------------------------

library(lattice)

dotplot(ranef(mmod, condVar = TRUE))

